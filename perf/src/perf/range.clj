(ns perf.range
  "Range inference for a numeric form — matching SBCL's depth, then going
  one rung past it.

  SBCL's compiler does abstract interpretation over an interval lattice:
  given x : (integer 0 10), it derives (* x 2)+1 :: (OR 1 (INTEGER 3 21)),
  refines branches ((if (> x 5) x 0) :: (OR 0 (INTEGER 6 10))), and
  handles *, mod, abs, /. Clojure's compiler does none of this — its type
  surface is Object/long/double. So this is a small abstract interpreter:
  walk the form, propagate intervals, refine on comparisons.

  THE PERF MOVE, and the reason this is not just a reimplementation: SBCL
  INFERS the bound and stops — it is a proof you trust. This DERIVES the
  interval and then VERIFIES it by sweeping the input box and running the
  actual compiled form, exactly as `weigh` derives a rewrite and verifies
  it by recompiling. You get a bound AND a measurement that the real
  values fell inside it. A derived interval the sweep escapes is reported
  as :perf.range/unsound, not quietly trusted.

  SCOPE, stated honestly: integer and double interval arithmetic through
  + - * quot mod abs inc dec min max and if/let, with comparison
  refinement. Loops widen to unbounded, as SBCL's did on the same input.
  Unknown forms return TOP. This covers the straight-line arithmetic that
  is most of what SBCL's range analysis buys; it is not SBCL's full
  fixpoint engine."
  (:require [clojure.walk :as walk]))

;; An interval is [lo hi] with ##-Inf/##Inf bounds. TOP is everything.
(def ^:private neg-inf ##-Inf)
(def ^:private pos-inf ##Inf)
(def top [neg-inf pos-inf])
(defn- top? [[lo hi]] (and (= lo neg-inf) (= hi pos-inf)))

(defn- ->long [x] (cond (= x neg-inf) neg-inf (= x pos-inf) pos-inf
                        (and (number? x) (== x (Math/floor x))) (long x) :else x))

(defn- mn [& xs] (reduce (fn [a b] (if (<= a b) a b)) xs))
(defn- mx [& xs] (reduce (fn [a b] (if (>= a b) a b)) xs))

(defn- iadd [[a b] [c d]] [(+ a c) (+ b d)])
(defn- isub [[a b] [c d]] [(- a d) (- b c)])
(defn- imul [[a b] [c d]]
  (let [ps [(* a c) (* a d) (* b c) (* b d)]]
    ;; NaN guard: 0 * Inf. Treat a zero factor as contributing 0.
    (let [ps (map (fn [p] (if (Double/isNaN (double p)) 0 p)) ps)]
      [(apply mn ps) (apply mx ps)])))
(defn- iabs [[a b]]
  (cond (>= a 0) [a b]
        (<= b 0) [(- b) (- a)]
        :else    [0 (mx (- a) b)]))
(defn- iquot [[a b] [c d]]
  (if (or (<= c 0 d))                    ; divisor interval spans 0
    top
    (let [ps (for [x [a b] y [c d]] (if (zero? y) 0 (quot (double x) y)))]
      [(long (Math/floor (double (apply mn ps)))) (long (Math/ceil (double (apply mx ps))))])))
(defn- imod [_ [c d]]
  ;; (mod x m): for a positive modulus interval, result in [0, maxm-1].
  (if (> c 0) [0 (dec d)] top))

;; ── comparison refinement ─────────────────────────────────────────
;; For (if (< x k) then else), the THEN branch knows x < k. Refine the
;; symbol's interval in each branch's env. Only handles (op SYM CONST) and
;; (op CONST SYM), the common shapes.

(defn- clamp-hi [[lo hi] h] [lo (mn hi h)])
(defn- clamp-lo [[lo hi] l] [(mx lo l) hi])

(defn- refine [env test then?]
  (if-not (and (seq? test) (= 3 (count test)))
    env
    (let [[op a b] test
          [sym k] (cond (and (symbol? a) (number? b)) [a b]
                        (and (symbol? b) (number? a)) [b a]
                        :else [nil nil])
          ;; normalise so the symbol is on the left; flip op if it was right
          op (if (and (symbol? b) (number? a))
               ({'< '> '> '< '<= '>= '>= '<=} op op) op)]
      (if (or (nil? sym) (not (contains? env sym)))
        env
        (let [iv (env sym)
              op (if then? op ({'< '>= '> '<= '<= '> '>= '< '= '=} op op))]
          (assoc env sym
                 (case op
                   <  (clamp-hi iv (dec k))
                   <= (clamp-hi iv k)
                   >  (clamp-lo iv (inc k))
                   >= (clamp-lo iv k)
                   =  (if then? [k k] iv)
                   iv)))))))

(defn- union [[a b] [c d]] [(mn a c) (mx b d)])

(defn interval
  "Derive the interval of FORM given ENV, a map of symbol -> [lo hi]."
  [form env]
  (cond
    (number? form) (let [n (->long form)] [n n])
    (symbol? form) (get env form top)
    (not (seq? form)) top
    :else
    (let [[op & args] form
          iv #(interval % env)]
      (case op
        + (reduce iadd [0 0] (map iv args))
        - (if (= 1 (count args)) (isub [0 0] (iv (first args)))
              (reduce isub (iv (first args)) (map iv (rest args))))
        * (reduce imul [1 1] (map iv args))
        inc (iadd (iv (first args)) [1 1])
        dec (isub (iv (first args)) [1 1])
        abs (iabs (iv (first args)))
        quot (iquot (iv (first args)) (iv (second args)))
        mod (imod (iv (first args)) (iv (second args)))
        min (reduce (fn [[a b] [c d]] [(mn a c) (mn b d)]) (map iv args))
        max (reduce (fn [[a b] [c d]] [(mx a c) (mx b d)]) (map iv args))
        if (let [[t then else] args]
             (union (interval then (refine env t true))
                    (interval else (refine env t false))))
        do (iv (last args))
        let* (let [[binds & body] args
                   env' (reduce (fn [e [s v]] (assoc e s (interval v e)))
                                env (partition 2 binds))]
               (interval (last body) env'))
        let  (let [[binds & body] args
                   env' (reduce (fn [e [s v]] (assoc e s (interval v e)))
                                env (partition 2 binds))]
               (interval (last body) env'))
        ;; loops and everything else: widen to TOP (SBCL also gave up the
        ;; exact bound on a dotimes-sum, returning merely non-negative).
        top))))

;; ── verify by sweep ───────────────────────────────────────────────

(defn- combo [colls]
  (if (empty? colls) [[]]
      (for [x (first colls) more (combo (rest colls))] (cons x more))))

(defn- sample-points [env]
  ;; corners of the input box + a few interior integers, capped.
  (let [syms (vec (keys env))
        vals (mapv (fn [s] (let [[lo hi] (env s)]
                             (if (or (= lo neg-inf) (= hi pos-inf))
                               [(if (= lo neg-inf) -1000 lo) 0 (if (= hi pos-inf) 1000 hi)]
                               (distinct [lo (long (/ (+ lo hi) 2)) hi]))))
                   syms)]
    (map #(zipmap syms %) (combo vals))))

(defn derive
  "Derive-and-verify the range of FORM over the input box ENV
  (symbol -> [lo hi]). Returns the derived interval, the observed
  interval from sweeping the box, and whether the observation stayed
  inside the derivation.

    (derive '(+ (* x 2) 1) '{x [0 10]})
    ;; => {:perf.range/derived [1 21] :perf.range/observed [1 21]
    ;;     :perf.range/sound true}"
  [form env]
  (let [form (walk/macroexpand-all form)
        [dlo dhi :as derived] (interval form env)
        f (eval (list 'fn (vec (keys env)) form))
        pts (sample-points env)
        outs (keep (fn [pt] (try (double (apply f (map pt (keys env))))
                                 (catch Throwable _ nil)))
                   pts)
        [olo ohi] (if (seq outs) [(apply min outs) (apply max outs)] [nil nil])]
    (cond-> #:perf.range{:derived (mapv ->long derived)
                         :top? (top? derived)}
      (seq outs)
      (assoc :perf.range/observed [(->long olo) (->long ohi)]
             :perf.range/sound (and (<= dlo olo) (<= ohi dhi))
             :perf.range/samples (count outs)))))
