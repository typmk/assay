(ns assay.range
  "Range inference for a numeric form — matching SBCL's depth, then going
  one rung past it.

  SBCL's compiler does abstract interpretation over an interval lattice:
  given x : (integer 0 10), it derives (* x 2)+1 :: (OR 1 (INTEGER 3 21)),
  refines branches ((if (> x 5) x 0) :: (OR 0 (INTEGER 6 10))), and
  handles *, mod, abs, /. Clojure's compiler does none of this — its type
  surface is Object/long/double. So this is a small abstract interpreter:
  walk the form, propagate intervals, refine on comparisons.

  THE ASSAY MOVE, and the reason this is not just a reimplementation: SBCL
  INFERS the bound and stops — it is a proof you trust. This DERIVES the
  interval and then VERIFIES it by sweeping the input box and running the
  actual compiled form, exactly as `weigh` derives a rewrite and verifies
  it by recompiling. You get a bound AND a measurement that the real
  values fell inside it. A derived interval the sweep escapes is reported
  as :assay.range/unsound, not quietly trusted.

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

;; ── union domain: a value is a SET of disjoint intervals ──────────
;; SBCL derives (if (> x 5) x 0) :: (OR 0 (INTEGER 6 10)) — it keeps the two
;; branch results DISJOINT instead of convex-hulling them to [0 10]. This
;; does the same: the abstract value is a normalised vector of disjoint
;; intervals. `if` unions the branches; every other op distributes over the
;; set (cross-product then normalise). `interval` still returns the hull for
;; the common single-interval case and backward compatibility; the disjoint
;; set surfaces as derive's :assay.range/union.

(def ^:private top-set [top])
(defn- empty-iv? [[lo hi]] (> lo hi))

(defn- normalize
  "Sorted, overlap-merged, empty-dropped set of intervals."
  [ivs]
  (->> (remove empty-iv? ivs)
       (sort-by first)
       (reduce (fn [acc [lo hi]]
                 (if (and (seq acc) (<= lo (second (peek acc))))
                   (conj (pop acc) [(first (peek acc)) (mx hi (second (peek acc)))])
                   (conj acc [lo hi])))
               [])))

(defn- hull [ivs]
  (if (empty? ivs) top [(apply mn (map first ivs)) (apply mx (map second ivs))]))

(defn- lift1 [iop] (fn [s] (normalize (map iop s))))
(defn- lift2 [iop] (fn [s1 s2] (normalize (for [i1 s1 i2 s2] (iop i1 i2)))))

(defn- refine
  "Clamp the tested symbol's interval SET in each branch's env."
  [env test then?]
  (if-not (and (seq? test) (= 3 (count test)))
    env
    (let [[op a b] test
          [sym k] (cond (and (symbol? a) (number? b)) [a b]
                        (and (symbol? b) (number? a)) [b a]
                        :else [nil nil])
          op (if (and (symbol? b) (number? a))
               ({'< '> '> '< '<= '>= '>= '<=} op op) op)]
      (if (or (nil? sym) (not (contains? env sym)))
        env
        (let [op (if then? op ({'< '>= '> '<= '<= '> '>= '< '= '=} op op))
              clamp (fn [iv]
                      (case op
                        <  (clamp-hi iv (dec k))
                        <= (clamp-hi iv k)
                        >  (clamp-lo iv (inc k))
                        >= (clamp-lo iv k)
                        =  (if then? [k k] iv)
                        iv))]
          (assoc env sym (normalize (map clamp (env sym)))))))))

(defn- intervals
  "The disjoint interval SET of FORM. ENV maps symbol -> interval-set."
  [form env]
  (cond
    (number? form) (let [n (->long form)] [[n n]])
    (symbol? form) (get env form top-set)
    (not (seq? form)) top-set
    :else
    (let [[op & args] form
          iv #(intervals % env)]
      (case op
        + (reduce (lift2 iadd) [[0 0]] (map iv args))
        - (if (= 1 (count args)) ((lift2 isub) [[0 0]] (iv (first args)))
              (reduce (lift2 isub) (iv (first args)) (map iv (rest args))))
        * (reduce (lift2 imul) [[1 1]] (map iv args))
        inc ((lift1 #(iadd % [1 1])) (iv (first args)))
        dec ((lift1 #(isub % [1 1])) (iv (first args)))
        abs ((lift1 iabs) (iv (first args)))
        quot ((lift2 iquot) (iv (first args)) (iv (second args)))
        mod ((lift2 imod) (iv (first args)) (iv (second args)))
        min (reduce (lift2 (fn [[a b] [c d]] [(mn a c) (mn b d)])) (map iv args))
        max (reduce (lift2 (fn [[a b] [c d]] [(mx a c) (mx b d)])) (map iv args))
        if (let [[t then else] args]
             (normalize (concat (intervals then (refine env t true))
                                (intervals else (refine env t false)))))
        do (iv (last args))
        let* (let [[binds & body] args
                   env' (reduce (fn [e [s v]] (assoc e s (intervals v e)))
                                env (partition 2 binds))]
               (intervals (last body) env'))
        let  (let [[binds & body] args
                   env' (reduce (fn [e [s v]] (assoc e s (intervals v e)))
                                env (partition 2 binds))]
               (intervals (last body) env'))
        top-set))))

(defn- lift-env [env] (into {} (map (fn [[s iv]] [s [iv]])) env))

(defn interval
  "The interval (convex HULL) of FORM given ENV (symbol -> [lo hi]). For the
  disjoint union — SBCL's (OR ...) — see derive's :assay.range/union."
  [form env]
  (hull (intervals form (lift-env env))))

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
    ;; => {:assay.range/derived [1 21] :assay.range/observed [1 21]
    ;;     :assay.range/sound true}

  When branches keep the result DISJOINT, :assay.range/union carries the
  SBCL-style (OR ...) set that the single :derived hull flattens:

    (derive '(if (> x 5) x 0) '{x [0 10]})
    ;; => {:assay.range/derived [0 10] :assay.range/union [[0 0] [6 10]] ...}"
  [form env]
  (let [form (walk/macroexpand-all form)
        rs (intervals form (lift-env env))
        [dlo dhi :as derived] (hull rs)
        f (eval (list 'fn (vec (keys env)) form))
        pts (sample-points env)
        outs (keep (fn [pt] (try (double (apply f (map pt (keys env))))
                                 (catch Throwable _ nil)))
                   pts)
        [olo ohi] (if (seq outs) [(apply min outs) (apply max outs)] [nil nil])]
    (cond-> #:assay.range{:derived (mapv ->long derived)
                         :top? (top? derived)}
      (> (count rs) 1)
      (assoc :assay.range/union (mapv (fn [[lo hi]] [(->long lo) (->long hi)]) rs))
      (seq outs)
      (assoc :assay.range/observed [(->long olo) (->long ohi)]
             :assay.range/sound (and (<= dlo olo) (<= ohi dhi))
             ;; tighter: does each observed point fall in SOME union interval?
             :assay.range/union-sound
             (every? (fn [o] (some (fn [[lo hi]] (<= lo o hi)) rs)) outs)
             :assay.range/samples (count outs)))))
