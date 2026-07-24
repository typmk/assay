(ns perf.forms
  "Alternative writings of a fragment, classified by MEASUREMENT — never by a
  remembered rule.

  Each candidate writing is placed on the (Δoutcome × Δcost) plane by
  running it, and the label is read off the two measured coordinates:

    Δoutcome = 0, cheaper   -> a synonym worth adopting
    Δoutcome = 0, same cost -> a synonym (fluency / style)
    Δoutcome != 0           -> a differing structure (sensitivity, or an
                               approximation if the difference is bounded)

  Both oracles are the hardened ones the red-team's teardown forced:

    * OUTCOME equivalence is checked over a RANDOMISED, perturbed pool of
      inputs, not a fixed probe set. Six fixed probes let a differing form
      collide at every point (e.g. (+ x x) = x*x exactly at x=2) and be
      mislabelled a synonym; a perturbed pool makes accidental agreement
      across all of them vanishingly unlikely. Still observational — rank 2,
      N-sample — never proven.

    * COST is `perf.measure/weigh`, which rotates args through a jittered
      pool (no constant-fold) and sinks results (no dead-code elimination),
      reporting time AND bytes. A constant arg once made a whole benchmark
      measure 0ns; that failure is designed out here.

  Every record carries the rank and sample count of the evidence that
  produced it.

  THE MODEL — two oracles and three views, nothing else:

    OUTCOME oracle  `equivalent?`  do two forms compute the same thing?
                    prove first (rank 1, algebraic, polynomial fragment),
                    else sample-equiv (rank 2, swept + edge pool).
    COST oracle     `perf.measure/weigh`  which is cheaper, by how much,
                    time and bytes (rank 1, measured, fold- and DCE-proof).

    classify      = one candidate on the (outcome × cost) plane.
    alternatives  = classify a supplied set, ranked.
    discover      = GENERATE candidates from a fragment's own structure
                    (synonyms, op-mutations, transducer fusion) then classify.

  Add an outcome rule to `prove`, a generator to `discover`, or a cost
  dimension to `weigh` — everything else composes those three."
  (:require [perf.measure :as measure]))

(defn- spread-val
  "A value that SWEEPS the input space by MAGNITUDE, not a perturbation near
  the given arg. The red-team fooled the old clamped perturbation with a
  form differing only on (100,1000): samples stayed near the args (~2) and
  the edge pool only reached ±1e6, leaving the whole mid-range unprobed.
  This sweeps 1e-3..1e5 across index K, both signs, so a difference anywhere
  in that band is hit. Still observational — a difference OUTSIDE the swept
  band (or at a measure-zero point) is not proof against; equivalence stays
  rank 2, closed only by `perf.range` on the arithmetic subset."
  [x k n]
  (cond
    (number? x)
    (let [frac (/ (double k) (double (max 1 n)))
          mag  (Math/pow 10.0 (- (* 8.0 frac) 3.0))     ; 1e-3 .. 1e5
          sign (if (zero? (mod k 3)) -1.0 1.0)
          v    (* sign mag)]
      (if (integer? x) (long v) v))
    (and (sequential? x) (every? number? x))
    ;; per-element index folded back into [0,n) so magnitude stays bounded —
    ;; adding a raw vector index blew frac past 1 and overflowed long.
    (mapv (fn [i y] (spread-val y (mod (+ k i) n) n)) (range) x)
    :else x))

(defn- arg-pool [args n]
  (mapv (fn [k] (mapv #(spread-val % k n) args)) (range n)))

(defn- fill [x v]
  (cond (number? x) v
        (and (sequential? x) (every? number? x)) (mapv (constantly v) x)
        :else x))

(defn- edge-pool
  "Adversarial corner inputs the random pool would rarely hit: a form that
  differs only at zero, at a sign flip, or at extreme magnitude (the
  red-team's escape route) is caught here. Each edge value is broadcast into
  the argument shape."
  [args]
  (mapv (fn [v] (mapv #(fill % v) args))
        [0.0 -0.0 1.0 -1.0 1.0e6 -1.0e6 1.0e-6 -3.14159 2.0]))

;; ── rank-1 equivalence: polynomial canonicalisation ──────────────
;; Sampling can never PROVE equivalence (the red-team fooled it twice). For
;; the polynomial fragment — + - * inc dec over number literals and symbols
;; — it can be proven: reduce both forms to a canonical multivariate
;; polynomial (a map {monomial -> coeff}, monomial a map {var -> power}).
;; Equal normal forms is an algebraic identity, not an observation, so
;; (+ a a) ≡ (* 2 a) and (* a b) ≡ (* b a) come back PROVEN, rank 1 — the
;; thing SBCL's type system proves and perf could previously only sample.
;; Anything with control flow, division, or an unknown op reduces to nil and
;; the caller falls back to observational equivalence, honestly rank 2.

(declare canon sample-equiv)

(defn- fn-body
  "The (single) body expr of a fn-form; a bare expression is returned
  unchanged. Anatomy parsing delegated to the one home, measure/fn-parts."
  [f]
  (if (and (seq? f) (= 'fn (first f)))
    (first (:body (measure/fn-parts f)))
    f))

(defn- poly-add [& ps] (apply merge-with + ps))
(defn- poly-neg [p] (into {} (map (fn [[m c]] [m (- c)])) p))
(defn- poly-scale [p k] (into {} (map (fn [[m c]] [m (* c k)])) p))
(defn- mono-mul [m1 m2] (merge-with + m1 m2))
(defn- poly-mul [p1 p2]
  (reduce (fn [acc [m1 c1]]
            (reduce (fn [acc [m2 c2]]
                      (update acc (mono-mul m1 m2) (fnil + 0) (* c1 c2)))
                    acc p2))
          {} p1))

(defn- constant-poly
  "The constant value of a polynomial that is purely a constant, else nil."
  [p]
  (let [nz (remove (fn [[_ c]] (zero? c)) p)]
    (cond (empty? nz) 0
          (and (= 1 (count nz)) (empty? (ffirst nz))) (second (first nz)))))

(defn- canon
  "FORM as a canonical polynomial {monomial -> coeff}, or nil if it leaves
  the polynomial fragment. Division by a CONSTANT is in-fragment (scale by
  the reciprocal, coeffs become ratios); division by anything else is not."
  [form]
  (cond
    (number? form) {{} form}
    (symbol? form) {{form 1} 1}
    (seq? form)
    (let [[op & args] form
          ps (map canon args)]
      (when (every? some? ps)
        (case op
          + (apply poly-add {} ps)
          - (cond (empty? ps) nil
                  (= 1 (count ps)) (poly-neg (first ps))
                  :else (apply poly-add (first ps) (map poly-neg (rest ps))))
          * (reduce poly-mul {{} 1} ps)
          / (when (seq ps)
              ;; (/ num d1 d2 ...) — divide only by constant divisors
              (reduce (fn [acc d]
                        (when-let [c (and acc (constant-poly d))]
                          (when-not (zero? c) (poly-scale acc (/ 1 c)))))
                      (first ps) (rest ps)))
          inc (poly-add (first ps) {{} 1})
          dec (poly-add (first ps) {{} -1})
          nil)))
    :else nil))

(defn- poly=?
  "Two polynomials equal? Coefficients compared with == so 2 and 2.0 agree,
  after dropping zero terms."
  [pa pb]
  (let [clean (fn [p] (into {} (remove (fn [[_ c]] (zero? c))) p))
        a (clean pa) b (clean pb)]
    (and (= (set (keys a)) (set (keys b)))
         (every? (fn [m] (== (a m) (b m))) (keys a)))))

(defn prove
  "Try to PROVE the relationship of two forms algebraically, over the
  polynomial fragment. :proven-equal / :proven-different are rank 1; nil
  means outside the fragment — fall back to sampling.

  SCOPE, stated precisely: this proves equality over the REAL/rational ring.
  For integer/long arguments that is bit-exact — (+ a a) and (* 2 a) compute
  identical longs. For DOUBLE arguments it is exact for the commutations it
  relies on (+ and * commute bit-exactly in IEEE) but NOT for the
  reassociation and distribution canon also performs: (+ (+ a b) c) and
  (+ a (+ b c)) share a normal form yet can differ in the last ULP, because
  float addition is not associative. So :proven-equal on float inputs means
  real-arithmetic-equal, not bit-identical — the same line SBCL draws when
  it refuses to reassociate floats. For bit-exactness on floats, the
  observational check still applies."
  [form-a form-b]
  (let [pa (canon (fn-body form-a)) pb (canon (fn-body form-b))]
    (cond (or (nil? pa) (nil? pb)) nil
          (poly=? pa pb) :proven-equal
          :else :proven-different)))

(defn equivalent?
  "Do FORM and CANDIDATE compute the same thing? For the polynomial fragment
  this is PROVEN (rank 1) by canonicalisation; otherwise it is observed
  (rank 2) over ARGS and a perturbed pool of N inputs. The record's :rank
  and :basis say which — the answer never masquerades as proof."
  ([form candidate args] (equivalent? form candidate args 40))
  ([form candidate args n]
   (if-let [p (prove form candidate)]
     #:perf.forms{:equivalent (= p :proven-equal)
                  :rank :perf.rank/proven
                  :basis :perf.forms/algebraic
                  :samples 0}
     (sample-equiv form candidate args n))))

(defn- sample-equiv
  "The observational fallback — sampling over the perturbed + edge pool."
  ([form candidate args] (sample-equiv form candidate args 40))
  ([form candidate args n]
   (let [f (eval form) g (eval candidate)
         pool (concat [(vec args)] (arg-pool args n) (edge-pool args))
         ;; agree WHERE BOTH ARE DEFINED. If both throw on an input (e.g. an
         ;; edge value like 3.14159 fed to a form using even?), that input is
         ;; outside both domains — not a disagreement. Only one-throws-other-
         ;; doesn't, or two differing values, counts as differing. Without
         ;; this, a valid rewrite of an even?-using form reads as differing
         ;; purely because the edge pool broke both.
         ;; FORCE the result inside the try: filter/map return a lazy seq
         ;; whose element exception (even? on a non-integer edge input) fires
         ;; only on realisation — which would otherwise escape the catch and
         ;; crash the comparison instead of being counted as :threw.
         run (fn [h t] (try (let [r (apply h t)] {:v (if (seq? r) (doall r) r)})
                            (catch Throwable _ :threw)))
         agree? (fn [t] (let [a (run f t) b (run g t)]
                          (cond (and (= :threw a) (= :threw b)) true
                                (or (= :threw a) (= :threw b))   false
                                :else (measure/same? (:v a) (:v b)))))]
     #:perf.forms{:equivalent (every? agree? pool)
                  :samples (count pool)
                  :rank :perf.rank/observed})))

(defn classify
  "Place CANDIDATE relative to FORM on the (Δoutcome × Δcost) plane, using
  the sound outcome and cost oracles. Returns a record with the measured
  coordinates and the derived label — nothing authored."
  ([form candidate args] (classify form candidate args {}))
  ([form candidate args {:keys [reps trials samples]
                         :or {reps 500000 trials 5 samples 40}}]
   (let [eq (equivalent? form candidate args samples)
         equiv? (:perf.forms/equivalent eq)
         ;; only price an EQUIVALENT candidate — cost is moot for a differing
         ;; one, and a differing candidate may throw on the sample args
         ;; (e.g. an op-swap to (reduce * ...) overflows), which must not
         ;; crash the classifier. Guard anyway.
         w  (when equiv?
              (try (measure/weigh form args {:against candidate :reps reps :trials trials})
                   (catch Throwable _ nil)))
         factor (:perf.weigh/factor w)
         bytes  (:perf.weigh/bytes-saved w)
         cheaper? (or (and factor (> factor 1.05)) (and bytes (pos? bytes)))]
     #:perf.forms{:candidate candidate
                  :equivalent equiv?
                  :samples (:perf.forms/samples eq)
                  ;; propagate the rank the oracle actually used — :proven
                  ;; when canonicalisation decided, :observed when it sampled.
                  ;; Hardcoding :observed here silently downgraded a rank-1
                  ;; proof to look like a sample.
                  :rank (:perf.forms/rank eq)
                  :factor factor
                  :bytes-saved bytes
                  :kind (cond (not equiv?) :perf.forms/differing-outcome
                              cheaper?      :perf.forms/cheaper-synonym
                              :else         :perf.forms/synonym)})))

(defn alternatives
  "Classify a set of CANDIDATE writings of FORM, ranked cheapest-first among
  the true synonyms, with differing-outcome structures listed after. The
  caller supplies the candidates (a discovery generator seeds them
  elsewhere); this is the measured judge that ranks them honestly."
  ([form candidates args] (alternatives form candidates args {}))
  ([form candidates args opts]
   (let [rows (mapv #(classify form % args opts) candidates)]
     (vec (sort-by (fn [r] [(if (:perf.forms/equivalent r) 0 1)
                            (- (or (:perf.forms/factor r) 0))])
                   rows)))))

(defn synonyms
  "Mechanical, compiler-sanctioned synonyms of a plain CALL form, needing no
  authored rule: its macroexpand-1 (every macro is a directed equivalence)
  and, when the head carries :inline metadata, the inlined form the compiler
  itself would emit. Sound by construction — these are what the compiler
  already does. Returns the distinct alternative forms."
  [form]
  (when (seq? form)
    (let [head (first form)
          expanded (macroexpand-1 form)
          v (when (symbol? head) (resolve head))
          inl (:inline (meta v))
          inlined (when (and inl (not (:macro (meta v))))
                    (try (apply inl (rest form)) (catch Throwable _ nil)))]
      (vec (distinct (remove #(or (nil? %) (= % form))
                             [(when (not= expanded form) expanded) inlined]))))))

;; ── the generator: candidates from the fragment's OWN structure ───

(def ^:private swap-ops '[+ - * / quot rem min max])

(defn- fn-rebody [form new-body]
  (let [{:keys [head name params]} (measure/fn-parts form)]
    (if name
      (list head name params new-body)
      (list head params new-body))))

(defn- op-mutations
  "Structural variants of BODY: replace every occurrence of one arithmetic op
  with another. Coarse (whole-op, not per-position) but derived purely from
  the ops the fragment actually contains — no authored rewrite. Most are
  differing-outcome; that IS the sensitivity map (what is load-bearing)."
  [body]
  (let [present (set (filter (set swap-ops) (tree-seq seqable? seq body)))]
    (distinct
     (for [from present to swap-ops :when (not= from to)
           :let [m (clojure.walk/postwalk (fn [x] (if (= x from) to x)) body)]
           :when (not= m body)]
       m))))

;; ── representation-level generator: transducer fusion ─────────────
;; The op-mutation rung cannot reach a rewrite that CHANGES THE SHAPE — seq
;; pipeline to transducer. This one can, and it is not authored either: the
;; fusable set is exactly the fns that expose a transducer arity (checked by
;; execution, the same discovery the whole `forms` idea rests on), and the
;; rewrite is the mechanical fusion the transducer contract already licenses.

(def ^:private fusable
  "Core seq fns confirmed by EXECUTION to run as single-arg transducers —
  the discovery from arglists+trial, frozen. Not an authored optimisation
  list; the SET is what the runtime answered."
  '#{map filter remove keep take-while drop-while map-indexed})

(defn- unwrap-chain
  "Peel a nest of fusable single-coll seq ops off COLL-EXPR, returning
  [[xform ...] coll] with xforms INNERMOST-FIRST (transducer-comp order)."
  [coll-expr]
  (loop [x coll-expr xs ()]
    (if (and (seq? x) (fusable (first x)) (= 3 (count x)))
      (recur (nth x 2) (cons (list (first x) (nth x 1)) xs))   ; cons = reverse-as-we-go
      [(vec xs) x])))

(defn- transducer-fusion
  "If BODY is a reduce over a chain of fusable seq ops, the equivalent
  transducer form that fuses away the intermediate seqs. nil otherwise."
  [body]
  (let [body (clojure.walk/macroexpand-all body)]
    (when (and (seq? body) (= 'reduce (first body)))
      (let [a (rest body)
            [rf init coll-expr] (if (= 3 (count a))
                                  [(first a) (second a) (nth a 2)]
                                  [(first a) nil (second a)])
            [xforms coll] (unwrap-chain coll-expr)]
        (when (seq xforms)
          (let [xf (if (= 1 (count xforms)) (first xforms) (cons 'comp xforms))]
            (if init (list 'transduce xf rf init coll)
                (list 'transduce xf rf coll))))))))

(defn discover
  "GENERATE candidate writings of FORM from its own structure — mechanical
  synonyms (macroexpand/inline), op-mutations of its body, AND transducer
  fusion of a fusable seq pipeline — then classify each with the sound
  oracles and rank. The caller supplies NO candidates; they are derived from
  the fragment. Op-mutation yields the differing-outcome sensitivity map;
  fusion + inline yield equivalent rewrites that may be cheaper."
  ([form args] (discover form args {}))
  ([form args opts]
   (let [body  (fn-body form)
         mech  (map #(fn-rebody form %) (synonyms body))
         muts  (map #(fn-rebody form %) (op-mutations body))
         fused (when-let [t (transducer-fusion body)] [(fn-rebody form t)])
         cands (vec (distinct (remove #(= % form) (concat fused mech muts))))]
     (alternatives form cands args opts))))
