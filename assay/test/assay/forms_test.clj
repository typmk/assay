(ns assay.forms-test
  "Non-timing invariants of the (Δoutcome × Δcost) classifier. Timing labels
  are machine-dependent; the OUTCOME oracle is not, so the assertions here
  are on equivalence detection — the exact axis the red-team found unsound
  in the fixed-probe demo."
  (:require [clojure.test :refer [deftest is testing]]
            [assay.forms :as forms]))

(deftest prove-rank1-polynomial-equivalence
  (testing "algebraic identities are PROVEN, not sampled"
    (is (= :proven-equal (forms/prove '(fn [a] (+ a a)) '(fn [a] (* 2 a)))))
    (is (= :proven-equal (forms/prove '(fn [a b] (* a b)) '(fn [a b] (* b a)))))
    (is (= :proven-equal (forms/prove '(fn [a b c] (* a (+ b c)))
                                      '(fn [a b c] (+ (* a b) (* a c))))))
    (is (= :proven-different (forms/prove '(fn [a] (* a a)) '(fn [a] (* 2 a))))))
  (testing "genuine control flow / non-polynomial declines to nil (falls back to sampling)"
    ;; a REDUNDANT if (both branches equal) is now PROVEN, not declined —
    ;; see prove-breadth-if-and-let. These are genuinely non-polynomial:
    (is (nil? (forms/prove '(fn [x] (if (> x 5) x 0)) '(fn [x] x))))
    (is (nil? (forms/prove '(fn [a b] (reduce + (map * a b))) '(fn [a b] 0)))))
  (testing "equivalent? reports rank :proven and needs no samples for the polynomial case"
    (let [eq (forms/equivalent? '(fn [a] (+ a a)) '(fn [a] (* 2 a)) [3])]
      (is (true? (:assay.forms/equivalent eq)))
      (is (= :assay.rank/proven (:assay.forms/rank eq))))))

(deftest randomized-pool-catches-differing-outcome
  (testing "a differing pair colliding at x=0,2 is caught — here PROVEN (both polynomials)"
    ;; (+ x x) and x*x collide at x=0 and x=2; a fixed probe set would
    ;; mislabel them synonyms. Canonicalisation proves them different (rank 1)
    ;; without needing the pool at all — strictly better than sampling.
    (let [eq (forms/equivalent? '(fn [x] (* x x)) '(fn [x] (+ x x)) [2.0] 40)]
      (is (false? (:assay.forms/equivalent eq)))
      (is (contains? #{:assay.rank/proven :assay.rank/observed} (:assay.forms/rank eq)))))
  (testing "a genuine equivalence survives the whole pool"
    (let [eq (forms/equivalent? '(fn [a b] (reduce + (map * a b)))
                                '(fn [a b] (+ (* (nth a 0) (nth b 0))
                                              (* (nth a 1) (nth b 1))
                                              (* (nth a 2) (nth b 2))))
                                [[1.0 2.0 3.0] [4.0 5.0 6.0]] 40)]
      (is (true? (:assay.forms/equivalent eq))))))

(deftest classify-labels-from-measured-coordinates
  (testing "differing outcome is labelled differing regardless of cost"
    (let [r (forms/classify '(fn [a b] (reduce + (map * a b)))
                            '(fn [a b] (reduce + (map + a b)))   ; + not *
                            [[1.0 2.0 3.0] [4.0 5.0 6.0]]
                            {:reps 100000 :trials 3})]
      (is (= :assay.forms/differing-outcome (:assay.forms/kind r)))
      (is (false? (:assay.forms/equivalent r)))))
  (testing "a true equivalent is labelled a synonym (cheaper or plain), never differing"
    (let [r (forms/classify '(fn [a b] (reduce + (map * a b)))
                            '(fn [a b] (apply + (map * a b)))
                            [[1.0 2.0 3.0] [4.0 5.0 6.0]]
                            {:reps 100000 :trials 3})]
      (is (true? (:assay.forms/equivalent r)))
      (is (contains? #{:assay.forms/synonym :assay.forms/cheaper-synonym}
                     (:assay.forms/kind r))))))

(deftest discover-generates-from-the-fragment
  (testing "discover derives candidates from the form's own ops — no supplied list"
    (let [rows (forms/discover '(fn [a b] (+ (* a b) a)) [3.0 5.0] {:reps 100000 :trials 3})]
      (is (seq rows) "generated at least one candidate")
      ;; the inline expansion of + is a true EQUIVALENT synonym the generator
      ;; found. It is cost-marginal (semantically identical to +), so timing
      ;; noise flips it between :synonym and :cheaper-synonym — both are the
      ;; equivalent-synonym outcome this test is about; don't pin the bucket.
      (is (some #(and (:assay.forms/equivalent %)
                      (contains? #{:assay.forms/synonym :assay.forms/cheaper-synonym}
                                 (:assay.forms/kind %))) rows))
      ;; op-mutations are OPT-IN, so the default must NOT spend on them
      (is (not-any? #(= :assay.forms/differing-outcome (:assay.forms/kind %)) rows)
          "default discover yields equivalent rewrites only")
      ;; each classification was either PROVEN (rank 1, no samples needed) or
      ;; saw the full adversarial edge pool
      (is (every? #(or (= :assay.rank/proven (:assay.forms/rank %))
                       (>= (:assay.forms/samples %) 49)) rows))))

  (testing "{:mutate? true} restores the differing-outcome sensitivity map"
    (let [rows (forms/discover '(fn [a b] (+ (* a b) a)) [3.0 5.0]
                               {:reps 100000 :trials 3 :mutate? true})]
      ;; op-swaps change the result — caught as differing over the edge+random pool
      (is (some #(= :assay.forms/differing-outcome (:assay.forms/kind %)) rows))
      ;; and the equivalent rewrites are still there alongside them
      (is (some :assay.forms/equivalent rows)))))

(deftest interval-difference-is-caught
  (testing "a form differing across a whole MID-RANGE interval is not mislabelled equivalent"
    ;; The red-team's break: (fn [x] x) vs a guard that zeroes (100,1000).
    ;; The old clamped-near-args pool never probed that interval and called
    ;; it a cheaper-synonym. The magnitude sweep must hit it.
    (let [eq (forms/equivalent? '(fn [x] (if (and (> x 100.0) (< x 1000.0)) 0.0 x))
                                '(fn [x] x) [2.0] 40)]
      (is (false? (:assay.forms/equivalent eq))
          "a difference on (100,1000) must be found by the magnitude sweep"))))

(deftest discover-reaches-transducer-fusion
  (testing "the generator finds a fused transducer for a reduce-over-seq-pipeline"
    (let [rows (forms/discover '(fn [xs] (->> xs (map inc) (filter even?) (reduce +)))
                               [(vec (range 300))] {:reps 15000 :trials 3})
          fused (filter #(some #{'transduce} (flatten (:assay.forms/candidate %))) rows)]
      (is (seq fused) "a transduce candidate was generated")
      (is (some #(and (:assay.forms/equivalent %)
                      (contains? #{:assay.forms/synonym :assay.forms/cheaper-synonym}
                                 (:assay.forms/kind %)))
                fused)
          "the transduce fusion is recognised as an equivalent synonym, not differing"))))

(deftest both-throw-is-agreement
  (testing "an input where BOTH forms throw is not counted as a difference"
    ;; (even? 1.5) throws; a valid rewrite of an even?-using form must not be
    ;; called differing just because the edge pool fed it a non-integer.
    (let [eq (forms/equivalent? '(fn [xs] (filter even? xs))
                                '(fn [xs] (remove odd? xs))
                                [[1 2 3 4]] 20)]
      (is (true? (:assay.forms/equivalent eq))))))

(deftest edge-pool-catches-corner-only-difference
  (testing "a candidate that differs only at zero is caught by the edge pool"
    ;; (* a a) vs (if (zero? a) 1.0 (* a a)) agree everywhere EXCEPT a=0.0,
    ;; which the random pool of positive-ish perturbations would miss.
    (let [eq (forms/equivalent? '(fn [a] (* a a))
                                '(fn [a] (if (zero? a) 1.0 (* a a)))
                                [3.0] 40)]
      (is (false? (:assay.forms/equivalent eq))
          "the 0.0 edge input must expose the difference"))))

(deftest synonyms-are-compiler-sanctioned
  (testing "macroexpand and inline give mechanical synonyms with no authored rule"
    (is (some #{'(if x (do y))} (forms/synonyms '(when x y))))
    ;; (+ a b) inlines to a Numbers static call — the compiler's own rewrite
    (is (seq (forms/synonyms '(+ a b))))))

(deftest prove-breadth-if-and-let
  (testing "if with equal branches reduces to the shared polynomial (rank 1)"
    (is (= :proven-equal (forms/prove '(fn [a c] (if c (+ a a) (* 2 a))) '(fn [a c] (* 2 a))))))
  (testing "let-inlining sees through naming"
    (is (= :proven-equal (forms/prove '(fn [x] (let [y (* x 2)] (+ y 1))) '(fn [x] (inc (* 2 x))))))
    (is (= :proven-equal (forms/prove '(fn [x] (let [y (* x 2) z (+ y 1)] (* z z)))
                                      '(fn [x] (let [w (inc (* 2 x))] (* w w)))))))
  (testing "a genuine conditional still declines to nil (falls back to sampling)"
    (is (nil? (forms/prove '(fn [x] (if (> x 5) x 0)) '(fn [x] x))))))
