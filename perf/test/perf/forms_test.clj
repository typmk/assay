(ns perf.forms-test
  "Non-timing invariants of the (Δoutcome × Δcost) classifier. Timing labels
  are machine-dependent; the OUTCOME oracle is not, so the assertions here
  are on equivalence detection — the exact axis the red-team found unsound
  in the fixed-probe demo."
  (:require [clojure.test :refer [deftest is testing]]
            [perf.forms :as forms]))

(deftest prove-rank1-polynomial-equivalence
  (testing "algebraic identities are PROVEN, not sampled"
    (is (= :proven-equal (forms/prove '(fn [a] (+ a a)) '(fn [a] (* 2 a)))))
    (is (= :proven-equal (forms/prove '(fn [a b] (* a b)) '(fn [a b] (* b a)))))
    (is (= :proven-equal (forms/prove '(fn [a b c] (* a (+ b c)))
                                      '(fn [a b c] (+ (* a b) (* a c))))))
    (is (= :proven-different (forms/prove '(fn [a] (* a a)) '(fn [a] (* 2 a))))))
  (testing "control flow / non-polynomial declines to nil (caller falls back to sampling)"
    (is (nil? (forms/prove '(fn [x] x) '(fn [x] (if (pos? x) x x)))))
    (is (nil? (forms/prove '(fn [a b] (reduce + (map * a b))) '(fn [a b] 0)))))
  (testing "equivalent? reports rank :proven and needs no samples for the polynomial case"
    (let [eq (forms/equivalent? '(fn [a] (+ a a)) '(fn [a] (* 2 a)) [3])]
      (is (true? (:perf.forms/equivalent eq)))
      (is (= :perf.rank/proven (:perf.forms/rank eq))))))

(deftest randomized-pool-catches-differing-outcome
  (testing "a differing pair colliding at x=0,2 is caught — here PROVEN (both polynomials)"
    ;; (+ x x) and x*x collide at x=0 and x=2; a fixed probe set would
    ;; mislabel them synonyms. Canonicalisation proves them different (rank 1)
    ;; without needing the pool at all — strictly better than sampling.
    (let [eq (forms/equivalent? '(fn [x] (* x x)) '(fn [x] (+ x x)) [2.0] 40)]
      (is (false? (:perf.forms/equivalent eq)))
      (is (contains? #{:perf.rank/proven :perf.rank/observed} (:perf.forms/rank eq)))))
  (testing "a genuine equivalence survives the whole pool"
    (let [eq (forms/equivalent? '(fn [a b] (reduce + (map * a b)))
                                '(fn [a b] (+ (* (nth a 0) (nth b 0))
                                              (* (nth a 1) (nth b 1))
                                              (* (nth a 2) (nth b 2))))
                                [[1.0 2.0 3.0] [4.0 5.0 6.0]] 40)]
      (is (true? (:perf.forms/equivalent eq))))))

(deftest classify-labels-from-measured-coordinates
  (testing "differing outcome is labelled differing regardless of cost"
    (let [r (forms/classify '(fn [a b] (reduce + (map * a b)))
                            '(fn [a b] (reduce + (map + a b)))   ; + not *
                            [[1.0 2.0 3.0] [4.0 5.0 6.0]]
                            {:reps 100000 :trials 3})]
      (is (= :perf.forms/differing-outcome (:perf.forms/kind r)))
      (is (false? (:perf.forms/equivalent r)))))
  (testing "a true equivalent is labelled a synonym (cheaper or plain), never differing"
    (let [r (forms/classify '(fn [a b] (reduce + (map * a b)))
                            '(fn [a b] (apply + (map * a b)))
                            [[1.0 2.0 3.0] [4.0 5.0 6.0]]
                            {:reps 100000 :trials 3})]
      (is (true? (:perf.forms/equivalent r)))
      (is (contains? #{:perf.forms/synonym :perf.forms/cheaper-synonym}
                     (:perf.forms/kind r))))))

(deftest discover-generates-from-the-fragment
  (testing "discover derives candidates from the form's own ops — no supplied list"
    (let [rows (forms/discover '(fn [a b] (+ (* a b) a)) [3.0 5.0] {:reps 100000 :trials 3})]
      (is (seq rows) "generated at least one candidate")
      ;; the inline expansion of + is a true synonym the generator found
      (is (some #(and (:perf.forms/equivalent %)
                      (= :perf.forms/synonym (:perf.forms/kind %))) rows))
      ;; op-swaps change the result — caught as differing over the edge+random pool
      (is (some #(= :perf.forms/differing-outcome (:perf.forms/kind %)) rows))
      ;; each classification was either PROVEN (rank 1, no samples needed) or
      ;; saw the full adversarial edge pool
      (is (every? #(or (= :perf.rank/proven (:perf.forms/rank %))
                       (>= (:perf.forms/samples %) 49)) rows)))))

(deftest interval-difference-is-caught
  (testing "a form differing across a whole MID-RANGE interval is not mislabelled equivalent"
    ;; The red-team's break: (fn [x] x) vs a guard that zeroes (100,1000).
    ;; The old clamped-near-args pool never probed that interval and called
    ;; it a cheaper-synonym. The magnitude sweep must hit it.
    (let [eq (forms/equivalent? '(fn [x] (if (and (> x 100.0) (< x 1000.0)) 0.0 x))
                                '(fn [x] x) [2.0] 40)]
      (is (false? (:perf.forms/equivalent eq))
          "a difference on (100,1000) must be found by the magnitude sweep"))))

(deftest discover-reaches-transducer-fusion
  (testing "the generator finds a fused transducer for a reduce-over-seq-pipeline"
    (let [rows (forms/discover '(fn [xs] (->> xs (map inc) (filter even?) (reduce +)))
                               [(vec (range 300))] {:reps 15000 :trials 3})
          fused (filter #(some #{'transduce} (flatten (:perf.forms/candidate %))) rows)]
      (is (seq fused) "a transduce candidate was generated")
      (is (some #(and (:perf.forms/equivalent %)
                      (contains? #{:perf.forms/synonym :perf.forms/cheaper-synonym}
                                 (:perf.forms/kind %)))
                fused)
          "the transduce fusion is recognised as an equivalent synonym, not differing"))))

(deftest both-throw-is-agreement
  (testing "an input where BOTH forms throw is not counted as a difference"
    ;; (even? 1.5) throws; a valid rewrite of an even?-using form must not be
    ;; called differing just because the edge pool fed it a non-integer.
    (let [eq (forms/equivalent? '(fn [xs] (filter even? xs))
                                '(fn [xs] (remove odd? xs))
                                [[1 2 3 4]] 20)]
      (is (true? (:perf.forms/equivalent eq))))))

(deftest edge-pool-catches-corner-only-difference
  (testing "a candidate that differs only at zero is caught by the edge pool"
    ;; (* a a) vs (if (zero? a) 1.0 (* a a)) agree everywhere EXCEPT a=0.0,
    ;; which the random pool of positive-ish perturbations would miss.
    (let [eq (forms/equivalent? '(fn [a] (* a a))
                                '(fn [a] (if (zero? a) 1.0 (* a a)))
                                [3.0] 40)]
      (is (false? (:perf.forms/equivalent eq))
          "the 0.0 edge input must expose the difference"))))

(deftest synonyms-are-compiler-sanctioned
  (testing "macroexpand and inline give mechanical synonyms with no authored rule"
    (is (some #{'(if x (do y))} (forms/synonyms '(when x y))))
    ;; (+ a b) inlines to a Numbers static call — the compiler's own rewrite
    (is (seq (forms/synonyms '(+ a b))))))
