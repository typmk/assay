(ns perf.explain-test
  "Locks the soundness fixes the four-lens review (Clojure/JVM/SBCL/ergonomics)
  found in the first cut of the spine. Every assertion here is a NON-timing
  invariant — a byte count that must be exactly zero, a boolean that must not
  flip, a key that must be read — so it guards the defect, not the machine."
  (:require [clojure.test :refer [deftest is testing]]
            [perf.explain :as explain]
            [perf.measure :as measure]))

(deftest cost-has-no-apply-boxing-floor
  (testing "a fully primitive fn prices at ZERO bytes — the direct-arity driver
            adds no floor (the review's blocker: #(apply f args) added ~120 B)"
    (is (= 0 (:perf.cost/bytes
              (measure/cost '(fn ^long [^long a ^long b] (unchecked-add a b))
                            [3 4] {:quick? true})))))
  (testing "a boxed-collection form prices well above zero — real allocation shows"
    (is (< 100 (:perf.cost/bytes
                (measure/cost '(fn [a b] (reduce + (map * a b)))
                              [[1.0 2.0 3.0] [4.0 5.0 6.0]] {:quick? true}))))))

(deftest cost-flags-unforced-lazy-result
  (testing "a lazy-returning form is flagged, not silently realised (review #3)"
    (let [c (measure/cost '(fn [a b] (map * a b)) [[1.0 2.0] [3.0 4.0]] {:quick? true})]
      (is (true? (:perf.cost/lazy? c))))))

(deftest needs-args-gate-is-not-a-tautology
  (testing "a NULLARY fn needs no args — COST measures it (review #5: the old
            gate counted fn-parts' 4-key map, always 4, so it always tripped)"
    (is (= :perf.rank/measured
           (:perf.explain/rank (:perf.explain/cost (explain/explain '(fn [] (+ 1 2))))))))
  (testing "a fn WITH params and no args reports :needs-args, not a guess"
    (let [cost (:perf.explain/cost (explain/explain '(fn [a b] (+ a b))))]
      (is (= :perf.rank/unavailable (:perf.explain/rank cost))))))

(deftest summary-type-half-is-live
  (testing "summary reads the namespaced :perf.types/unresolved (review #7: it
            read plain :unresolved, always nil, so the type line never showed)"
    (let [lines (explain/summary '(fn [a b] (+ a b)) [1 2])]
      (is (some #(re-find #"unresolved" %) lines)
          "an all-boxed fn must surface its unresolved Object positions"))))

(deftest blind?-is-sound-not-just-allocation
  (testing "a primitive fn is NOT blind — nothing cheaper exists (review #2:
            the old blind? fired on (pos? bytes) alone, blessing optimal code)"
    (is (false? (:perf.explain/blind?
                 (explain/explain '(fn ^long [^long a ^long b] (unchecked-add a b)) [3 4])))))
  (testing "a seq pipeline over which a proven-equivalent transducer fusion
            allocates less IS blind — structure silent AND a cheaper writing exists"
    (let [e (explain/explain '(fn [xs] (->> xs (map inc) (filter even?) (reduce +)))
                             [(vec (range 200))])]
      (is (zero? (count (:perf.explain/data (:perf.explain/structure e))))
          "precondition: the compiler emits no note for the seq pipeline")
      (is (true? (:perf.explain/blind? e))
          "blind? composes STRUCTURE-silent with an OUTCOME byte-saving rewrite"))))
