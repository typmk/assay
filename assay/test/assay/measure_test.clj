(ns assay.measure-test
  "Covers the parts of measure/weigh and capture that are NOT timing —
  timing verdicts are inherently machine- and load-dependent, so the
  assertions here are on the invariants a wrong result would violate:
  correctness tracking, vocabulary acceptance, and the snapshot seam."
  (:require [clojure.test :refer [deftest is testing]]
            [assay.measure :as measure]
            [assay.capture :as capture]
            [assay.query :as query]))

(deftest weigh-against-explicit-alternative
  (testing "an explicit :against rewrite is measured even when the compiler is silent"
    (let [w (measure/weigh '(fn [a b] (reduce + (map * a b)))
                           [[1.0 2.0 3.0] [4.0 5.0 6.0]]
                           {:against '(fn [a b]
                                        (+ (* (double (nth a 0)) (double (nth b 0)))
                                           (* (double (nth a 1)) (double (nth b 1)))
                                           (* (double (nth a 2)) (double (nth b 2)))))
                            :reps 50000 :trials 3})]
      ;; the notes gate does NOT fire this path: it runs though notes-before is 0
      (is (not= :assay.weigh/nothing-to-compare (:assay.weigh/verdict w)))
      (is (contains? #{:assay.weigh/alternative-is-faster
                       :assay.weigh/alternative-is-slower
                       :assay.weigh/inconclusive}
                     (:assay.weigh/verdict w)))
      (is (= :assay.cost.basis/measured (:assay.weigh/basis w)))
      ;; the primitive unroll returns the same dot product as the boxed form
      (is (true? (:assay.weigh/same-result w)))
      (is (some? (:assay.weigh/factor w))))))

(deftest weigh-against-catches-wrong-rewrite
  (testing "a rewrite that changes the answer is reported, not hidden"
    (let [w (measure/weigh '(fn [a b] (reduce + (map * a b)))
                           [[1.0 2.0 3.0] [4.0 5.0 6.0]]
                           {:against '(fn [a b] 0.0)   ; obviously wrong
                            :reps 20000 :trials 3})]
      (is (false? (:assay.weigh/same-result w))
          "same-result must be false for a rewrite that returns a different value"))))

(deftest weigh-numeric-tolerance
  (testing "= would call (= 6 6.0) false; same? treats a primitive rewrite as equal"
    (let [w (measure/weigh '(fn [a b] (+ a b))       ; may return a Long
                           [3 4]
                           {:against '(fn [a b] (double (+ a b)))  ; returns 7.0
                            :reps 20000 :trials 3})]
      (is (true? (:assay.weigh/same-result w))
          "7 and 7.0 are the same result within numeric tolerance"))))

(deftest capture-accepts-keyword-types
  (testing "start speaks the same keyword vocabulary as the rest of assay"
    ;; keyword :types must NOT throw ClassCastException (the pre-fix bug)
    (let [rec (capture/start {:types [:alloc] :period-ms 50})]
      (try
        (is (some? rec))
        (is (vector? (capture/observations rec)))
        (finally (capture/stop rec)))))
  (testing "an unknown event keyword fails loud, not with an opaque cast error"
    (is (thrown? IllegalArgumentException
                 (capture/start {:types [:not-an-event]})))))

(deftest observations-accepts-snapshot
  (testing "observations reads a snapshot value, not only a live recorder"
    (let [rec  (capture/start {:types [:alloc] :period-ms 50})
          _    (capture/stop rec)
          snap (capture/snapshot rec)]
      ;; a caller holding a snapshot naturally calls (observations snap);
      ;; before the fix this read [:stream :obs] off the snapshot and got []
      (is (= (query/allocation (capture/observations snap))
             (query/allocation (:assay/observations snap)))
          "the snapshot path and the recorder path feed query identically"))))
