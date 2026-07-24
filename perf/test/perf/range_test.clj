(ns perf.range-test
  "perf.range vs SBCL's inferred bounds — every derivation must be SOUND
  (the swept observation stays inside it), and match SBCL on the core
  arithmetic examples."
  (:require [clojure.test :refer [deftest is testing]]
            [perf.range :as rng]))

(defn- d [form env] (rng/derive form env))

(deftest matches-sbcl-arithmetic
  (is (= [1 21]  (:perf.range/derived (d '(+ (* x 2) 1) '{x [0 10]}))))
  (is (= [0 100] (:perf.range/derived (d '(* x x) '{x [0 10]}))))
  (is (= [0 50]  (:perf.range/derived (d '(* x y) '{x [0 10] y [0 5]}))))
  (is (= [0 2]   (:perf.range/derived (d '(mod x 3) '{x [0 1000]}))))
  (is (= [0 7]   (:perf.range/derived (d '(abs x) '{x [-7 3]}))))
  (is (= [1 21]  (:perf.range/derived (d '(let [y (* x 2)] (+ y 1)) '{x [0 10]})))))

(deftest every-derivation-is-sound
  ;; the perf move: the swept box must never escape the derived interval.
  (doseq [[form env] '[[(+ (* x 2) 1) {x [0 10]}]
                       [(* x x) {x [-10 10]}]
                       [(if (> x 5) x 0) {x [0 10]}]
                       [(min (inc x) 8) {x [0 20]}]
                       [(- (abs x) 3) {x [-7 3]}]]]
    (is (:perf.range/sound (d form env))
        (str "unsound derivation for " form))))

(deftest branch-refinement
  ;; convex hull, sound but looser than SBCL's disjoint union — documented.
  (is (:perf.range/sound (d '(if (> x 5) x 0) '{x [0 10]}))))
