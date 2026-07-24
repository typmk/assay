(ns perf.code-test
  "Tests for the compile-cost rungs — written after 13 bugs were found in
  this code by hand, most with silent-empty or inverted failure modes. The
  cases here are exactly those: an empty result where there should be
  findings, a wrapped lazy seq, a root binding that did not take, a stored
  number that should not exist."
  (:require [clojure.test :refer [deftest is testing]]
            [perf.code :as code]
            [perf.measure :as measure]))

(deftest notes-find-and-order
  (testing "reflection and boxed math are both found"
    (let [ns (code/notes '(defn f [a b s] (+ (* a b) (.length s))))]
      (is (= #{:perf.note/reflection :perf.note/boxed-math}
             (set (map :perf.note/code ns))))))
  (testing "ranked by MECHANISM — reflection sorts before boxing"
    (let [ns (code/notes '(defn f [a b s] (+ (* a b) (.length s))))]
      (is (= :perf.note/reflection (:perf.note/code (first ns))))))
  (testing "a clean fn says nothing"
    (is (= [] (code/notes '(defn f ^long [^long n] (* n 2)))))))

(deftest notes-carry-no-remembered-magnitude
  ;; the invariant Apollo insisted on: perf stores no measured cost.
  (let [ns (code/notes '(defn f [s] (.length s)))]
    (is (not-any? :perf.note/cost ns))
    (is (not-any? :perf.note/cost-basis ns))))

(deftest notes-are-derived-not-authored
  ;; every string on a note traces to the compiler or the class.
  (let [n (first (code/notes '(defn f [s] (.length s))))]
    (is (re-find #"can't be resolved" (:perf.note/message n))
        "message is the compiler's own string")
    (is (nil? (:perf.note/why n)) "no perf-authored why")
    (is (nil? (:perf.note/suggestion n)) "no perf-authored suggestion")))

(deftest boxed-note-reads-overloads-off-the-class
  (let [n (first (code/notes '(defn f [a b] (+ a b))))]
    (is (= :perf.note/boxed-math (:perf.note/code n)))
    (is (some #(re-find #"long,long" %) (:perf.note/refused n))
        "the primitive overload the compiler refused, read off Numbers")))

(deftest kind-rank-is-structural-order
  (is (< (code/kind-rank :perf.note/reflection)
         (code/kind-rank :perf.note/boxed-math)))
  (is (= 99 (code/kind-rank :perf.note/nonsense)) "unknown kinds sort last"))

(deftest explain-accepts-one-note-or-a-seq
  ;; the vector? bug: a lazy seq from take/filter was wrapped and iterated
  ;; as ONE note whose every key was nil -> NullPointerException.
  (let [ns (code/notes '(defn f [s] (.length s)))]
    (is (string? (code/explain ns)))
    (is (string? (code/explain (first ns))) "a single note map")
    (is (string? (code/explain (take 1 ns))) "a lazy seq")
    (is (string? (code/explain [])) "nothing")))

(deftest explain-leads-with-the-fetch
  ;; emitted/took/refused before the interpretive lines; no authored prose.
  (let [out (code/explain (code/notes '(defn f [a b] (+ a b))))]
    (is (re-find #"took:" out))
    (is (not (re-find #"why:" out)) "no authored why in the render")
    (is (not (re-find #"help:" out)) "no authored suggestion in the render")))

(deftest types-verdict
  (is (= :perf.types/all-boxed
         (:perf.types/verdict (code/types '(fn [a b] (+ a b))))))
  (is (= :perf.types/resolved
         (:perf.types/verdict (code/types '(fn ^long [^long a ^long b] (+ a b))))))
  (testing "sample values give inferred-vs-actual"
    (let [t (code/types '(fn [a b] (+ a b)) [3 4])]
      (is (= '[Object Object] (:perf.types/params t)))
      (is (= '[Long Long] (:perf.types/actual t))))))

(deftest weigh-measures-live-and-is-honest-below-resolution
  (testing "reflection: a large, real effect resolves cleanly"
    (let [w (measure/weigh '(fn [s] (.length s)) ["hello"] {:trials 3 :reps 200000})]
      (is (:perf.weigh/verified w) "the rewrite silenced the notes")
      (is (:perf.weigh/same-result w))
      (is (= :perf.weigh/hinting-is-faster (:perf.weigh/verdict w)))
      (is (> (:perf.weigh/factor w) 5) "reflection is many times slower")))
  (testing "nothing to compare when the form is already clean"
    (is (= :perf.weigh/nothing-to-compare
           (:perf.weigh/verdict (measure/weigh '(fn ^long [^long n] (* n 2)) [7]))))))

(deftest weigh-does-not-run-the-form-as-a-side-effect-vehicle
  ;; notes* evaluates; wrapping in fn keeps the effect from firing.
  (let [fired (atom false)]
    (code/notes (list 'fn [] (list 'reset! fired true)))
    (is (false? @fired) "a wrapped effect is built, not called")))

(deftest watch-round-trips-the-root-bindings
  (let [warn0 (.getRawRoot #'*warn-on-reflection*)
        math0 (.getRawRoot #'*unchecked-math*)]
    (code/watch!)
    (eval '(defn watched-reflect [s] (.length s)))
    (eval '(defn watched-box [a b] (+ a b)))
    (let [seen (set (map :perf.note/code (code/watched)))]
      (is (contains? seen :perf.note/reflection))
      (is (contains? seen :perf.note/boxed-math)
          "boxed-math fired — the thread-local shadow bug is fixed"))
    (code/unwatch!)
    (is (= warn0 (.getRawRoot #'*warn-on-reflection*)) "root restored")
    (is (= math0 (.getRawRoot #'*unchecked-math*)) "root restored")))

(deftest fix-returns-a-verified-paste-able-rewrite
  ;; fix returns non-nil ONLY when the rewrite recompiled clean — that is
  ;; the verification; there is no separate flag to trust.
  (let [f (measure/fix '(fn [a b] (+ a b)) [3 4])]
    (is (some? f) "a rewrite that verified")
    (is (zero? (:perf.fix/notes-after f)) "the notes went silent")
    (is (:perf.fix/same-result f))
    (is (re-find #"\^long" (:perf.fix/source f)) "hints are visible in the source"))
  (is (nil? (measure/fix '(fn ^long [^long n] (* n 2)) [7])) "nothing to fix -> nil"))
