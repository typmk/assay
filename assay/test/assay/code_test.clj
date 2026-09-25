(ns assay.code-test
  "Tests for the compile-cost rungs — written after 13 bugs were found in
  this code by hand, most with silent-empty or inverted failure modes. The
  cases here are exactly those: an empty result where there should be
  findings, a wrapped lazy seq, a root binding that did not take, a stored
  number that should not exist."
  (:require [clojure.test :refer [deftest is testing]]
            [assay.code :as code]
            [assay.measure :as measure]
            [assay.watch :as watch]))

(deftest notes-find-and-order
  (testing "reflection and boxed math are both found"
    (let [ns (code/notes '(defn f [a b s] (+ (* a b) (.length s))))]
      (is (= #{:assay.note/reflection :assay.note/boxed-math}
             (set (map :assay.note/code ns))))))
  (testing "ranked by MECHANISM — reflection sorts before boxing"
    (let [ns (code/notes '(defn f [a b s] (+ (* a b) (.length s))))]
      (is (= :assay.note/reflection (:assay.note/code (first ns))))))
  (testing "a clean fn says nothing"
    (is (= [] (code/notes '(defn f ^long [^long n] (* n 2)))))))

(deftest notes-carry-no-remembered-magnitude
  ;; the invariant Apollo insisted on: assay stores no measured cost.
  (let [ns (code/notes '(defn f [s] (.length s)))]
    (is (not-any? :assay.note/cost ns))
    (is (not-any? :assay.note/cost-basis ns))))

(deftest notes-are-derived-not-authored
  ;; every string on a note traces to the compiler or the class.
  (let [n (first (code/notes '(defn f [s] (.length s))))]
    (is (re-find #"can't be resolved" (:assay.note/message n))
        "message is the compiler's own string")
    (is (nil? (:assay.note/why n)) "no assay-authored why")
    (is (nil? (:assay.note/suggestion n)) "no assay-authored suggestion")))

(deftest boxed-note-reads-overloads-off-the-class
  (let [n (first (code/notes '(defn f [a b] (+ a b))))]
    (is (= :assay.note/boxed-math (:assay.note/code n)))
    (is (some #(re-find #"long,long" %) (:assay.note/refused n))
        "the primitive overload the compiler refused, read off Numbers")))

(deftest kind-rank-is-structural-order
  (is (< (code/kind-rank :assay.note/reflection)
         (code/kind-rank :assay.note/boxed-math)))
  (is (= 99 (code/kind-rank :assay.note/nonsense)) "unknown kinds sort last"))

(deftest explain-accepts-one-note-or-a-seq
  ;; the vector? bug: a lazy seq from take/filter was wrapped and iterated
  ;; as ONE note whose every key was nil -> NullPointerException.
  (let [ns (code/notes '(defn f [s] (.length s)))]
    (is (string? (code/print-notes ns)))
    (is (string? (code/print-notes (first ns))) "a single note map")
    (is (string? (code/print-notes (take 1 ns))) "a lazy seq")
    (is (string? (code/print-notes [])) "nothing")))

(deftest explain-leads-with-the-fetch
  ;; emitted/took/refused before the interpretive lines; no authored prose.
  (let [out (code/print-notes (code/notes '(defn f [a b] (+ a b))))]
    (is (re-find #"took:" out))
    (is (not (re-find #"why:" out)) "no authored why in the render")
    (is (not (re-find #"help:" out)) "no authored suggestion in the render")))

(deftest types-verdict
  (is (= :assay.types/all-boxed
         (:assay.types/verdict (code/types '(fn [a b] (+ a b))))))
  (is (= :assay.types/resolved
         (:assay.types/verdict (code/types '(fn ^long [^long a ^long b] (+ a b))))))
  (testing "sample values give inferred-vs-actual"
    (let [t (code/types '(fn [a b] (+ a b)) [3 4])]
      (is (= '[Object Object] (:assay.types/params t)))
      (is (= '[Long Long] (:assay.types/actual t))))))

(deftest ^:slow weigh-measures-live-and-is-honest-below-resolution
  (testing "reflection: a large, real effect resolves cleanly"
    (let [w (measure/weigh '(fn [s] (.length s)) ["hello"] {:trials 3 :reps 200000})]
      (is (:assay.weigh/verified w) "the rewrite silenced the notes")
      (is (:assay.weigh/same-result w))
      (is (= :assay.weigh/hinting-is-faster (:assay.weigh/verdict w)))
      (is (> (:assay.weigh/factor w) 5) "reflection is many times slower")))
  (testing "nothing to compare when the form is already clean"
    (is (= :assay.weigh/nothing-to-compare
           (:assay.weigh/verdict (measure/weigh '(fn ^long [^long n] (* n 2)) [7]))))))

(deftest weigh-does-not-run-the-form-as-a-side-effect-vehicle
  ;; notes* evaluates; wrapping in fn keeps the effect from firing.
  (let [fired (atom false)]
    (code/notes (list 'fn [] (list 'reset! fired true)))
    (is (false? @fired) "a wrapped effect is built, not called")))

(deftest watch-round-trips-the-root-bindings
  (let [warn0 (.getRawRoot #'*warn-on-reflection*)
        math0 (.getRawRoot #'*unchecked-math*)]
    (watch/watch!)
    (eval '(defn watched-reflect [s] (.length s)))
    (eval '(defn watched-box [a b] (+ a b)))
    (let [seen (set (map :assay.note/code (watch/watched)))]
      (is (contains? seen :assay.note/reflection))
      (is (contains? seen :assay.note/boxed-math)
          "boxed-math fired — the thread-local shadow bug is fixed"))
    (watch/unwatch!)
    (is (= warn0 (.getRawRoot #'*warn-on-reflection*)) "root restored")
    (is (= math0 (.getRawRoot #'*unchecked-math*)) "root restored")))

(deftest ^:slow fix-returns-a-verified-paste-able-rewrite
  ;; fix returns non-nil ONLY when the rewrite recompiled clean — that is
  ;; the verification; there is no separate flag to trust.
  (let [f (measure/fix '(fn [a b] (+ a b)) [3 4])]
    (is (some? f) "a rewrite that verified")
    (is (zero? (:assay.fix/notes-after f)) "the notes went silent")
    (is (:assay.fix/same-result f))
    (is (re-find #"\^long" (:assay.fix/source f)) "hints are visible in the source"))
  (is (nil? (measure/fix '(fn ^long [^long n] (* n 2)) [7])) "nothing to fix -> nil"))

(deftest types-multiarity-does-not-hide-boxing
  ;; red-team: reporting the first arity hid boxing in the others.
  (let [t (code/types '(fn ([a] a) ([a b] (+ a b))))]
    (is (= :assay.types/all-boxed (:assay.types/verdict t))
        "the boxed 2-arg arity must surface, not be hidden by the 1-arg")
    (is (>= (:assay.types/arities t) 2) "counts both arities")))

(deftest watching?-reflects-state-not-output
  ;; red-team: the toggle could not turn OFF when watched was empty.
  (is (false? (watch/watching?)))
  (watch/watch!)
  (is (true? (watch/watching?)) "on, even with zero notes accrued yet")
  (watch/unwatch!)
  (is (false? (watch/watching?))))

(deftest notes-parse-source-names-with-colons
  ;; The bug that only a LIVE nREPL surfaced: the compiler's source name
  ;; there is "*cider-repl host:127.0.0.1:PORT(clj)*" — colons everywhere.
  ;; warning-re used [^:] for the file segment, so it matched NO_SOURCE_PATH
  ;; (every -e test and the suite) but NOTHING in a real REPL, and notes
  ;; returned [] live while passing every test.
  (let [w (java.io.StringWriter.)
        tmp (create-ns (gensym "colon-probe"))]
    (binding [*warn-on-reflection* true *err* w *ns* tmp]
      (clojure.core/refer-clojure)
      (eval '(fn [s] (.length s))))
    (remove-ns (ns-name tmp))
    (let [raw (str w)
          ;; force a colon-laden source name like a live nREPL's
          faked (clojure.string/replace raw #"warning, [^:]*:"
                                        "warning, *cider-repl h:127.0.0.1:43623(clj)*:")]
      (is (re-find #"Reflection warning" faked) "the raw warning is present")
      (is (= 1 (count (#'assay.code/parse-warnings faked)))
          "parses a source name containing colons"))))
