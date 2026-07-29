(ns assay.query-test
  "Tests exist because the queries are PURE.

  That is the payoff of taking observations as an argument rather than
  reading a singleton: every one of these runs with literal maps, no JVM
  feed, no JFR, no timing, no sleeping. v2's views read global state, so
  testing one meant starting a real recorder and hoping."
  (:require [clojure.test :refer [deftest is testing]]
            [assay.query :as q]
            [assay.model :as m]))

(defn- obs [kind site & {:as attrs}]
  (merge {:assay/kind kind
          :assay/t 0
          :assay/site {:assay/fn site :assay/line 1}
          :assay/via {:assay/fn site :assay/line 1}
          :assay/stack [{:assay/depth 0 :assay/fn site :assay/line 1}]}
         attrs))

(deftest allocation-ranks-by-count
  (let [data [(obs :assay.kind/alloc "app/hot")
              (obs :assay.kind/alloc "app/hot")
              (obs :assay.kind/alloc "app/cold")]
        [top second-] (q/allocation data)]
    (is (= "app/hot" (:assay/fn top)))
    (is (= 2 (:assay/n top)))
    (is (= "app/cold" (:assay/fn second-)))))

(deftest allocation-excludes-noise
  ;; This used to assert against a hardcoded prefix list, so it could name
  ;; any namespace it liked — "nrepl.server/handle" was excluded whether or
  ;; not nrepl was anywhere near the classpath. Ownership is now DERIVED
  ;; from where the namespace's file actually lives, which is more honest
  ;; and costs exactly this: the test must name namespaces really present,
  ;; and assay's own frames resolve differently depending on whether the
  ;; suite ran against the jar or against src. What holds either way is the
  ;; jar rule, so that is what is asserted.
  (testing "a namespace that demonstrably comes from a JAR is never a site"
    (is (empty? (q/allocation [(obs :assay.kind/alloc "clojure.core/vec")
                               (obs :assay.kind/alloc "clojure.string/join")]))))
  (testing "one that cannot be proven foreign is KEPT — hiding your own hot
            code is the more expensive of the two errors"
    (is (seq (q/allocation [(obs :assay.kind/alloc "app/handler")])))))

(deftest blocking-sums-durations
  (let [data [(obs :assay.kind/block "app/lock" :assay.block/duration-ns 100)
              (obs :assay.kind/block "app/lock" :assay.block/duration-ns 250)]
        [top] (q/blocking data)]
    (is (= 350 (:assay/total top)))
    (is (= 2 (:assay/n top)))))

(deftest deopts-keep-site-and-mechanism
  (testing "a megamorphic call deopts inside clojure.lang.Var — both are kept"
    (let [data [(assoc (obs :assay.kind/deopt "app/poly")
                       :assay/via {:assay/fn "clojure.lang.Var"}
                       :assay.deopt/reason "class_check")]
          [top] (q/deopts data)]
      (is (= "app/poly" (:assay/fn top)))
      (is (= "clojure.lang.Var" (:assay/via top)))
      (is (= "class_check" (:assay.deopt/reason top))))))

(deftest callers-collapses-duplicate-frames
  (testing "Clojure emits invoke -> invokeStatic; without collapsing, every
            fn appears to call itself"
    (let [data [{:assay/kind :assay.kind/alloc
                 :assay/stack [{:assay/fn "app/inner"} {:assay/fn "app/inner"}
                              {:assay/fn "app/outer"} {:assay/fn "app/outer"}]}]
          [top] (q/callers data "app/inner")]
      (is (= "app/outer" (:assay/caller top))))))

(deftest rates-verdict-watches-the-floor
  (testing "a rising post-GC floor is the leak; the peak says nothing"
    (let [rising (concat (repeat 4 {:assay/heap 100 :assay/alloc-rate 1})
                         (repeat 4 {:assay/heap 500 :assay/alloc-rate 1}))
          steady (repeat 8 {:assay/heap 100 :assay/alloc-rate 1})]
      (is (= :assay.verdict/floor-rising (:assay/verdict (q/rates rising 500))))
      (is (= :assay.verdict/stable (:assay/verdict (q/rates steady 500))))))
  (testing "too few samples is not a verdict"
    (is (= :assay.verdict/warming-up (:assay/verdict (q/rates [{:assay/heap 1}] 500))))))

(deftest growth-reports-only-increases
  (let [before [#:assay.class{:name "A" :instances 10 :bytes 100}
                #:assay.class{:name "B" :instances 5 :bytes 50}]
        after  [#:assay.class{:name "A" :instances 40 :bytes 400}
                #:assay.class{:name "B" :instances 5 :bytes 50}]
        [top & rest-] (q/growth before after)]
    (is (= "A" (:assay.class/name top)))
    (is (= 30 (:assay/d-instances top)))
    (is (empty? rest-) "B did not grow, so B is not reported")))

(deftest keys-are-qualified
  (testing "results cross library boundaries, so every key carries provenance"
    (let [row (first (q/allocation [(obs :assay.kind/alloc "app/f")]))]
      (is (every? namespace (keys row))))))

(deftest own-frame-tests-slash-not-dollar
  (testing "frames are demunged on the way in: user$f is already user/f,
            and testing for $ silently matched nothing"
    (is (m/own-frame? "app/handler"))
    (is (not (m/own-frame? "app$handler")))
    (is (not (m/own-frame? "clojure.core/map")))))

(deftest by-fn-joins-compile-and-runtime
  (let [notes [#:assay.note{:var 'user/hot :code :assay.note/reflection
                           :message "x" :span {:assay/line 3}}]
        obs   [{:assay/kind :assay.kind/alloc :assay/site {:assay/fn "user/hot" :assay/line 3}}
               {:assay/kind :assay.kind/alloc :assay/site {:assay/fn "user/hot" :assay/line 3}}
               {:assay/kind :assay.kind/block :assay/site {:assay/fn "user/cold" :assay/line 9}}]
        rows  (assay.query/by-fn obs notes)
        hot   (first (filter #(= "user/hot" (:assay.fn/name %)) rows))]
    (is (= 1 (count (:assay.fn/compile hot))) "the reflection note")
    (is (= 2 (get (:assay.fn/runtime hot) :assay.kind/alloc)) "two alloc samples")
    (is (= #{"user/cold" "user/hot"} (set (map :assay.fn/name rows))))))

(deftest callers-matches-by-name-not-prefix
  ;; red-team: str/starts-with? fabricated phantom callers.
  (let [obs [{:assay/stack [{:assay/fn "app/foo"} {:assay/fn "app/realcaller"}]}]]
    (is (= [] (assay.query/callers obs "app/f")) "app/f must NOT match app/foo")
    (is (= 1 (count (assay.query/callers obs "app/foo"))) "exact match works")
    (is (= 1 (count (assay.query/callers obs "foo"))) "bare name matches qualified frame")
    (is (= [] (assay.query/callers obs "realcall")) "no prefix phantom on the caller side")))
