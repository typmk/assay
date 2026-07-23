(ns perf.query-test
  "Tests exist because the queries are PURE.

  That is the payoff of taking observations as an argument rather than
  reading a singleton: every one of these runs with literal maps, no JVM
  feed, no JFR, no timing, no sleeping. v2's views read global state, so
  testing one meant starting a real recorder and hoping."
  (:require [clojure.test :refer [deftest is testing]]
            [perf.query :as q]
            [perf.model :as m]))

(defn- obs [kind site & {:as attrs}]
  (merge {:perf/kind kind
          :perf/t 0
          :perf/site {:perf/fn site :perf/line 1}
          :perf/via {:perf/fn site :perf/line 1}
          :perf/stack [{:perf/depth 0 :perf/fn site :perf/line 1}]}
         attrs))

(deftest allocation-ranks-by-count
  (let [data [(obs :perf.kind/alloc "app/hot")
              (obs :perf.kind/alloc "app/hot")
              (obs :perf.kind/alloc "app/cold")]
        [top second-] (q/allocation data)]
    (is (= "app/hot" (:perf/fn top)))
    (is (= 2 (:perf/n top)))
    (is (= "app/cold" (:perf/fn second-)))))

(deftest allocation-excludes-noise
  (testing "clojure.* and the tooling's own frames are never sites"
    (is (empty? (q/allocation [(obs :perf.kind/alloc "clojure.core/vec")
                               (obs :perf.kind/alloc "perf/watch")
                               (obs :perf.kind/alloc "nrepl.server/handle")])))))

(deftest blocking-sums-durations
  (let [data [(obs :perf.kind/block "app/lock" :perf.block/duration-ns 100)
              (obs :perf.kind/block "app/lock" :perf.block/duration-ns 250)]
        [top] (q/blocking data)]
    (is (= 350 (:perf/total top)))
    (is (= 2 (:perf/n top)))))

(deftest deopts-keep-site-and-mechanism
  (testing "a megamorphic call deopts inside clojure.lang.Var — both are kept"
    (let [data [(assoc (obs :perf.kind/deopt "app/poly")
                       :perf/via {:perf/fn "clojure.lang.Var"}
                       :perf.deopt/reason "class_check")]
          [top] (q/deopts data)]
      (is (= "app/poly" (:perf/fn top)))
      (is (= "clojure.lang.Var" (:perf/via top)))
      (is (= "class_check" (:perf.deopt/reason top))))))

(deftest callers-collapses-duplicate-frames
  (testing "Clojure emits invoke -> invokeStatic; without collapsing, every
            fn appears to call itself"
    (let [data [{:perf/kind :perf.kind/alloc
                 :perf/stack [{:perf/fn "app/inner"} {:perf/fn "app/inner"}
                              {:perf/fn "app/outer"} {:perf/fn "app/outer"}]}]
          [top] (q/callers data "app/inner")]
      (is (= "app/outer" (:perf/caller top))))))

(deftest rates-verdict-watches-the-floor
  (testing "a rising post-GC floor is the leak; the peak says nothing"
    (let [rising (concat (repeat 4 {:perf/heap 100 :perf/alloc-rate 1})
                         (repeat 4 {:perf/heap 500 :perf/alloc-rate 1}))
          steady (repeat 8 {:perf/heap 100 :perf/alloc-rate 1})]
      (is (= :perf.verdict/floor-rising (:perf/verdict (q/rates rising 500))))
      (is (= :perf.verdict/stable (:perf/verdict (q/rates steady 500))))))
  (testing "too few samples is not a verdict"
    (is (= :perf.verdict/warming-up (:perf/verdict (q/rates [{:perf/heap 1}] 500))))))

(deftest growth-reports-only-increases
  (let [before [#:perf.class{:name "A" :instances 10 :bytes 100}
                #:perf.class{:name "B" :instances 5 :bytes 50}]
        after  [#:perf.class{:name "A" :instances 40 :bytes 400}
                #:perf.class{:name "B" :instances 5 :bytes 50}]
        [top & rest-] (q/growth before after)]
    (is (= "A" (:perf.class/name top)))
    (is (= 30 (:perf/d-instances top)))
    (is (empty? rest-) "B did not grow, so B is not reported")))

(deftest keys-are-qualified
  (testing "results cross library boundaries, so every key carries provenance"
    (let [row (first (q/allocation [(obs :perf.kind/alloc "app/f")]))]
      (is (every? namespace (keys row))))))

(deftest own-frame-tests-slash-not-dollar
  (testing "frames are demunged on the way in: user$f is already user/f,
            and testing for $ silently matched nothing"
    (is (m/own-frame? "app/handler"))
    (is (not (m/own-frame? "app$handler")))
    (is (not (m/own-frame? "clojure.core/map")))))
