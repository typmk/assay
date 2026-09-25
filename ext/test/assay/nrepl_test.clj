(ns assay.nrepl-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.edn :as edn]
            [nrepl.core :as nrepl]
            [nrepl.server :as server]
            [assay.nrepl]
            [assay.repl :as repl]))

(def ^:dynamic *client* nil)

(use-fixtures :once
  (fn [run]
    (let [srv (server/start-server :port 0
                                   :handler (server/default-handler #'assay.nrepl/wrap-assay))]
      (try
        (with-open [conn (nrepl/connect :port (:port srv))]
          (binding [*client* (nrepl/client conn 120000)]
            (repl/start!)
            (run)))
        (finally (server/stop-server srv) (repl/stop!))))))

(defn- value [op req]
  (let [resp (nrepl/combine-responses (nrepl/message *client* (assoc req :op op)))]
    (some->> (:assay/value resp) (edn/read-string {:default (fn [tag v] [tag v])}))))

(defn- answers? [op req]
  (let [v (value op req)]
    (is (some? v) (str op " returned no :assay/value"))
    (is (not (and (map? v) (:assay/error v))) (str op " → " (pr-str v)))))

(def client-ops
  "The ops gna-assay.el sends. Renaming one breaks the editor with no error here
  unless this set names it."
  #{"assay/allocation" "assay/blocking" "assay/boxing" "assay/callers" "assay/deopts"
    "assay/describe" "assay/fix" "assay/notes" "assay/scan" "assay/types"
    "assay/weigh" "assay/writing"})

(deftest the-op-table-covers-the-editor
  (let [handled (set (keys (:handles (:nrepl.middleware/descriptor (meta #'assay.nrepl/wrap-assay)))))]
    (is (empty? (remove handled client-ops)))))

(deftest cheap-ops-answer-edn
  (testing "compile-time and session reads"
    (doseq [[op req] {"assay/notes"      {:form "(fn [x] (+ x 1))"}
                      "assay/types"      {:form "(fn [x] (+ x 1))" :args "[1]"}
                      "assay/summary-of" {:form "(fn [x] (+ x 1))" :args "[1]"}
                      "assay/summary"    {}
                      "assay/describe"   {}
                      "assay/allocation" {:n "5"}
                      "assay/blocking"   {:n "5"}
                      "assay/deopts"     {:n "5"}
                      "assay/callers"    {:sym "clojure.core/str" :n "5"}
                      "assay/boxing"     {:ns "clojure.core" :sym "inc"}
                      "assay/scan"       {:ns "clojure.set"}
                      "assay/by-fn"      {:ns "clojure.set"}}]
      (answers? op req))))

(deftest ^:slow measuring-ops-answer-edn
  (testing "ops that run criterium"
    (doseq [[op req] {"assay/weigh"   {:form "(fn [xs] (reduce + xs))" :args "[[1 2 3]]"}
                      "assay/fix"     {:form "(fn [a b] (+ a b))" :args "[3 4]"}
                      "assay/writing" {:form "(fn [xs] (reduce + (map inc xs)))" :args "[[1 2 3]]"}
                      "assay/explain" {:form "(fn [xs] (reduce + xs))" :args "[[1 2 3]]"}}]
      (answers? op req))))

(deftest unknown-ops-fall-through
  (is (nil? (value "assay/no-such-op" {}))))
