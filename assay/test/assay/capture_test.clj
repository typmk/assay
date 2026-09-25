(ns assay.capture-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [assay.capture :as capture]))

(defn- raw-rank-1
  "The class on the histogram's `1:` row, read straight off the dcmd text."
  []
  (let [srv (java.lang.management.ManagementFactory/getPlatformMBeanServer)
        on  (javax.management.ObjectName. "com.sun.management:type=DiagnosticCommand")
        out (str (.invoke srv on "gcClassHistogram"
                          (into-array Object [nil]) (into-array String ["[Ljava.lang.String;"])))]
    (some (fn [l] (let [[n _ _ cls] (str/split (str/trim l) #"\s+")]
                    (when (= "1:" n) cls)))
          (str/split-lines out))))

(deftest census-keeps-jdk-classes
  (testing "JDK rows carry a fifth (module) field; a four-field parse dropped all of them"
    (let [names (set (map :assay.class/name (capture/census)))]
      (is (names "java.lang.String"))
      (is (names "[B")))))

(deftest census-keeps-rank-1
  (testing "the largest class is first — a fixed header drop ate it"
    (let [top (:assay.class/name (first (capture/census)))]
      (is (= (raw-rank-1) top)))))
