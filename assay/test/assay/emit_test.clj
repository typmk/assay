(ns assay.emit-test
  (:require [clojure.test :refer [deftest is testing]]
            [assay.emit :as emit]))

(deftest emit-lands-in-jfr-stream
  (testing "a custom assay event (EventFactory) is captured in a JFR stream with typed fields"
    (let [rs  (jdk.jfr.consumer.RecordingStream.)
          got (atom nil)]
      (.enable rs "assay.Test")
      (.onEvent rs "assay.Test"
                (reify java.util.function.Consumer
                  (accept [_ e] (reset! got {:factor (.getDouble e "factor")
                                             :name (.getString e "name")
                                             :reps (.getLong e "reps")}))))
      (.startAsync rs)
      (Thread/sleep 300)
      (emit/emit "assay.Test" {:factor 8.28 :name "dot" :reps 1000000})
      (loop [i 0] (when (and (nil? @got) (< i 60)) (Thread/sleep 100) (recur (inc i))))
      (.close rs)
      (is (= {:factor 8.28 :name "dot" :reps 1000000} @got)))))
