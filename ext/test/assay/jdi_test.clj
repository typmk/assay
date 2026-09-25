(ns assay.jdi-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [assay.remote :as remote]))

(defn- free-port []
  (with-open [s (java.net.ServerSocket. 0)] (.getLocalPort s)))

(defn- java-bin []
  (str (System/getProperty "java.home") "/bin/java"))

(defn- spawn
  "A child JVM with a jdwp agent on PORT, running EXPR. Returns the Process
  once the agent listens, and its stdout reader."
  [port expr]
  (let [p (.start (ProcessBuilder.
                   ^java.util.List
                   [(java-bin)
                    (str "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=127.0.0.1:" port)
                    (str "-Djdwp.port=" port)
                    "-cp" (System/getProperty "java.class.path")
                    "clojure.main" "-e" (pr-str expr)]))
        out (io/reader (.getInputStream p))]
    (loop [n 0]
      (let [l (.readLine out)]
        (cond (nil? l) (throw (ex-info "child exited before jdwp listened" {:port port}))
              (.contains l "Listening for transport") [p out]
              (< n 50) (recur (inc n))
              :else (throw (ex-info "no jdwp banner" {:port port})))))))

(deftest remote-attaches-and-detaches
  (let [port (free-port)
        [p _] (spawn port '(Thread/sleep 60000))]
    (try
      (let [c (remote/attach port)]
        (is (instance? com.sun.jdi.VirtualMachine (:assay.remote/vm c)))
        (is (= :detached (remote/detach c))))
      (finally (.destroyForcibly p)))))

(deftest control-arms-restarts-in-a-jdwp-vm
  (testing "restarts! self-attaches through jdwp.port and arms"
    (let [port (free-port)
          [p out] (spawn port '(do (require 'assay.control)
                                   (println :result ((resolve 'assay.control/restarts!)))
                                   (flush)
                                   (System/exit 0)))]
      (try
        (let [lines (doall (line-seq out))]
          (is (some #{":result :armed"} lines) (pr-str lines)))
        (finally (.destroyForcibly p))))))
