(ns assay.load-test
  (:require [clojure.test :refer [deftest is testing]]))

(def aot-set
  "The namespaces build.sh compiles into assay-ext.jar."
  '[assay.diagnose assay.control assay.native assay.flow assay.trace assay.remote assay.jit])

(deftest every-jarred-namespace-loads
  (testing "a namespace that fails to load here is a jar build that fails"
    (doseq [n aot-set]
      (is (nil? (require n)) (str n))
      (is (find-ns n) (str n)))))
