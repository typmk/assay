(ns perf.jvmci
  "The JIT's OWN profiling — observed types and branch probabilities, read
  straight from JVMCI. This is the one thing perf otherwise cannot give and
  SBCL's static type inference can only approximate: not what types COULD
  flow (inference) but what types DID flow (measurement), per call site, as
  the running JIT recorded them.

  GRAAL ONLY. Requires a JVMCI-enabled runtime (GraalVM) launched with:

    --add-modules=jdk.internal.vm.ci
    -XX:+UnlockExperimentalVMOptions -XX:+EnableJVMCI
    --add-exports=jdk.internal.vm.ci/jdk.vm.ci.runtime=ALL-UNNAMED
    --add-exports=jdk.internal.vm.ci/jdk.vm.ci.meta=ALL-UNNAMED
    --add-exports=jdk.internal.vm.ci/jdk.vm.ci.hotspot=ALL-UNNAMED

  This namespace's jdk.vm.ci imports will not COMPILE on a stock JVM, so it
  is source-only — required on demand on Graal, never in the AOT jar. That
  is honest capability-gating: the code exists where the runtime does.

  Profiling data is collected by the C1 profiling tiers, so a method must be
  WARM (driven thousands of times) before its profile is populated — the
  caller drives it, this reads it. Same shape as `perf.code/native`: you ran
  it, now inspect what the machine recorded."
  (:import [jdk.vm.ci.runtime JVMCI]
           [jdk.vm.ci.meta MetaAccessProvider ProfilingInfo JavaTypeProfile
            JavaTypeProfile$ProfiledType TriState]))

(defn- meta-access ^MetaAccessProvider []
  (-> (JVMCI/getRuntime) .getHostJVMCIBackend .getMetaAccess))

(defn- fn-methods
  "The invoke / invokeStatic reflect-methods carrying FN's compiled body."
  [f]
  (->> (.getDeclaredMethods (class f))
       (filter #(#{"invoke" "invokeStatic" "invokePrim"} (.getName ^java.lang.reflect.Method %)))))

(defn- tri->kw [^TriState t]
  (condp = t TriState/TRUE :yes, TriState/FALSE :no, :unknown))

(defn profile
  "The JIT's recorded profile of FN's body — one entry per bytecode index
  that carries data. For each: execution count, branch-taken probability (if
  it is a branch), and the OBSERVED concrete types with their probabilities
  (if it is a virtual call / typecheck). Empty until FN is hot; drive it
  first.

    (dotimes [_ 200000] (my-fn x))   ; warm it
    (perf.jvmci/profile my-fn)        ; read what the JIT saw"
  [f]
  (let [ma (meta-access)]
    (vec
     (for [^java.lang.reflect.Method jm (fn-methods f)
           :let [rm (.lookupJavaMethod ma jm)
                 pi (.getProfilingInfo rm)
                 n  (.getCodeSize rm)]
           bci (range n)
           :let [ec (try (.getExecutionCount pi bci) (catch Throwable _ 0))
                 bp (try (.getBranchTakenProbability pi bci) (catch Throwable _ -1.0))
                 tp (try (.getTypeProfile pi bci) (catch Throwable _ nil))]
           :when (or (pos? ec) (>= bp 0.0) tp)]
       (cond-> #:perf.jvmci{:method (.getName jm) :bci bci}
         (pos? ec)   (assoc :perf.jvmci/count ec)
         (>= bp 0.0) (assoc :perf.jvmci/branch-taken-prob bp)
         tp          (assoc :perf.jvmci/types
                            (-> (vec (for [^JavaTypeProfile$ProfiledType pt (.getTypes ^JavaTypeProfile tp)]
                                       [(.toJavaName (.getType pt)) (.getProbability pt)]))
                                (cond-> (pos? (.getNotRecordedProbability ^JavaTypeProfile tp))
                                  (conj [:other (.getNotRecordedProbability ^JavaTypeProfile tp)])))))))))

(defn available?
  "Is JVMCI reachable here? False on a stock JVM or without the exports."
  []
  (try (some? (meta-access)) (catch Throwable _ false)))
