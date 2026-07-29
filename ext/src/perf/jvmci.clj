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
  (:require [perf.code])
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

;; ── notes from the JIT's profile ──────────────────────────────────
;;
;; MEASURED, this JVM: `profile` returns type data ONLY where the site has
;; a RECEIVER. A protocol call reported its three implementors at 33.3%
;; each; (.length s) over String and StringBuilder reported exactly the
;; 2:1 mix it was fed. A boxed (+ (* a b) a) driven 500k times reported
;; ZERO type entries — it compiles to invokestatic Numbers.add(Object,
;; Object), and a static call has no receiver to profile. Argument types
;; live in HotSpot's ParametersTypeData, which JVMCI's public
;; ProfilingInfo does not expose.
;;
;; So this does NOT recover "the compiler said Object, every call passed a
;; Long" — that remains `code/types` with sample args. What it does give,
;; and nothing else in the toolkit can, is the site the JIT gave up on.

;; TypeProfileWidth IS NOT THE THRESHOLD, and using it was a bug caught by
;; measurement: it is the RECORDING width — how many receiver types the
;; profiler will store — and it varies by VM. MEASURED: Red Hat OpenJDK 25
;; sets 2, GraalVM CE 25 sets 8. Dispatch behaviour does not differ between
;; them; only how much the profiler remembers does. Thresholding on it made
;; a site that is megamorphic on both report on neither.
;;
;; The real limit is structural and fixed: HotSpot's inline cache is
;; monomorphic (1 type), C2 will emit a bimorphic guard (2), and at 3 or
;; more it falls back to a vtable/itable lookup it cannot inline through.
;; That is the same class of claim as "reflection resolves by name on every
;; call" in perf.code/kind-order — a rank-3 statement about how the
;; mechanism works, not a magnitude.

(def ^:private inline-cache-limit
  "Receiver types HotSpot can still dispatch without a vtable lookup:
  1 monomorphic, 2 bimorphic. Above this the site is megamorphic."
  2)

(defn- type-profile-width
  "The VM's receiver-type RECORDING width — context, not threshold. When
  the observed count reaches it the profile may be truncated, which is why
  the note carries it: `3 of 8 recorded` and `8 of 8 recorded` are
  different claims about completeness."
  []
  (or (try (-> (java.lang.management.ManagementFactory/getPlatformMXBean
                com.sun.management.HotSpotDiagnosticMXBean)
               (.getVMOption "TypeProfileWidth")
               .getValue
               parse-long)
           (catch Throwable _ nil))
      inline-cache-limit))

(defn notes
  "Megamorphic call sites in FN, as notes — the JIT's own measurement,
  in the same shape perf.code emits, so it ranks, tiers and muffles
  identically. F must be WARM; an unprofiled fn correctly yields [].

  A site is reported when it observed MORE distinct receiver types than
  HotSpot can dispatch without a vtable lookup — see `inline-cache-limit`,
  which is NOT TypeProfileWidth.

    (dotimes [_ 500000] (f x))
    (perf.jvmci/notes f)"
  [f]
  (let [w (type-profile-width)]
    (perf.code/report
     (for [e (profile f)
           :let [ts (remove #(= :other (first %)) (:perf.jvmci/types e))
                 n  (count ts)]
           :when (> n inline-cache-limit)]
       #:perf.note{:code :perf.note/megamorphic
                   :severity (perf.code/severity :perf.note/megamorphic)
                   :span {:perf/file nil
                          :perf/line nil
                          :perf/col (:perf.jvmci/bci e)}
                   :message (format "%s receiver types at %s bci %s (%s of %s recorded%s)"
                                    n (:perf.jvmci/method e) (:perf.jvmci/bci e) n w
                                    (if (>= n w) "; profile may be truncated" ""))
                   ;; taken/refused, the same two keys the boxed-math note
                   ;; carries: what the JIT settled for, and the inlining
                   ;; it declined. Read off the profile, not remembered.
                   :taken "vtable/itable dispatch"
                   :refused ["monomorphic inline cache"]
                   :observed (vec (sort-by (comp - second) ts))}))))
