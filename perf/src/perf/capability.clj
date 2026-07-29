(ns perf.capability
  "What works here, and why not — declared in ONE place.

  v1 scattered this: `host` sniffed the runtime, `restarts!` checked for a
  jdwp port, `mem` failed with an opaque IOException when the attach flag
  was missing, `native` returned :nmt-off, and `jfr-trace!` silently
  produced nothing when armed the wrong way. Five ad-hoc checks with five
  failure shapes, so 'it returned nothing' never distinguished BROKEN from
  NOT-AVAILABLE from NOTHING-TO-SEE.

  A capability declares what it needs and how to get it. Absent is a
  first-class answer with a remedy attached."
  (:require [clojure.string :as str]))

(defn- prop [p] (System/getProperty p))

(defn- jvm-arg? [re]
  (some #(re-find re %)
        (.getInputArguments (java.lang.management.ManagementFactory/getRuntimeMXBean))))

(defn- class-present? [c]
  (try (Class/forName c) true (catch Throwable _ false)))

(def registry
  "capability -> {:available? fn :needs str :remedy str}"
  {:jfr        {:available? #(class-present? "jdk.jfr.consumer.RecordingStream")
                :needs "jdk.jfr"
                :remedy "present on any modern JDK"}
   :jdi        {:available? #(and (class-present? "com.sun.jdi.Bootstrap")
                                  (some? (prop "jdwp.port")))
                :needs "jdwp agent at startup"
                :remedy "clj -M:debug:cider  (JDI cannot attach to a running VM)"}
   ;; The JDK's own rule, from HotSpotVirtualMachine's static init, is
   ;;   ALLOW_ATTACH_SELF = "".equals(s) || Boolean.parseBoolean(s)
   ;; so a BARE -Djdk.attach.allowAttachSelf (property = "") enables it.
   ;; Testing (= "true" s) reported self-attach missing on a JVM where the
   ;; attach demonstrably succeeded, and require! then refused mem/ —
   ;; a false negative that blocks a working capability. Match the JDK.
   :self-attach {:available? #(when-let [s (prop "jdk.attach.allowAttachSelf")]
                                (or (= "" s) (Boolean/parseBoolean s)))
                 :needs "-Djdk.attach.allowAttachSelf (bare, or =true)"
                 :remedy "clj -M:perf  — required by mem/ and by nREPL's hard interrupt"}
   :nmt        {:available? #(jvm-arg? #"NativeMemoryTracking")
                :needs "-XX:NativeMemoryTracking=summary"
                :remedy "clj -M:perf"}
   :debug-nonsafepoints
               {:available? #(jvm-arg? #"DebugNonSafepoints")
                :needs "-XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints"
                :remedy "clj -M:perf — without it profiler frames land on wrong lines"}
   :hsdis      {:available? #(.exists (java.io.File. (str (prop "user.home")
                                                          "/.clojure/dev/lib/hsdis-amd64.so")))
                :needs "hsdis-amd64.so"
                :remedy "built against capstone; used by bin/clj-asm"}
   :ffm        {:available? #(class-present? "java.lang.foreign.Linker")
                :needs "JDK 22+"
                :remedy "present"}})

(defn available?
  "Is CAP usable right now?"
  [cap]
  (boolean (when-let [{:keys [available?]} (registry cap)]
             (try (available?) (catch Throwable _ false)))))

(defn require!
  "Throw with an ACTIONABLE message if CAP is missing.
  The point: never let a missing capability look like an empty result."
  [cap]
  (when-not (available? cap)
    (let [{:keys [needs remedy]} (registry cap)]
      (throw (ex-info (format "capability %s unavailable — needs %s. %s" cap needs remedy)
                      {:capability cap :needs needs :remedy remedy}))))
  true)

(defn report
  "Every capability, with remedies for the missing ones."
  []
  (let [host (cond (prop "babashka.version") :babashka
                   (= "runtime" (prop "org.graalvm.nativeimage.imagecode")) :native-image
                   :else :jvm)]
    {:host host
     :version (or (prop "babashka.version") (prop "java.version"))
     :available (vec (sort (filter available? (keys registry))))
     :missing (into {} (for [c (keys registry) :when (not (available? c))]
                         [c (:remedy (registry c))]))
     :note (case host
             :babashka "SCI interprets — there is no emitted bytecode to inspect"
             :native-image "closed world — no runtime class generation"
             nil)}))
