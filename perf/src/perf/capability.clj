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
  "capability -> {:available? fn :needs str :remedy str :alias str?}

  :alias is the ONE alias that supplies the capability, or nil when no
  alias can (a JDK feature, or a file on disk). `report` folds the aliases
  of everything missing into a single launch line — three missing
  capabilities used to mean three remedy strings for the reader to merge
  by hand, which is a step a machine can take and a reader will get wrong."
  {:jfr        {:available? #(class-present? "jdk.jfr.consumer.RecordingStream")
                :needs "jdk.jfr"
                :remedy "present on any modern JDK"
                :alias nil}
   :jdi        {:available? #(and (class-present? "com.sun.jdi.Bootstrap")
                                  (some? (prop "jdwp.port")))
                :needs "jdwp agent at startup"
                :remedy "clj -M:debug:cider  (JDI cannot attach to a running VM)"
                :alias "debug"}
   ;; The JDK's own rule, from HotSpotVirtualMachine's static init, is
   ;;   ALLOW_ATTACH_SELF = "".equals(s) || Boolean.parseBoolean(s)
   ;; so a BARE -Djdk.attach.allowAttachSelf (property = "") enables it.
   ;; Testing (= "true" s) reported self-attach missing on a JVM where the
   ;; attach demonstrably succeeded, and require! then refused mem/ —
   ;; a false negative that blocks a working capability. Match the JDK.
   :self-attach {:available? #(when-let [s (prop "jdk.attach.allowAttachSelf")]
                                (or (= "" s) (Boolean/parseBoolean s)))
                 :needs "-Djdk.attach.allowAttachSelf (bare, or =true)"
                 :remedy "clj -M:perf  — required by mem/ and by nREPL's hard interrupt"
                 :alias "perf"}
   :nmt        {:available? #(jvm-arg? #"NativeMemoryTracking")
                :needs "-XX:NativeMemoryTracking=summary"
                :remedy "clj -M:perf"
                :alias "perf"}
   :debug-nonsafepoints
               {:available? #(jvm-arg? #"DebugNonSafepoints")
                :needs "-XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints"
                :remedy "clj -M:perf — without it profiler frames land on wrong lines"
                :alias "perf"}
   :hsdis      {:available? #(.exists (java.io.File. (str (prop "user.home")
                                                          "/.clojure/dev/lib/hsdis-amd64.so")))
                :needs "hsdis-amd64.so"
                :remedy "built against capstone; used by bin/clj-asm"
                :alias nil}
   :ffm        {:available? #(class-present? "java.lang.foreign.Linker")
                :needs "JDK 22+"
                :remedy "present"
                :alias nil}})

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

;; ── stale build ───────────────────────────────────────────────────
;;
;; perf ships AOT-compiled: the classpath carries perf.jar, not perf/src.
;; So an edit to the source changes NOTHING until build.sh runs, and the
;; failure is silent — the fix is there on disk, the running image is the
;; old one, and every measurement you take to check agrees with the old
;; one. Cost, observed: one debugging cycle spent on a fix that had
;; already been written.
;;
;; build.sh knows this and says so in a comment. A comment in a build
;; script is not where a running REPL can read it.

(def ^:private dev-root (str (prop "user.home") "/.clojure/dev"))

(defn stale
  "Source files newer than the jar that shadows them. Empty is good.
  Cheap — a directory walk over two small trees, and only ever run when
  someone asks what works here."
  []
  (vec (for [[jar src] [["perf.jar" "perf/src"] ["perf-ext.jar" "ext/src"]]
             :let [j (java.io.File. dev-root ^String jar)]
             :when (.exists j)
             ^java.io.File f (file-seq (java.io.File. dev-root ^String src))
             :when (and (.isFile f)
                        (str/ends-with? (.getName f) ".clj")
                        (> (.lastModified f) (.lastModified j)))]
         (.getPath f))))

(defn launch
  "ONE command line supplying every capability currently missing that an
  alias can supply. nil when nothing missing is alias-fixable — a JDK
  feature or a file on disk cannot be fixed by relaunching, and saying so
  beats printing a command that would not help."
  []
  (let [as (->> (keys registry)
                (remove available?)
                (keep #(:alias (registry %)))
                distinct
                sort)]
    (when (seq as)
      (str "clojure -M:" (str/join ":" as)))))

(defn report
  "Every capability, with remedies for the missing ones, the one launch
  line that would fix the fixable ones, and whether the running image is
  behind the source on disk."
  []
  (let [host (cond (prop "babashka.version") :babashka
                   (= "runtime" (prop "org.graalvm.nativeimage.imagecode")) :native-image
                   :else :jvm)
        stale-files (stale)]
    (cond-> {:host host
             :version (or (prop "babashka.version") (prop "java.version"))
             :available (vec (sort (filter available? (keys registry))))
             :missing (into {} (for [c (keys registry) :when (not (available? c))]
                                 [c (:remedy (registry c))]))
             :note (case host
                     :babashka "SCI interprets — there is no emitted bytecode to inspect"
                     :native-image "closed world — no runtime class generation"
                     nil)}
      (launch) (assoc :launch (launch))
      (seq stale-files) (assoc :stale {:files stale-files
                                       :remedy (str dev-root "/build.sh")}))))
