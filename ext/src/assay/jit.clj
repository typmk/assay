(ns assay.jit
  "Observe AND control HotSpot's JIT on a RUNNING VM.

  assay.code shows what the Clojure compiler did (notes, bytecode, native).
  This is the layer below: what the JVM's JIT is doing to that bytecode
  right now, and how to steer it — the JIT-decision surface SBCL has no
  analog for, because SBCL is AOT and its compiler is done before the
  program runs. HotSpot compiles continuously, and every decision it makes
  — which tier, inline or not, compile or stay interpreted — is both
  readable and controllable live, through the DiagnosticCommand MBean (the
  jcmd Compiler.* commands), no restart, no flags.

    (queue)         ; methods waiting to be JIT-compiled, right now
    (codelist)      ; every compiled method + its tier (1-4) and address
    (directives)    ; the JIT control directives in effect
    (steer! ...)    ; ADD a directive — force/block compile or inline
    (clear!)        ; drop the directives you added

  OBSERVE is harmless; STEER changes how your code is compiled for the
  rest of the process. It is real control over the compiler, so it is in
  the ext (blast-radius) tier."
  (:require [clojure.string :as str]))

(def ^:private ^javax.management.ObjectName dcmd
  (javax.management.ObjectName. "com.sun.management:type=DiagnosticCommand"))

(defn- srv [] (java.lang.management.ManagementFactory/getPlatformMBeanServer))

(defn- run
  "Invoke a DiagnosticCommand op (a jcmd Compiler.* command). ARGS are the
  string arguments the command takes, or none."
  ([op] (run op nil))
  ([op args]
   (let [^javax.management.MBeanServer s (srv)]
     (if args
       (.invoke s dcmd (name op)
                (into-array Object [(into-array String args)])
                (into-array String ["[Ljava.lang.String;"]))
       (.invoke s dcmd (name op) (make-array Object 0) (make-array String 0))))))

(defn queue
  "Methods currently queued for JIT compilation — the compiler's in-tray.
  A window that does not exist at all in an AOT compiler."
  []
  (->> (str/split-lines (str (run :compilerQueue)))
       (remove str/blank?)
       vec))

(defn codelist
  "Every method the JIT has compiled, with its compile id and tier.
  Tier 1-3 are C1 (increasing profiling), tier 4 is C2, tier 0 is
  interpreted/OSR — so this is the live map of what is hot and how hard it
  was optimised.

  The line format is HotSpot's own: `<id> <tier> <flags> <method-sig>
  [<addr-range>]`, and the method is dot-separated Java
  (java.lang.Byte.toUnsignedInt(B)I), NOT ns/name. An earlier regex
  assumed ns/name and SILENTLY DROPPED 30% of the list — a partial window
  passed off as whole, the exact silent-omission this toolkit exists to
  refuse. Now every line becomes a structured map, or, if it will not
  parse, passes through as {:assay.jit/raw line}. Nothing vanishes: the
  fetch is complete, even the parts we could not structure."
  []
  (->> (str/split-lines (str (run :compilerCodelist)))
       (remove str/blank?)
       (mapv (fn [l]
               (if-let [[_ id tier method]
                        (re-find #"^\s*(\d+)\s+(-?\d+)\s+\S+\s+(.+?)(?:\s+\[|$)" l)]
                 #:assay.jit{:id (parse-long id)
                            :tier (parse-long tier)
                            :method (str/trim method)}
                 #:assay.jit{:raw l})))))

(defn directives
  "The JIT control directives currently in effect (what steer! added,
  plus the default). This is the compiler's own account of the rules it
  is following."
  []
  (str (run :compilerDirectivesPrint)))

(defn steer!
  "ADD a JIT directive — control the compiler's decision for a method
  pattern. DIRECTIVE is the directives-string HotSpot accepts, e.g.

    (steer! \"[{ match: \\\"*::hot*\\\", Inline: \\\"+*::helper*\\\" }]\")
    (steer! \"[{ match: \\\"my.ns/slow\\\", Exclude: true }]\")  ; never compile

  Standard keys: Inline (+force/-block a callee), Exclude (never compile),
  DontInline, BreakAtExecute, PrintAssembly. Applies for the rest of the
  process; drop with clear!. This is compile-time control done at RUNTIME,
  which is the thing an AOT compiler cannot offer.

  HotSpot reads directives from a FILE or an inline string; this writes a
  temp file because the MBean's add takes a filename."
  [directive-str]
  (let [f (java.io.File/createTempFile "assay-jit" ".txt")]
    (spit f directive-str)
    (try (str (run :compilerDirectivesAdd [(.getAbsolutePath f)]))
         (finally (.delete f)))))

(defn clear!
  "Drop the directives steer! added, back to the JIT's default policy."
  []
  (str (run :compilerDirectivesClear)))

;; ── timeline: the JIT's compilation history, not its snapshot ──────
;; codelist is the CURRENT set of compiled methods. This is the SEQUENCE:
;; every compilation the JIT performed over a workload, with tier and
;; timestamp — the decisions as they happened. -XX:+LogCompilation is a
;; startup flag (like native's PrintAssembly), so a fresh JVM runs the
;; workload and we parse its emitted log.

(defn- unescape [s]
  (-> s (str/replace "&lt;" "<") (str/replace "&gt;" ">")
      (str/replace "&apos;" "'") (str/replace "&quot;" "\"") (str/replace "&amp;" "&")))

(defn- attr [tag k]
  (some-> (second (re-find (re-pattern (str k "='([^']*)'")) tag)) unescape))

(defn timeline
  "The JIT COMPILATION TIMELINE of a workload: each method the JIT compiled,
  in order, with its tier (1-4) and timestamp (seconds since VM start).
  Spawns a fresh JVM with -XX:+LogCompilation, evals DRIVER-FORM (a
  self-contained hot workload), and parses the log. codelist over TIME."
  ([] (timeline '(dotimes [i 3000000] (Math/sqrt (double (unchecked-inc i))))))
  ([driver-form]
   (let [log (java.io.File/createTempFile "assay-jit" ".log")
         java (str (System/getProperty "java.home") "/bin/java")
         ^java.util.List cmd [java "-XX:+UnlockDiagnosticVMOptions" "-XX:+LogCompilation"
                              (str "-XX:LogFile=" (.getAbsolutePath log))
                              "-cp" (System/getProperty "java.class.path")
                              "clojure.main" "-e" (pr-str driver-form)]
         p (.start (doto (ProcessBuilder. cmd) (.redirectErrorStream true)))]
     (when-not (.waitFor p 90 java.util.concurrent.TimeUnit/SECONDS)
       (.destroyForcibly p))
     (let [text (slurp log)]
       (.delete log)
       (->> (re-seq #"<nmethod\b[^>]*>" text)
            (keep (fn [tag]
                    (when-let [m (attr tag "method")]
                      #:assay.jit{:id (some-> (attr tag "compile_id") parse-long)
                                 :tier (some-> (attr tag "level") parse-long)
                                 :method (str/replace m #"\s+" " ")
                                 :stamp (some-> (attr tag "stamp") Double/parseDouble)})))
            (sort-by :assay.jit/stamp)
            vec)))))
