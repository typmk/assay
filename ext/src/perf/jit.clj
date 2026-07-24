(ns perf.jit
  "Observe AND control HotSpot's JIT on a RUNNING VM.

  perf.code shows what the Clojure compiler did (notes, bytecode, native).
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
  "Every method the JIT has compiled, with its tier and code address.
  Tier 1-3 are C1 (increasing profiling), tier 4 is C2 — so this is the
  live map of what is hot and how hard it was optimised."
  []
  (->> (str/split-lines (str (run :compilerCodelist)))
       (keep (fn [l]
               ;; "<addr> <level> <flags> <method> (<size> bytes)"
               (when-let [[_ addr level method]
                          (re-find #"^(\S+)\s+(\d+)\s+\S*\s+(\S+::\S+|\S+/\S+)" l)]
                 #:perf.jit{:address addr
                            :tier (parse-long level)
                            :method method})))
       vec))

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
  (let [f (java.io.File/createTempFile "perf-jit" ".txt")]
    (spit f directive-str)
    (try (str (run :compilerDirectivesAdd [(.getAbsolutePath f)]))
         (finally (.delete f)))))

(defn clear!
  "Drop the directives steer! added, back to the JIT's default policy."
  []
  (str (run :compilerDirectivesClear)))
