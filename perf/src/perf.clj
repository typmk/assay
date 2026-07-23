(ns perf
  "Cost-of-code tools for Clojure. Data in, data out.

    (require '[perf.capture :as capture] '[perf.query :as query])

    (def c (capture/start))         ; a value you hold
    ;; ... run a workload ...
    (query/allocation (capture/observations c))
    (capture/stop c)

  Or, for the REPL, the two-liner:

    (def c (perf/watch))            ; ... workload ...
    (perf/summary c)

  DESIGN
    perf.model      the fact schema, plus datafy/nav
    perf.capability what works here and why not — one registry
    perf.capture    start -> value; three mechanisms (stream/poll/census)
    perf.query      pure fns over observations — testable with literals
    perf.code       the ladder: expand -> notes -> java -> bytecode -> native
    perf.diagnose   structured diagnostics with machine-applicable fixes
    perf.control    things that CHANGE execution — trace, restarts
    perf.native     FFM: prevent / recover / isolate

  This namespace deliberately does NOT re-export the others. Aliasing
  (def allocation query/allocation) complects the namespace graph and
  breaks M-. — require what you need.

  BROWSING: observations are Navigable. `(clojure.datafy/nav o :site v)`
  yields the var; browsers that speak datafy/nav — Portal, Reveal, REBL —
  render the whole graph without knowing this library exists."
  (:require [perf.capture :as capture]
            [perf.query :as query]
            [perf.code]
            [perf.capability :as capability]))

(defn watch
  "Start recording. Returns a RECORDER — a live handle, Closeable.
  `perf.capture/snapshot` turns what it has seen into a value."
  ([] (capture/start))
  ([opts] (capture/start opts)))

(defn summary
  "The dashboard for a RECORDER or a SNAPSHOT. Taking a snapshot first
  means the same fn works on data that arrived over a socket."
  [r-or-snap]
  (let [snap (if (:perf/observations r-or-snap) r-or-snap (capture/snapshot r-or-snap))]
    (assoc (query/summary (:perf/observations snap)
                          (:perf/samples snap)
                          (:perf/period-ms snap))
           :perf/host (:host (capability/report)))))

(defn capabilities [] (capability/report))

;; ── Point measurements ────────────────────────────────────────────
;;
;; Functions taking thunks, not macros. v2 made these macros purely to
;; delay evaluation, which is not a reason to reach for a macro — it is
;; what a function argument already does. Editors wrap the form in
;; (fn [] ...) for you, so nothing is lost at the keyboard.

(defn allocated
  "EXACT bytes allocated by calling F — not sampled.
  Warm it first: a cold run allocates for classloading and JIT."
  [f]
  (let [tmx ^com.sun.management.ThreadMXBean
        (java.lang.management.ManagementFactory/getThreadMXBean)
        b (.getCurrentThreadAllocatedBytes tmx)]
    (f)
    (- (.getCurrentThreadAllocatedBytes tmx) b)))

(defn- resolve! [sym]
  (or (try (requiring-resolve sym) (catch Exception _ nil))
      (throw (ex-info (str "could not resolve " sym) {:sym sym}))))

(defn mean-ns
  "Mean nanoseconds for F, JIT-warmed. `time` measures the interpreter
  plus compilation, not your code."
  [f]
  (* 1e9 (first (:mean ((resolve! 'criterium.core/quick-benchmark*) f {})))))

(defn bench
  "Full criterium report for F."
  [f]
  ((resolve! 'criterium.core/report-result)
   ((resolve! 'criterium.core/quick-benchmark*) f {})))

(defn flames
  "Flamegraph of F. EVENT is :alloc (start here — in Clojure the cost is
  usually garbage), :cpu, or :wall (the only one that shows BLOCKED time)."
  [event f]
  ((resolve! 'clj-async-profiler.core/profile*) {:event event} f))

(defn deep-size
  "Deep size of VAL. Needs -M:perf (attach agent).

  Careful what you hand it: `doall` returns its ARGUMENT, so
  (deep-size (doall (range 1000))) measures an unrealized LongRange at
  56 B, not a realized seq at 62.5 KiB."
  [val]
  (capability/require! :self-attach)
  ((resolve! 'clj-memory-meter.core/measure) val))

;; ── The ladder ────────────────────────────────────────────────────
;;
;; These moved to perf.code, which is the whole ladder rather than two
;; rungs of it: expand -> notes -> java -> bytecode -> native. They stay
;; here as one-line delegations because they are the two people reach for
;; most, and macros cannot be re-exported by def — the form has to survive
;; unevaluated, which is exactly why they are macros.

(defmacro java
  "The Java the Clojure compiler emitted — where reflection and boxing
  become visible. See `perf.code/java`."
  [form]
  `(perf.code/java ~form))

(defmacro bytecode
  "JVM bytecode. See `perf.code/bytecode`."
  [form]
  `(perf.code/bytecode ~form))

(defmacro notes
  "What the compiler could not do, as data, ranked by measured cost —
  Clojure's answer to SBCL's efficiency notes. See `perf.code/notes`.

    (perf/notes '(defn f [s] (.length s)))
    ;; => [#:perf.note{:code :perf.note/reflection :cost 202 ...}]"
  [form]
  `(perf.code/notes ~form))
