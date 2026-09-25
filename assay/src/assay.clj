(ns assay
  "Cost-of-code tools for Clojure. Data in, data out.

    (require '[assay.capture :as capture] '[assay.query :as query])

    (def c (capture/start))         ; a value you hold
    ;; ... run a workload ...
    (query/allocation (capture/observations c))
    (capture/stop c)

  Or, for the REPL, the two-liner:

    (def c (assay/record))           ; ... workload ...
    (assay/dashboard c)

  DESIGN
    assay.model      the fact schema, plus datafy/nav
    assay.capability what works here and why not — one registry
    assay.capture    start -> value; three mechanisms (stream/poll/census)
    assay.emit       assay's own numbers written INTO the JFR stream
    assay.query      pure fns over observations — testable with literals
    assay.code       the ladder: expand -> notes -> java -> bytecode -> native
    assay.diagnose   structured diagnostics with machine-applicable fixes
    assay.control    things that CHANGE execution — trace, restarts
    assay.native     FFM: prevent / recover / isolate

  This namespace deliberately does NOT re-export the others. Aliasing
  (def allocation query/allocation) complects the namespace graph and
  breaks M-. — require what you need.

  BROWSING: observations are Navigable. `(clojure.datafy/nav o :site v)`
  yields the var; browsers that speak datafy/nav — Portal, Reveal, REBL —
  render the whole graph without knowing this library exists."
  (:require [assay.capture :as capture]
            [assay.query :as query]
            [assay.code]
            [assay.watch]
            [assay.measure :as measure]
            [assay.capability :as capability]))

;; `record`, not `watch`. assay.watch/watch! collects COMPILER NOTES and
;; this starts an ALLOCATION RECORDER — two unrelated operations one `!`
;; apart, both reachable from a single (require '[assay :as assay]
;; '[assay.watch :as watch]). The name here follows what it returns, which
;; the docstring already said.
(defn record
  "Start recording. Returns a RECORDER — a live handle, Closeable.
  `assay.capture/snapshot` turns what it has seen into a value."
  ([] (capture/start))
  ([opts] (capture/start opts)))

;; `dashboard`, not `summary`, for the same reason: assay.explain/summary
;; renders the four oracles for a FORM, this renders a capture. Same word,
;; unrelated subjects.
(defn dashboard
  "The dashboard for a RECORDER or a SNAPSHOT. Taking a snapshot first
  means the same fn works on data that arrived over a socket."
  [r-or-snap]
  (let [snap (if (:assay/observations r-or-snap) r-or-snap (capture/snapshot r-or-snap))]
    (cond-> (assoc (query/summary (:assay/observations snap)
                                  (:assay/samples snap)
                                  (:assay/period-ms snap))
                   :assay/host (:host (capability/report)))
      ;; surface partial coverage — the facade used to omit this while
      ;; assay.repl/summary added it, so the recommended non-REPL entry
      ;; point silently reported dropped-ring totals as if complete.
      (pos? (:assay/dropped snap 0))
      (assoc :assay/dropped (:assay/dropped snap)
             :assay/coverage :assay.coverage/partial))))

(defn capabilities [] (capability/report))

;; ── Point measurements ────────────────────────────────────────────
;;
;; Functions taking thunks, not macros. v2 made these macros purely to
;; delay evaluation, which is not a reason to reach for a macro — it is
;; what a function argument already does. Editors wrap the form in
;; (fn [] ...) for you, so nothing is lost at the keyboard.

(defn allocated
  "EXACT bytes allocated by calling F — not sampled. See `assay.measure/allocated`."
  [f]
  (measure/allocated f))

(def ^:private resolve! assay.code/resolve!)

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
  "Deep size of VAL. Needs -M:assay (attach agent).

  Careful what you hand it: `doall` returns its ARGUMENT, so
  (deep-size (doall (range 1000))) measures an unrealized LongRange at
  56 B, not a realized seq at 62.5 KiB."
  [val]
  (capability/require! :self-attach)
  ((resolve! 'clj-memory-meter.core/measure) val))

;; ── The ladder ────────────────────────────────────────────────────
;;
;; These moved to assay.code, which is the whole ladder rather than two
;; rungs of it: expand -> notes -> java -> bytecode -> native. They stay
;; here as one-line delegations because they are the two people reach for
;; most, and macros cannot be re-exported by def — the form has to survive
;; unevaluated, which is exactly why they are macros.

(defmacro java
  "The Java the Clojure compiler emitted — where reflection and boxing
  become visible. See `assay.code/java`."
  [form]
  `(assay.code/java ~form))

(defmacro bytecode
  "JVM bytecode. See `assay.code/bytecode`."
  [form]
  `(assay.code/bytecode ~form))

(defmacro notes
  "What the compiler could not do, as data, ranked by mechanism —
  Clojure's answer to SBCL's efficiency notes. See `assay.code/notes`.
  A note carries no cost number; `measure/weigh` supplies the live factor.

    (assay/notes '(defn f [s] (.length s)))
    ;; => [#:assay.note{:code :assay.note/reflection :span {...} :message ..}]"
  [form]
  `(assay.code/notes ~form))

(defn muffle!
  "Drop note codes from every reporting path — SBCL's *muffled-warnings*.
  Reachable here because it is only worth having where `watch!` is: once
  notes arrive on every compile, suppressing the ones you have judged is
  what keeps the rest readable. See `assay.code/muffle!`.

    (assay/muffle! #{:assay.note/boxed-math})   ; session-wide
    (assay/muffle!)                            ; clear"
  ([] (assay.code/muffle!))
  ([codes] (assay.code/muffle! codes)))

;; The ladder's ACTIONABLE end. `notes` says what the compiler refused;
;; these two say what to write instead and whether it actually helped —
;; and they lived in assay.measure, reachable only if you already knew the
;; namespace. The most valuable verbs were the least findable. Delegated
;; on the same ground as `notes` and `java`: the ones people reach for.
;;
;; They stay in assay.measure because that is where BLAST RADIUS puts them
;; — both RUN your code — and the delegation does not move them.

(defn weigh
  "The form as written against the form the compiler wanted, measured on
  ARGS. `:verified` is the load-bearing key: true only when the rewrite
  actually silenced the notes. See `assay.measure/weigh`.

    (assay/weigh '(fn [a b] (+ a b)) [3 4])"
  ([form args] (measure/weigh form args))
  ([form args opts] (measure/weigh form args opts)))

(defn fix
  "The verified rewrite, as source you can paste — rustc suggests, cargo
  fix applies, this hands you the text. See `assay.measure/fix`.

    (assay/fix '(fn [a b] (+ a b)) [3 4])"
  ([form args] (measure/fix form args))
  ([form args opts] (measure/fix form args opts)))
