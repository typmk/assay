(ns perf.repl
  "Interactive convenience. The ONE place with a session global.

  A REPL session genuinely has a notion of 'the current capture' — that
  is state belonging to the session, not to the library. v2 got this
  backwards and put five singletons inside the library, which made
  queries untestable and meant two captures were impossible.

  So the statefulness lives here, named, in one atom, in a namespace
  whose whole purpose is to be interactive. `perf.capture` and
  `perf.query` stay pure and are what you use in code.

    (start!)      ; ... run a workload ...
    (summary)
    (allocation)
    (stop!)

  This is also what the editor client drives, so the editor never has to
  invent a place to keep the capture."
  (:require [perf.capture :as capture]
            [perf.query :as query]
            [perf.capability :as capability]))

(defonce ^:private session (atom nil))

;; Every mutation of `session` goes through this lock. start!/ensure! were
;; check-then-act ((when-let [c @session] (stop c)) (reset! ...)), so two
;; concurrent calls — reachable from concurrent nREPL ops, which all call
;; ensure! — each built a live Recorder (open JFR stream + daemon poll
;; thread) and the losers were leaked, never stopped. Starting a recorder
;; is a heavy side effect; it must be serialized, not raced.
(defonce ^:private session-lock (Object.))

(defn current
  "The current capture, or nil."
  [] @session)

(defn start!
  "Begin (or restart) the session capture."
  ([] (start! {}))
  ([opts]
   (locking session-lock
     (when-let [c @session] (capture/stop c))
     (reset! session (capture/start opts))
     @session)))

(defn stop! []
  (locking session-lock
    (when-let [c @session]
      (reset! session (capture/stop c)))
    @session))

(defn- ensure! []
  ;; double-checked under the lock: the fast path (already started) takes
  ;; no lock; only the start races serialize.
  (or @session
      (locking session-lock
        (or @session
            (do (reset! session (capture/start {})) @session)))))

(defn obs [] (capture/observations (ensure!)))

(defn snapshot
  "An immutable value of the session so far — pr-str it, ship it, diff it.
  This is what you send from a production REPL back to your laptop."
  [] (capture/snapshot (ensure!)))

(defn summary
  "Summary of the session, or of any SNAPSHOT — including one that
  travelled here from another machine."
  ([] (summary (snapshot)))
  ([snap]
   (cond-> (assoc (query/summary (:perf/observations snap)
                                 (:perf/samples snap)
                                 (:perf/period-ms snap))
                  :perf/host (:host (capability/report)))
     ;; If the ring dropped anything, every :perf/n below is a count over
     ;; the RETAINED window, not the run. Saying so is the difference
     ;; between a sample and a lie — the numbers are still ranked
     ;; correctly relative to each other, but they are not totals.
     (pos? (:perf/dropped snap 0))
     (assoc :perf/dropped (:perf/dropped snap)
            :perf/coverage :perf.coverage/partial))))

(defn allocation ([] (allocation 15)) ([n] (query/allocation (obs) n)))
(defn blocking   ([] (blocking 15))   ([n] (query/blocking (obs) n)))
(defn deopts     ([] (deopts 15))     ([n] (query/deopts (obs) n)))
(defn callers    ([f] (callers f 10)) ([f n] (query/callers (obs) f n)))
(defn rates      [] (let [c (ensure!)] (query/rates (capture/samples c) (capture/period c))))
(defn datoms     [] (query/datoms (obs)))

(defn census
  "Exact object census. Stop-the-world, so on demand only."
  ([] (census 20))
  ([n] (vec (take n (capture/census)))))

(defonce ^:private marks (atom {}))

(defn mark!
  "Record an exact census under LABEL, for a later `growth` comparison."
  ([] (mark! :last))
  ([label] (swap! marks assoc label (capture/census)) {:marked label}))

(defn growth
  "What GREW since `mark!` — the leak query. Exact, not sampled."
  ([] (growth :last 15))
  ([label n]
   (if-let [before (get @marks label)]
     (query/growth before (capture/census) n)
     :no-mark--call-perf.repl/mark!)))
