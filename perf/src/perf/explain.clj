(ns perf.explain
  "One subject, four oracles — the spine the rest of perf is a projection of.

  perf grew as a ladder of ~25 commands because Clojure forced it onto the
  measurement path where SBCL gets everything from one compile. Underneath,
  every one of those commands is one of four questions asked of a form:

    STRUCTURE  what the compiler DID          (perf.code/notes + types)
    COST       what it costs                  (perf.measure — time + bytes)
    OUTCOME    what it computes, and better    (perf.forms — prove -> sample)
    TYPE       what's known about the values   (compiler types, + Graal
                                                static Stamps + JVMCI runtime
                                                profile when on GraalVM)

  `explain` asks all four and returns one rank-tagged map. Every oracle is
  wrapped so a missing capability (no Graal, no sample args) reports itself
  as :perf.rank/unavailable with a reason — a missing reading never looks
  like an empty one. `:perf.explain/blind?` is where two oracles COMPOSE:
  true when STRUCTURE emitted no note yet OUTCOME found a proven-equivalent
  rewrite that allocates less — an AVOIDABLE cost the compiler's advisory
  channel missed. Not 'it allocates' (a vector you asked to build allocates,
  and SBCL would not warn either) but 'a cheaper equivalent exists and the
  compiler stayed silent'. The honest form of the Clojure-vs-SBCL gap.

  RANK VOCAB (centralised here — it was scattered across code/measure/forms):
    :perf.rank/proven      rank 1, algebraic canonicalisation decided it
    :perf.rank/measured    rank 1, a live measurement now
    :perf.rank/observed    rank 2, sampled over a pool
    :perf.rank/read        rank 3-4, parsed from the compiler / a static graph
    :perf.rank/mixed       per-row rank varies (see the rows)
    :perf.rank/unavailable the oracle could not run — reason attached"
  (:require [perf.code :as code]
            [perf.forms :as forms]
            [perf.measure :as measure]))

;; ── the four oracles: each (form args opts) -> a reading map ──────────
(defn- structure-oracle [form _args _opts]
  {:perf.explain/rank :perf.rank/read
   :perf.explain/data (code/notes* form)})

(defn- type-oracle [form args _opts]
  ;; Always: the compiler's own type conclusions (:perf.types/unresolved =
  ;; the Object positions, the boxing story). Enriched, on GraalVM, with the
  ;; static Stamp lattice and the runtime profile — requiring-resolve throws
  ;; on a stock JVM (the jdk.graal/jdk.vm.ci imports won't compile), so the
  ;; form is only eval'd (once, via the delay) where those actually load, and
  ;; an EMPTY reading is dropped rather than attached as a hollow one (a
  ;; profile read off a never-driven class is always empty — that is
  ;; :unavailable, not a finding; driving it hot first is the deferred fix).
  (let [compiler (code/types form args)
        f (delay (eval form))
        graal (try (when-let [g (requiring-resolve 'perf.graal/stamps-of)]
                     (when-let [s (seq (g @f))] (vec s)))
                   (catch Throwable _ nil))
        jvmci (try (when-let [p (requiring-resolve 'perf.jvmci/profile)]
                     (when-let [pr (seq (p @f))] (vec pr)))
                   (catch Throwable _ nil))]
    {:perf.explain/rank :perf.rank/read
     :perf.explain/data (cond-> {:compiler compiler}
                          graal (assoc :graal-stamps graal)
                          jvmci (assoc :jvmci-profile jvmci))}))

(defn- cost-oracle [form args {:keys [quick?]}]
  (when (and (nil? args) (seq (:params (measure/fn-parts form))))
    (throw (ex-info "needs sample args to price the form" {:reason :needs-args})))
  {:perf.explain/rank :perf.rank/measured
   :perf.explain/data (measure/cost form args {:quick? quick?})})

(defn- outcome-oracle [form args _opts]
  (when (nil? args)
    (throw (ex-info "needs sample args to rank alternative writings" {:reason :needs-args})))
  {:perf.explain/rank :perf.rank/mixed
   :perf.explain/data (forms/discover form args)})

(def ^:private oracles
  "The spine. Order is the reading order: what the compiler did, what it
  costs, what's known about the values, what it computes and how else."
  [[:perf.explain/structure structure-oracle]
   [:perf.explain/cost      cost-oracle]
   [:perf.explain/type      type-oracle]
   [:perf.explain/outcome   outcome-oracle]])

(defn- run-oracle [oracle form args opts]
  (try (oracle form args opts)
       (catch Throwable e
         {:perf.explain/rank :perf.rank/unavailable
          :perf.explain/reason (or (ex-message e) (str e))})))

(defn- structure-silent?
  "STRUCTURE emitted no note AND actually ran (a crashed oracle is not
  silence). Only a real, empty compiler advisory counts."
  [reading]
  (and (= :perf.rank/read (:perf.explain/rank reading))
       (let [notes (:perf.explain/data reading)]
         (or (nil? notes) (zero? (count notes))))))

(defn- avoidable-allocation?
  "Did OUTCOME find a VERIFIED cheaper writing that removes bytes? This is
  the sound core of blind?: not 'the form allocates' (harness noise, or a
  vector you asked to build), but 'a rewrite proven equivalent allocates
  less' — evidence the cost was avoidable and the compiler said nothing."
  [outcome-reading]
  (boolean
   (some (fn [row]
           (and (= :perf.forms/cheaper-synonym (:perf.forms/kind row))
                (pos? (or (:perf.forms/bytes-saved row) 0))))
         (:perf.explain/data outcome-reading))))

(defn explain
  "Ask all four oracles of FORM (a fn form) and return one rank-tagged map:
  {:perf.explain/subject :perf.explain/structure :perf.explain/cost
   :perf.explain/type :perf.explain/outcome :perf.explain/blind?}. ARGS are
  the sample call the cost/outcome oracles need; omit them and those two
  report :perf.rank/unavailable :needs-args rather than guess.

  `:perf.explain/blind?` is the payoff AND the one place two oracles compose:
  true when STRUCTURE emitted no note YET OUTCOME found a proven-equivalent
  rewrite that allocates less — an AVOIDABLE cost the compiler's own advisory
  channel missed. This is the honest form of the Clojure-vs-SBCL gap: not
  'it allocates' (a vector you asked to build allocates, and SBCL wouldn't
  warn either), but 'a cheaper equivalent exists and the compiler was silent'.
  Needs args (for OUTCOME); nil when either oracle could not run."
  ([form] (explain form nil {}))
  ([form args] (explain form args {}))
  ([form args opts]
   (let [readings (into {} (for [[k oracle] oracles]
                             [k (run-oracle oracle form args opts)]))
         structure (:perf.explain/structure readings)
         outcome (:perf.explain/outcome readings)
         blind? (and (structure-silent? structure)
                     (= :perf.rank/mixed (:perf.explain/rank outcome))
                     (avoidable-allocation? outcome))]
     (assoc readings
            :perf.explain/subject form
            :perf.explain/blind? blind?))))

(defn summary
  "The always-on eldoc glance: STRUCTURE (compiler notes) + the compiler's own
  TYPE view, straight from perf.code — no benchmarking, no discover, no Graal.
  Compile-only when ARGS are absent (the usual eldoc case); with ARGS it also
  calls the fn once for the actual types. Cheap enough (~3 ms) to run where the
  arglist already shows. COST, OUTCOME and the blind? verdict need measurement
  and live in `explain`, which the user invokes. Returns short strings."
  ([form] (summary form nil))
  ([form args]
   (let [notes (try (code/notes* form) (catch Throwable _ nil))
         typ   (try (code/types form args) (catch Throwable _ nil))
         unres (count (:perf.types/unresolved typ))]
     (cond-> []
       (seq notes) (conj (format "structure: %d note(s)" (count notes)))
       (pos? unres) (conj (format "type: %d unresolved (Object)" unres))
       (and (empty? notes) (zero? unres))
       (conj "clean: compiler took the fast path (cost still worth checking)")))))
