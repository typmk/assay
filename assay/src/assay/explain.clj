(ns assay.explain
  "One subject, four oracles — the spine the rest of assay is a projection of.

  assay grew as a ladder of ~25 commands because Clojure forced it onto the
  measurement path where SBCL gets everything from one compile. Underneath,
  every one of those commands is one of four questions asked of a form:

    STRUCTURE  what the compiler DID          (assay.code/notes + types)
    COST       what it costs                  (assay.measure — time + bytes)
    OUTCOME    what it computes, and better    (assay.forms — prove -> sample)
    TYPE       what's known about the values   (compiler types, + Graal
                                                static Stamps + JVMCI runtime
                                                profile when on GraalVM)

  `explain` asks all four and returns one rank-tagged map. Every oracle is
  wrapped so a missing capability (no Graal, no sample args) reports itself
  as :assay.rank/unavailable with a reason — a missing reading never looks
  like an empty one. `:assay.explain/blind?` is where two oracles COMPOSE:
  true when STRUCTURE emitted no note yet OUTCOME found a proven-equivalent
  rewrite that allocates less — an AVOIDABLE cost the compiler's advisory
  channel missed. Not 'it allocates' (a vector you asked to build allocates,
  and SBCL would not warn either) but 'a cheaper equivalent exists and the
  compiler stayed silent'. The honest form of the Clojure-vs-SBCL gap.

  RANK VOCAB (centralised here — it was scattered across code/measure/forms):
    :assay.rank/proven      rank 1, algebraic canonicalisation decided it
    :assay.rank/measured    rank 1, a live measurement now
    :assay.rank/observed    rank 2, sampled over a pool
    :assay.rank/read        rank 3-4, parsed from the compiler / a static graph
    :assay.rank/mixed       per-row rank varies (see the rows)
    :assay.rank/unavailable the oracle could not run — reason attached"
  (:require [assay.code :as code]
            [assay.forms :as forms]
            [assay.measure :as measure]))

;; ── the four oracles: each (form args opts) -> a reading map ──────────
(defn- structure-oracle [form _args _opts]
  {:assay.explain/rank :assay.rank/read
   :assay.explain/data (code/notes* form)})

(defn- type-oracle [form args _opts]
  ;; Always: the compiler's own type conclusions (:assay.types/unresolved =
  ;; the Object positions, the boxing story). Enriched, on GraalVM, with the
  ;; static Stamp lattice and the runtime profile — requiring-resolve throws
  ;; on a stock JVM (the jdk.graal/jdk.vm.ci imports won't compile), so the
  ;; form is only eval'd (once, via the delay) where those actually load, and
  ;; an EMPTY reading is dropped rather than attached as a hollow one (a
  ;; profile read off a never-driven class is always empty — that is
  ;; :unavailable, not a finding; driving it hot first is the deferred fix).
  (let [compiler (code/types form args)
        ;; jvmci FIRST: the runtime profile is empty off a never-driven class,
        ;; so drive the fn hot (C1 profiling tiers populate it) then read. A
        ;; CONSTANT input profiles nothing (one path, constant-folded), so
        ;; numeric args are VARIED per iteration to exercise the branches.
        ;; MUST run before graal below: building a StructuredGraph initialises
        ;; the Graal runtime and flips JIT state that suppresses this profile
        ;; (measured — reading jvmci after graal in-process returns empty).
        ;; This is the observed-types signal SBCL (AOT) cannot have.
        jvmci (try (when-let [p (requiring-resolve 'assay.jvmci/profile)]
                     (when (seq args)
                       (let [g (eval form)]
                         (dotimes [i 200000]
                           (apply g (mapv (fn [a] (if (number? a) (+ a (rem i 32)) a)) args)))
                         (when-let [pr (seq (p g))] (vec pr)))))
                   (catch Throwable _ nil))
        graal (try (when-let [g (requiring-resolve 'assay.graal/stamps-of)]
                     (when-let [s (seq (g (eval form)))] (vec s)))
                   (catch Throwable _ nil))]
    {:assay.explain/rank :assay.rank/read
     :assay.explain/data (cond-> {:compiler compiler}
                          graal (assoc :graal-stamps graal)
                          jvmci (assoc :jvmci-profile jvmci))}))

(defn- cost-oracle [form args {:keys [quick?]}]
  (when (and (nil? args) (seq (:params (measure/fn-parts form))))
    (throw (ex-info "needs sample args to price the form" {:reason :needs-args})))
  {:assay.explain/rank :assay.rank/measured
   :assay.explain/data (measure/cost form args {:quick? quick?})})

(defn- outcome-oracle [form args _opts]
  (when (nil? args)
    (throw (ex-info "needs sample args to rank alternative writings" {:reason :needs-args})))
  {:assay.explain/rank :assay.rank/mixed
   :assay.explain/data (forms/discover form args)})

(def ^:private oracles
  "The spine. Order is the reading order: what the compiler did, what it
  costs, what's known about the values, what it computes and how else."
  [[:assay.explain/structure structure-oracle]
   [:assay.explain/cost      cost-oracle]
   [:assay.explain/type      type-oracle]
   [:assay.explain/outcome   outcome-oracle]])

(defn- run-oracle [oracle form args opts]
  (try (oracle form args opts)
       (catch Throwable e
         {:assay.explain/rank :assay.rank/unavailable
          :assay.explain/reason (or (ex-message e) (str e))})))

(defn- structure-silent?
  "STRUCTURE emitted no note AND actually ran (a crashed oracle is not
  silence). Only a real, empty compiler advisory counts."
  [reading]
  (and (= :assay.rank/read (:assay.explain/rank reading))
       (let [notes (:assay.explain/data reading)]
         (or (nil? notes) (zero? (count notes))))))

(defn- byte-saving-rewrite
  "The single most byte-saving proven-equivalent rewrite OUTCOME found, or nil.
  The actionable core the two oracles COMPOSE to produce: a form the compiler
  said nothing about, and the exact cheaper equivalent + how many bytes it
  saves. This is what turns blind? from a verdict into a remedy."
  [outcome-reading]
  (->> (:assay.explain/data outcome-reading)
       (filter (fn [row] (and (= :assay.forms/cheaper-synonym (:assay.forms/kind row))
                              (pos? (or (:assay.forms/bytes-saved row) 0)))))
       (sort-by (comp - :assay.forms/bytes-saved))
       first))

(defn explain
  "Ask all four oracles of FORM (a fn form) and return one rank-tagged map:
  {:assay.explain/subject :assay.explain/structure :assay.explain/cost
   :assay.explain/type :assay.explain/outcome :assay.explain/blind?}. ARGS are
  the sample call the cost/outcome oracles need; omit them and those two
  report :assay.rank/unavailable :needs-args rather than guess.

  `:assay.explain/blind?` is the payoff AND the one place two oracles compose:
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
         structure (:assay.explain/structure readings)
         outcome (:assay.explain/outcome readings)
         remedy (when (= :assay.rank/mixed (:assay.explain/rank outcome))
                  (byte-saving-rewrite outcome))
         blind? (and (structure-silent? structure) (some? remedy))]
     (cond-> (assoc readings
                    :assay.explain/subject form
                    :assay.explain/blind? blind?)
       remedy (assoc :assay.explain/remedy
                     {:assay.explain/rewrite (:assay.forms/candidate remedy)
                      :assay.explain/bytes-saved (:assay.forms/bytes-saved remedy)})))))

(defn summary
  "The always-on eldoc glance: STRUCTURE (compiler notes) + the compiler's own
  TYPE view, straight from assay.code — no benchmarking, no discover, no Graal.
  Compile-only when ARGS are absent (the usual eldoc case); with ARGS it also
  calls the fn once for the actual types. Cheap enough (~3 ms) to run where the
  arglist already shows. COST, OUTCOME and the blind? verdict need measurement
  and live in `explain`, which the user invokes. Returns short strings."
  ([form] (summary form nil))
  ([form args]
   (let [notes (try (code/notes* form) (catch Throwable _ nil))
         typ   (try (code/types form args) (catch Throwable _ nil))
         unres (count (:assay.types/unresolved typ))]
     (cond-> []
       (seq notes) (conj (format "structure: %d note(s)" (count notes)))
       (pos? unres) (conj (format "type: %d unresolved (Object)" unres))
       (and (empty? notes) (zero? unres))
       (conj "clean: compiler took the fast path (cost still worth checking)")))))
