(ns perf.model
  "The fact schema. Qualified keys, because this data leaves the library.

  An observation:

    {:perf/kind   :perf.kind/alloc
     :perf/t      1784778161749
     :perf/thread \"main\"
     :perf/site   {:perf/fn \"user/allocs\" :perf/line 4}
     :perf/via    {:perf/fn \"clojure.core/vec\" :perf/line 379}
     :perf/stack  [{:perf/depth 0 :perf/fn ... :perf/line ...} ...]
     :perf.alloc/class \"java.lang.Long\"
     :perf.alloc/weight 12345}

  WHY QUALIFIED: v3 used :kind, :fn, :line, :n. The moment that map lands
  beside anyone else's, :fn and :line collide with everything and carry no
  provenance. Qualified keys are what makes data transportable between
  libraries — you can merge two systems' facts into one map and still know
  whose is whose.

  WHY FLAT: kind-specific values are top-level qualified keys rather than a
  nested :attrs map. Nesting to avoid collisions is what you do WITHOUT
  namespaces; with them the flat form is both simpler and Datomic-shaped.

  :perf/site vs :perf/via is load-bearing: a megamorphic Clojure call
  deopts inside clojure.lang.Var, and (vec ...) allocates inside
  clojure.core/vec. Both are true; neither is a line you can edit.

  Observations are Navigable — nav a site or a frame and you get the VAR.
  Portal, Reveal and REBL render that without knowing this library exists."
  (:require [clojure.string :as str]
            [clojure.datafy :as d]
            [clojure.core.protocols :as p]))

;; ── Schema, as data ───────────────────────────────────────────────

(def Frame
  [:map
   [:perf/depth :int]
   [:perf/fn :string]
   [:perf/line :int]])

(def Site
  [:map
   [:perf/fn {:optional true} :string]
   [:perf/line {:optional true} :int]])

(def Observation
  "One fact. Everything observable reduces to this."
  [:map
   [:perf/kind [:enum :perf.kind/alloc :perf.kind/block :perf.kind/deopt]]
   [:perf/t :int]
   [:perf/thread {:optional true} [:maybe :string]]
   [:perf/site Site]
   [:perf/via Site]
   [:perf/stack [:sequential Frame]]])

(defn validate
  "Validate DATA against SCHEMA if malli is present; nil means valid.
  Returns :malli-unavailable rather than pretending to have checked — a
  validator that silently passes is worse than none."
  [schema data]
  (if-let [explain (try (requiring-resolve 'malli.core/explain) (catch Throwable _ nil))]
    (explain schema data)
    :malli-unavailable))

;; ── Frames ────────────────────────────────────────────────────────

(def ^:private noise-prefixes
  ;; Note the SLASH forms: after demunging, perf$watch is "perf/watch", so
  ;; a "perf." check never matches and the profiler shows up in its own
  ;; output. Tooling namespaces are excluded for the same reason —
  ;; observing costs something, and charging that to the user is a lie.
  ["java." "jdk." "sun." "clojure." "perf." "perf/"
   "cider." "nrepl." "orchard." "criterium." "malli."])

;; ── name caches ───────────────────────────────────────────────────
;;
;; MEASURED 2026-07-23, 13179 alloc events over a 5s workload: normalising
;; one event cost 182 us, of which demunge alone was 90.8 — half the entire
;; cost of observing. Across 312,893 frames there were 59 DISTINCT names.
;; Class and method names come from a small set that stops growing almost
;; immediately, so both caches below are near-pure-hit after the first
;; millisecond.
;;
;; ConcurrentHashMap, not core/memoize: memoize wraps every call in a deref
;; plus a swap! on an atom holding a map, which is exactly the contended
;; write path being avoided. computeIfAbsent locks one bin.
;;
;; Bounded, because a JVM emitting genuinely unbounded names (lambda
;; spinning, heavy runtime codegen) would otherwise make the profiler the
;; leak it is meant to find. Past the cap both still return correct
;; answers, just uncached.
(def ^:private cache-max 50000)

(defn clojure-frame?
  "Does FN-NAME look like a demunged Clojure fn (ns/name)?
  Tests for `/`, not `$`: frames are demunged on the way in, so
  user$allocs is already user/allocs. Testing for `$` matched nothing and
  silently made two whole views return nil."
  [fn-name]
  (str/includes? (str fn-name) "/"))

;; A pure predicate over those same names, scanning 11 prefixes each time.
;; Caching collapses the whole classification to one hash lookup.
(def ^:private ^java.util.concurrent.ConcurrentHashMap own-cache
  (java.util.concurrent.ConcurrentHashMap. 512))

(def ^:private own-fn
  (reify java.util.function.Function
    (apply [_ s]
      (Boolean/valueOf
       (boolean (and (str/includes? ^String s "/")
                     (not (some #(str/starts-with? ^String s %) noise-prefixes))))))))

(defn own-frame? [fn-name]
  (let [s (str fn-name)]
    (if (< (.size own-cache) cache-max)
      (.booleanValue ^Boolean (.computeIfAbsent own-cache s own-fn))
      (and (clojure-frame? s)
           (not (some #(str/starts-with? s %) noise-prefixes))))))

(def ^:private ^java.util.concurrent.ConcurrentHashMap demunge-cache
  (java.util.concurrent.ConcurrentHashMap. 512))

(def ^:private demunge-fn
  (reify java.util.function.Function
    (apply [_ s] (clojure.lang.Compiler/demunge s))))

(defn demunge [cls]
  (let [s (str cls)]
    (if (< (.size demunge-cache) cache-max)
      (.computeIfAbsent demunge-cache s demunge-fn)
      (or (.get demunge-cache s) (clojure.lang.Compiler/demunge s)))))

(defn cache-stats
  "Occupancy of the name caches. Exposed because 'is the cache doing its
  job' is a question about THIS process, and the honest answer is a
  number — including when it says the cap has been hit."
  []
  #:perf.cache{:demunge (.size demunge-cache)
               :own (.size own-cache)
               :max cache-max})

(defn frames
  "Normalise a JFR stack into rows. One row per frame makes stacks a JOIN
  TARGET — 'fns in both allocation and blocking stacks' is a set
  intersection, not a bespoke report.

  A typed loop over the List with a transient, not map-indexed into vec.
  This runs once per frame per event — 23.7 frames/event measured — so the
  lazy seq, its chunk buffer and the boxed index were pure overhead on the
  hottest path in the library."
  [^jdk.jfr.consumer.RecordedStackTrace st]
  (when st
    (let [^java.util.List fs (.getFrames st)
          n (.size fs)]
      (loop [i 0 out (transient [])]
        (if (== i n)
          (persistent! out)
          (let [^jdk.jfr.consumer.RecordedFrame f (.get fs i)]
            (recur (unchecked-inc i)
                   (conj! out {:perf/depth i
                               :perf/fn (demunge (.getName (.getType (.getMethod f))))
                               :perf/line (.getLineNumber f)}))))))))

(defn site
  "Nearest frame the user owns; falls back to the innermost Clojure frame.

  One pass, not two lazy filters. The fallback used to force a second
  traversal building a second lazy seq to answer a question the first pass
  had already seen the answer to."
  [fs]
  (let [^java.util.List v (if (vector? fs) fs (vec fs))
        n (.size v)]
    (loop [i 0 fallback nil]
      (if (== i n)
        fallback
        (let [f (.get v i)
              nm (:perf/fn f)]
          (if (clojure-frame? nm)
            (if (own-frame? nm)
              f
              (recur (unchecked-inc i) (or fallback f)))
            (recur (unchecked-inc i) fallback)))))))

(defn- site-ref
  "A frame reduced to the two keys a site carries. Returns {} for nil so
  the schema's [:map] holds even when a stack had no usable frame."
  [f]
  (if f
    {:perf/fn (:perf/fn f) :perf/line (:perf/line f)}
    {}))

(defn observation
  "Build an observation. ATTRS are already-qualified kind-specific keys,
  merged flat rather than nested."
  [kind ^jdk.jfr.consumer.RecordedEvent e attrs]
  (let [fs (frames (.getStackTrace e))]
    (merge {:perf/kind kind
            :perf/t (.toEpochMilli (.getStartTime e))
            :perf/thread (some-> (.getThread e) .getJavaName)
            ;; Built directly, not via select-keys. select-keys reduces over
            ;; the key vector doing a find and a conj per key, allocating a
            ;; transient it immediately persists — twice per event, for two
            ;; keys each, on a map we are already holding.
            :perf/site (site-ref (site fs))
            :perf/via (site-ref (first fs))
            :perf/stack fs}
           attrs)))

;; ── datafy / nav ──────────────────────────────────────────────────
;;
;; The Clojure-native answer to clickable frames: a frame navs to its var,
;; a var datafies to its source location. No editor involved, and every
;; data browser gets it free.

(defn- resolve-frame [fn-name]
  (when (clojure-frame? fn-name)
    (let [[ns-part name-part] (str/split (str fn-name) #"/" 2)
          ;; anonymous fns demunge to ns/outer/fn--123 — the var is `outer`
          base (first (str/split (or name-part "") #"/"))]
      (try (some-> (find-ns (symbol ns-part)) ns-interns (get (symbol base)))
           (catch Throwable _ nil)))))

(defn nav-frame [frame]
  (or (some-> (resolve-frame (:perf/fn frame)) d/datafy) frame))

(extend-protocol p/Navigable
  clojure.lang.IPersistentMap
  (nav [_ k v]
    (cond
      (and (map? v) (:perf/fn v)) (nav-frame v)
      (and (= k :perf/stack) (sequential? v)) (mapv nav-frame v)
      :else v)))

(extend-protocol p/Datafiable
  clojure.lang.Var
  (datafy [v]
    (let [m (meta v)]
      {:perf/var (symbol (str (ns-name (:ns m))) (str (:name m)))
       :perf/file (:file m)
       :perf/line (:line m)
       :perf/arglists (:arglists m)
       :perf/doc (:doc m)})))
