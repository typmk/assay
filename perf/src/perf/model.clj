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

(defn clojure-frame?
  "Does FN-NAME look like a demunged Clojure fn (ns/name)?
  Tests for `/`, not `$`: frames are demunged on the way in, so
  user$allocs is already user/allocs. Testing for `$` matched nothing and
  silently made two whole views return nil."
  [fn-name]
  (str/includes? (str fn-name) "/"))

(defn own-frame? [fn-name]
  (let [s (str fn-name)]
    (and (clojure-frame? s)
         (not (some #(str/starts-with? s %) noise-prefixes)))))

(defn demunge [cls] (clojure.lang.Compiler/demunge (str cls)))

(defn collapse
  "Collapse runs of equal names. Clojure emits invoke -> invokeStatic for
  every call, so without this every function appears to call itself."
  [names]
  (mapv first (partition-by identity names)))

(defn frames
  "Normalise a JFR stack into rows. One row per frame makes stacks a JOIN
  TARGET — 'fns in both allocation and blocking stacks' is a set
  intersection, not a bespoke report."
  [st]
  (when st
    (vec (map-indexed (fn [d f]
                        {:perf/depth d
                         :perf/fn (demunge (.getName (.getType (.getMethod f))))
                         :perf/line (.getLineNumber f)})
                      (.getFrames st)))))

(defn site
  "Nearest frame the user owns; falls back to the innermost Clojure frame."
  [fs]
  (let [clj (filter #(clojure-frame? (:perf/fn %)) fs)]
    (or (first (filter #(own-frame? (:perf/fn %)) clj)) (first clj))))

(defn observation
  "Build an observation. ATTRS are already-qualified kind-specific keys,
  merged flat rather than nested."
  [kind e attrs]
  (let [fs (frames (.getStackTrace e))]
    (merge {:perf/kind kind
            :perf/t (.toEpochMilli (.getStartTime e))
            :perf/thread (some-> (.getThread e) .getJavaName)
            :perf/site (select-keys (site fs) [:perf/fn :perf/line])
            :perf/via (select-keys (first fs) [:perf/fn :perf/line])
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
