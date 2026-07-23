(ns perf.query
  "Pure functions over observations. No state, no I/O, no rendering.

  Every fn takes observations explicitly, so a query is testable with a
  literal vector of maps — no running JVM, no JFR, no feed. Results use
  qualified keys for the same reason the facts do: they cross library
  boundaries, and :fn/:line/:n collide with everything."
  (:require [perf.model :as model]
            [clojure.string :as str]))

(defn of-kind [obs kind] (filterv #(= kind (:perf/kind %)) obs))

(defn- ranked
  "Group, count, optionally sum, sort descending, truncate.

  Every ranking view in this namespace was this same shape written out
  again with a different row constructor — `by`, `sites` and `deopts` had
  three copies of the group/count/agg/sort pipeline between them. ROW
  turns a [group-key members] pair into the row; everything else is here
  once."
  [xs keyfn agg-key limit row]
  (let [rows (->> (group-by keyfn xs)
                  (map (fn [[k vs]]
                         (let [base (row k (count vs))]
                           (if agg-key
                             (assoc base :perf/total
                                    (reduce (fn [a o] (if-let [v (agg-key o)] (+ a v) a)) 0 vs))
                             base))))
                  (sort-by #(or (:perf/total %) (:perf/n %)) >))]
    (vec (if limit (take limit rows) rows))))

(defn by
  "Group OBS by KEY-PATH, count, optionally sum AGG-KEY."
  ([obs key-path] (by obs key-path nil))
  ([obs key-path agg-key]
   (let [path (if (vector? key-path) key-path [key-path])]
     (ranked obs #(get-in % path) agg-key nil
             (fn [k n] {:perf/key k :perf/n n})))))

(defn- sites [obs kind agg-key limit]
  (ranked (into [] (comp (filter #(= kind (:perf/kind %)))
                         (filter #(model/own-frame? (get-in % [:perf/site :perf/fn]))))
                obs)
          :perf/site agg-key limit
          (fn [site n] {:perf/fn (:perf/fn site) :perf/line (:perf/line site) :perf/n n})))

(defn allocation
  "Allocation sites, ranked by SAMPLE COUNT.

  Not by summed weight: JFR's weight is relative and heavy-tailed —
  measured, a 10-sample row extrapolated to 65 GB and flattened every
  other bar. Count is proportional to allocation pressure and robust."
  ([obs] (allocation obs 12))
  ([obs n] (sites obs :perf.kind/alloc nil n)))

(defn blocking
  "Where threads block, by total nanoseconds.
  Invisible to a CPU profile: a blocked thread burns no CPU."
  ([obs] (blocking obs 12))
  ([obs n] (sites obs :perf.kind/block :perf.block/duration-ns n)))

(defn deopts
  "What the JIT had to un-assume — the runtime analogue of SBCL's
  compile-time efficiency notes. By site AND mechanism: a megamorphic
  Clojure call deopts inside clojure.lang.Var, which is true and not a
  line you can edit."
  ([obs] (deopts obs 12))
  ([obs n]
   (ranked (of-kind obs :perf.kind/deopt)
           (juxt #(get-in % [:perf/site :perf/fn])
                 #(get-in % [:perf/via :perf/fn])
                 :perf.deopt/reason)
           nil n
           (fn [[site via reason] cnt]
             {:perf/fn site :perf/via via
              :perf.deopt/reason reason :perf/n cnt}))))

(defn callers
  "Who called FN-NAME, from recorded stacks — a call graph from actual
  execution, not static analysis."
  ([obs fn-name] (callers obs fn-name 10))
  ([obs fn-name n]
   ;; MEASURED 2026-07-23: 100 ms over 20,705 observations, 20x every other
   ;; query. Two causes, both from building collections to answer a question
   ;; that needs none. (str fn-name) sat INSIDE the per-frame predicate, so
   ;; it ran once per frame — 490,000 times for one call. And `collapse`
   ;; materialised a partition-by of every stack just to read one element
   ;; of it.
   ;;
   ;; Now: one pass per stack, stopping at the first frame above the match.
   ;; No intermediate sequence exists.
   ;;
   ;; Runs of the same name are skipped because Clojure emits
   ;; invoke -> invokeStatic for every call: without collapsing them, every
   ;; function appears to call itself. That rule used to live in
   ;; model/collapse, which had no other caller once this loop absorbed it.
   (let [prefix (str fn-name)]
     (->> obs
          (keep (fn [o]
                  (let [stack (:perf/stack o)
                        cnt (count stack)]
                    (loop [i 0 matched? false prev nil]
                      (when (< i cnt)
                        (let [nm (:perf/fn (nth stack i))]
                          (cond
                            (= nm prev)   (recur (inc i) matched? prev)
                            matched?      nm
                            (str/starts-with? nm prefix) (recur (inc i) true nm)
                            :else         (recur (inc i) false nm))))))))
          frequencies
          (sort-by second >)
          (take n)
          (mapv (fn [[f c]] {:perf/caller f :perf/calls c}))))))

(defn rates
  "Aggregated time view over SAMPLES taken every PERIOD-MS.

  The verdict watches the post-GC FLOOR, not the peak: a healthy sawtooth
  returns to the same baseline, and a rising floor is the leak. Peak tells
  you nothing either way."
  [samples period-ms]
  (let [n (count samples)]
    (if (< n 4)
      {:perf/samples n :perf/verdict :perf.verdict/warming-up}
      (let [per-s (/ 1000.0 period-ms)
            alloc (map :perf/alloc-rate samples)
            heaps (map :perf/heap samples)
            half (quot n 2)
            f1 (apply min (take half heaps))
            f2 (apply min (drop half heaps))]
        {:perf/samples n
         :perf.alloc/mean-mb-s (/ (* per-s (/ (reduce + alloc) n)) 1e6)
         :perf.alloc/peak-mb-s (/ (* per-s (apply max alloc)) 1e6)
         :perf.heap/now-mb (/ (last heaps) 1e6)
         :perf.heap/peak-mb (/ (apply max heaps) 1e6)
         :perf.heap/floor-mb (/ (apply min heaps) 1e6)
         :perf/verdict (cond (> f2 (* 1.15 f1)) :perf.verdict/floor-rising
                             (< f2 (* 0.85 f1)) :perf.verdict/floor-falling
                             :else :perf.verdict/stable)}))))

(defn growth
  "What GREW between two censuses — the single most valuable leak query
  in any memory profiler. Big is usually your working set; grew-across-a-
  net-zero-cycle is the bug. Exact, not sampled."
  ([before after] (growth before after 15))
  ([before after n]
   (let [b (into {} (map (juxt :perf.class/name identity) before))
         a (into {} (map (juxt :perf.class/name identity) after))]
     (->> (into #{} (concat (keys b) (keys a)))
          (map (fn [k] {:perf.class/name k
                        :perf/d-instances (- (:perf.class/instances (a k) 0)
                                             (:perf.class/instances (b k) 0))
                        :perf/d-bytes (- (:perf.class/bytes (a k) 0)
                                         (:perf.class/bytes (b k) 0))}))
          (filter #(pos? (:perf/d-instances %)))
          (sort-by :perf/d-bytes >)
          (take n)
          vec))))

(defn datoms
  "Observations as [e a v] tuples for Datomic / DataScript. The attribute
  names are already qualified, so they transact as-is."
  [obs]
  (mapcat (fn [i o]
            (concat
             (for [[k v] (dissoc o :perf/stack) :when (and v (not (map? v)))] [i k v])
             (for [f (:perf/stack o)] [i :perf/frame (:perf/fn f)])))
          (range) obs))

(defn summary
  "Everything worth checking. Ordered by the questions you actually ask:
  is it leaking, how hard is it allocating, and where."
  [obs samples period-ms]
  (merge (rates samples period-ms)
         {:perf/top-alloc (allocation obs 5)
          :perf/top-blocked (blocking obs 3)
          :perf/top-deopts (deopts obs 3)}))
