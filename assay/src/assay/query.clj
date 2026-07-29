(ns assay.query
  "Functions over observations. No I/O, no rendering, and referentially
  transparent — but not literally stateless: ranking queries call
  `model/own-frame?`, which memoises into a bounded process-global cache
  (idempotent; see assay.model). Every fn takes observations explicitly, so
  a query is testable with a literal vector of maps — no running JVM, no
  JFR, no feed. Results use
  qualified keys for the same reason the facts do: they cross library
  boundaries, and :fn/:line/:n collide with everything."
  (:require [assay.model :as model]
            [clojure.string :as str]))

(defn of-kind [obs kind] (filterv #(= kind (:assay/kind %)) obs))

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
                             (assoc base :assay/total
                                    (reduce (fn [a o] (if-let [v (agg-key o)] (+ a v) a)) 0 vs))
                             base))))
                  (sort-by #(or (:assay/total %) (:assay/n %)) >))]
    (vec (if limit (take limit rows) rows))))

(defn by
  "Group OBS by KEY-PATH, count, optionally sum AGG-KEY."
  ([obs key-path] (by obs key-path nil))
  ([obs key-path agg-key]
   (let [path (if (vector? key-path) key-path [key-path])]
     (ranked obs #(get-in % path) agg-key nil
             (fn [k n] {:assay/key k :assay/n n})))))

(defn- sites [obs kind agg-key limit]
  (ranked (into [] (comp (filter #(= kind (:assay/kind %)))
                         (filter #(model/own-frame? (get-in % [:assay/site :assay/fn]))))
                obs)
          :assay/site agg-key limit
          (fn [site n] {:assay/fn (:assay/fn site) :assay/line (:assay/line site) :assay/n n})))

(defn allocation
  "Allocation sites, ranked by SAMPLE COUNT.

  Not by summed weight: JFR's weight is relative and heavy-tailed —
  measured, a 10-sample row extrapolated to 65 GB and flattened every
  other bar. Count is proportional to allocation pressure and robust."
  ([obs] (allocation obs 12))
  ([obs n] (sites obs :assay.kind/alloc nil n)))

(defn blocking
  "Where threads block, by total nanoseconds.
  Invisible to a CPU profile: a blocked thread burns no CPU."
  ([obs] (blocking obs 12))
  ([obs n] (sites obs :assay.kind/block :assay.block/duration-ns n)))

(defn deopts
  "What the JIT had to un-assume — the runtime analogue of SBCL's
  compile-time efficiency notes. By site AND mechanism: a megamorphic
  Clojure call deopts inside clojure.lang.Var, which is true and not a
  line you can edit."
  ([obs] (deopts obs 12))
  ([obs n]
   (ranked (of-kind obs :assay.kind/deopt)
           (juxt #(get-in % [:assay/site :assay/fn])
                 #(get-in % [:assay/via :assay/fn])
                 :assay.deopt/reason)
           nil n
           (fn [[site via reason] cnt]
             {:assay/fn site :assay/via via
              :assay.deopt/reason reason :assay/n cnt}))))

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
   ;; Match by NAME EQUALITY, not prefix. `starts-with?` fabricated
   ;; phantom callers — a query for "app/f" matched "app/foo", "map"
   ;; matched "map-indexed". And because a client often sends a BARE name
   ;; ("handle", from cider-symbol-at-point) while frames are qualified
   ;; ("user/handle"), also accept a "/name" suffix — which is still exact
   ;; on the name segment, so "handle" matches "user/handle" but NOT
   ;; "user/handler".
   (let [target (str fn-name)
         suffix (str "/" target)
         hit? (fn [nm] (or (= nm target) (str/ends-with? nm suffix)))]
     (->> obs
          (keep (fn [o]
                  (let [stack (:assay/stack o)
                        cnt (count stack)]
                    (loop [i 0 matched? false prev nil]
                      (when (< i cnt)
                        (let [nm (:assay/fn (nth stack i))]
                          (cond
                            (= nm prev)   (recur (inc i) matched? prev)
                            matched?      nm
                            (hit? nm)     (recur (inc i) true nm)
                            :else         (recur (inc i) false nm))))))))
          frequencies
          (sort-by second >)
          (take n)
          (mapv (fn [[f c]] {:assay/caller f :assay/calls c}))))))

(defn by-fn
  "Join compile-time NOTES and runtime OBSERVATIONS by the function they
  belong to — the one view no other tool gives: what the compiler gave up
  on, beside what the machine actually spent, per fn.

  They are DIFFERENT fact types and are NOT forced into one schema — a
  note has a message and a signature, an observation a stack and a sample
  count. What they share is a SITE, and the site is the join key. Each
  cost stays in its own basis: the compile side lists the notes, the
  runtime side counts samples per kind, and the two never share a column —
  the same discipline capture keeps between JFR weight and census.

  NOTES come from `assay.diagnose/scan` (they carry :assay.note/var). A note
  taken from a raw form has no fn to key on and is not included."
  [observations notes]
  (let [note-fn #(some-> (:assay.note/var %) str)
        obs-fn  #(get-in % [:assay/site :assay/fn])
        by-note (group-by note-fn (filter note-fn notes))
        by-obs  (group-by obs-fn (filter obs-fn observations))
        fns (into (sorted-set) (concat (keys by-note) (keys by-obs)))]
    (vec (for [f fns]
           #:assay.fn{:name f
                     :compile (mapv #(select-keys % [:assay.note/code
                                                     :assay.note/message
                                                     :assay.note/span])
                                    (by-note f))
                     :runtime (reduce (fn [m [k os]] (assoc m k (count os)))
                                      {} (group-by :assay/kind (by-obs f)))}))))

(defn rates
  "Aggregated time view over SAMPLES taken every PERIOD-MS.

  The verdict watches the post-GC FLOOR, not the peak: a healthy sawtooth
  returns to the same baseline, and a rising floor is the leak. Peak tells
  you nothing either way."
  [samples period-ms]
  (let [n (count samples)]
    (if (< n 4)
      {:assay/samples n :assay/verdict :assay.verdict/warming-up}
      (let [per-s (/ 1000.0 period-ms)
            alloc (map :assay/alloc-rate samples)
            heaps (map :assay/heap samples)
            half (quot n 2)
            f1 (apply min (take half heaps))
            f2 (apply min (drop half heaps))]
        {:assay/samples n
         :assay.alloc/mean-mb-s (/ (* per-s (/ (reduce + alloc) n)) 1e6)
         :assay.alloc/peak-mb-s (/ (* per-s (apply max alloc)) 1e6)
         :assay.heap/now-mb (/ (last heaps) 1e6)
         :assay.heap/peak-mb (/ (apply max heaps) 1e6)
         :assay.heap/floor-mb (/ (apply min heaps) 1e6)
         ;; The verdict's EVIDENCE, exposed. `:floor-rising` was handed
         ;; down with only the combined floor visible — you could not see
         ;; the two numbers it compared. SBCL never does that: `describe`
         ;; shows the derived type, `time` shows the cycles, and you draw
         ;; the conclusion. So both half-window floors and their ratio are
         ;; here, and the verdict is a tag you can check, not one to trust.
         :assay.heap/floor-first-mb (/ f1 1e6)
         :assay.heap/floor-second-mb (/ f2 1e6)
         :assay.heap/floor-ratio (if (pos? f1) (/ (double f2) f1) 1.0)
         :assay/verdict (cond (> f2 (* 1.15 f1)) :assay.verdict/floor-rising
                             (< f2 (* 0.85 f1)) :assay.verdict/floor-falling
                             :else :assay.verdict/stable)}))))

(defn growth
  "What GREW between two censuses — the single most valuable leak query
  in any memory profiler. Big is usually your working set; grew-across-a-
  net-zero-cycle is the bug. Exact, not sampled."
  ([before after] (growth before after 15))
  ([before after n]
   (let [b (into {} (map (juxt :assay.class/name identity) before))
         a (into {} (map (juxt :assay.class/name identity) after))]
     (->> (into #{} (concat (keys b) (keys a)))
          (map (fn [k] {:assay.class/name k
                        :assay/d-instances (- (:assay.class/instances (a k) 0)
                                             (:assay.class/instances (b k) 0))
                        :assay/d-bytes (- (:assay.class/bytes (a k) 0)
                                         (:assay.class/bytes (b k) 0))}))
          (filter #(pos? (:assay/d-instances %)))
          (sort-by :assay/d-bytes >)
          (take n)
          vec))))

(defn datoms
  "Observations as [e a v] tuples for Datomic / DataScript. The attribute
  names are already qualified, so they transact as-is."
  [obs]
  (mapcat (fn [i o]
            (concat
             (for [[k v] (dissoc o :assay/stack) :when (and v (not (map? v)))] [i k v])
             (for [f (:assay/stack o)] [i :assay/frame (:assay/fn f)])))
          (range) obs))

(defn summary
  "Everything worth checking, as DATA. Every key coexists — the
  measurements, their ranked sites, and a verdict tag beside its evidence.

  This is the opt-in digest, the sb-sprof :report of this library. The raw
  facts it reduces (observations, samples) stay reachable underneath it;
  nothing here replaces them. To render it fetch-first, `describe`."
  [obs samples period-ms]
  (merge (rates samples period-ms)
         {:assay/top-alloc (allocation obs 5)
          :assay/top-blocked (blocking obs 3)
          :assay/top-deopts (deopts obs 3)}))

(defn- mb [x] (when x (format "%.1f MB" (double x))))

(defn describe
  "Render a `summary` the way SBCL renders `describe` and `time`: labeled
  JVM measurements, then the ranked sites as raw facts. The numbers are
  the MXBeans' and JFR's; assay adds only the field labels and does the
  arithmetic. No verdict WORD is printed — the two half-window floors and
  their ratio are the reading; whether that is 'rising' is yours to say,
  the same way `time` prints cycles and lets you call it slow. The verdict
  tag lives in the map for anyone who wants it; it is not printed here.

  The map is the data. This is one reading of it, for someone who would
  rather see the readings than be told what they mean."
  [s]
  (with-out-str
    (printf "heap    now %s   peak %s   floor %s\n"
            (mb (:assay.heap/now-mb s)) (mb (:assay.heap/peak-mb s)) (mb (:assay.heap/floor-mb s)))
    (when-let [m (:assay.alloc/mean-mb-s s)]
      (printf "alloc   mean %.1f MB/s   peak %.1f MB/s\n" (double m) (double (:assay.alloc/peak-mb-s s))))
    (printf "floor   first-half %s   second-half %s   ratio %.2fx\n"
            (mb (:assay.heap/floor-first-mb s)) (mb (:assay.heap/floor-second-mb s))
            (double (or (:assay.heap/floor-ratio s) 1.0)))
    (doseq [[label rows] [["alloc sites" (:assay/top-alloc s)]
                          ["blocked at" (:assay/top-blocked s)]
                          ["deopts" (:assay/top-deopts s)]]
            :when (seq rows)]
      (printf "\n%s\n" label)
      (doseq [r rows]
        (printf "  %-40s n=%s%s\n"
                (str (:assay/fn r) ":" (:assay/line r)) (:assay/n r)
                (if-let [t (:assay/total r)] (format "  total=%s" t) ""))))))
