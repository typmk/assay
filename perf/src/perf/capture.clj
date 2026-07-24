(ns perf.capture
  "Capture returns a VALUE you hold, not a global you mutate.

  v2 had five `defonce` singletons and a `monitor!`/`unmonitor!` pair —
  hidden places, one per feed, so you could not hold two captures, diff
  them, pass one to a test, or run two at once, and 'is it recording?'
  was invisible.

    (def c (start))          ; a value
    (observations c)         ; read it
    (stop c)                 ; done

  Three mechanisms, because there are three:
    stream  JFR events with stacks
    poll    MXBeans over time
    census  stop-the-world, exact

  Sampling vs exactness is not a detail: JFR's allocation `weight` is a
  RELATIVE sample weight — measured, 639 samples came within 1.13x of
  the truth while 10 samples extrapolated to 65 GB. Stream data is
  ranked by SAMPLE COUNT; census data is absolute; the two never share
  a column."
  (:require [perf.model :as model]
            [perf.capability :as cap]))

(def event-kinds
  {"jdk.ObjectAllocationSample" :perf.kind/alloc
   "jdk.JavaMonitorEnter"       :perf.kind/block
   "jdk.ThreadPark"             :perf.kind/block
   "jdk.JavaMonitorWait"        :perf.kind/block
   "jdk.Deoptimization"         :perf.kind/deopt})

;; The rest of perf speaks qualified keywords; `start` should too. JFR's
;; own event names are strings ("jdk.ObjectAllocationSample"), so a caller
;; who reached for the vocabulary the tool uses everywhere else —
;; (start {:types [:alloc]}) — got a ClassCastException from deep inside
;; start-stream. Accept a friendly short keyword, the datafy'd
;; :perf.kind/* keyword, or the JFR string itself; normalise to the string
;; the RecordingStream needs.
(def ^:private event-aliases
  (merge (into {} (map (fn [[jfr kind]] [kind jfr])) event-kinds)
         {:alloc  "jdk.ObjectAllocationSample"
          :block  "jdk.JavaMonitorEnter"
          :monitor "jdk.JavaMonitorEnter"
          :park   "jdk.ThreadPark"
          :wait   "jdk.JavaMonitorWait"
          :deopt  "jdk.Deoptimization"}))

(defn- ->event
  "Normalise an event spec to the JFR event-name string RecordingStream
  needs. Accepts the JFR string, a friendly keyword (:alloc :block :park
  :wait :monitor :deopt), or the datafy'd :perf.kind/* keyword."
  [t]
  (cond
    (string? t) t
    (contains? event-aliases t) (event-aliases t)
    :else (throw (IllegalArgumentException.
                  (str "unknown event " (pr-str t) " — pass a JFR name string or one of "
                       (pr-str (vec (sort (keys event-aliases)))))))))

(defn- attrs-for
  "Kind-specific values as top-level QUALIFIED keys, merged flat.
  Nesting under :attrs is what you do without namespaces; with them the
  flat form is simpler and Datomic-shaped."
  [kind ^jdk.jfr.consumer.RecordedEvent e]
  (case kind
    :perf.kind/alloc
    {:perf.alloc/class (try (some-> ^jdk.jfr.consumer.RecordedClass (.getValue e "objectClass") .getName)
                            (catch Throwable _ nil))
     :perf.alloc/weight (try (.getLong e "weight") (catch Throwable _ nil))}
    :perf.kind/block
    {:perf.block/duration-ns (try (.toNanos (.getDuration e)) (catch Throwable _ nil))}
    :perf.kind/deopt
    {:perf.deopt/reason (try (.getString e "reason") (catch Throwable _ nil))
     :perf.deopt/action (try (.getString e "action") (catch Throwable _ nil))}
    {}))

;; MEASURED 2026-07-23: a 5s workload produced 15,000 alloc events, each
;; carrying ~24 frame maps. That is roughly 7 MB/s of retained
;; observations, unbounded — a profiler that becomes the leak it exists to
;; find, and the failure mode is an OOM in the process you were debugging.
;;
;; So the observation log is a ring: past MAX, the oldest half is dropped.
;; Halving rather than dropping one-per-event keeps it amortised O(1)
;; instead of O(n) per event.
;;
;; Drops are COUNTED and reported by `describe`, `snapshot` and
;; `observations-dropped`. A cap that silently discards data would make
;; every count downstream a quiet lie — "12 samples at this site" has to
;; mean twelve, or the ranking is fiction.
(def default-max-observations 200000)

(defn- start-stream [types tap? max-obs]
  (let [obs (atom [])
        dropped (atom 0)
        rs  (jdk.jfr.consumer.RecordingStream.)]
    (doseq [t types]
      (let [^jdk.jfr.EventSettings s (.withStackTrace ^jdk.jfr.EventSettings (.enable rs ^String t))]
        ;; ObjectAllocationSample is THROTTLED — default rate produced 1
        ;; sample over a 5-GC render, so the site ranking was empty. A rate
        ;; makes the sites actually populate. Throttle is only valid for
        ;; the sample event; setting it on a monitor/park event throws, so
        ;; it is scoped to the one event that takes it.
        (when (= t "jdk.ObjectAllocationSample")
          (.with s "throttle" "2000/s"))))
    (doseq [t types]
      (let [kind (event-kinds t)]
        (.onEvent rs t (reify java.util.function.Consumer
                         (accept [_ e]
                           (let [o (model/observation kind e (attrs-for kind e))
                                 half (quot (long max-obs) 2)
                                 ;; The drop count is accounted OUTSIDE the
                                 ;; swap! fn. swap! re-invokes its fn on
                                 ;; every CAS retry, so a nested
                                 ;; (swap! dropped + half) inside it
                                 ;; multiply-counted under contention —
                                 ;; masked today only because JFR dispatches
                                 ;; on one thread. swap-vals! gives the
                                 ;; atomic before/after; a drop shrank the
                                 ;; vector, so count it exactly once.
                                 [before after]
                                 (swap-vals! obs
                                             (fn [v]
                                               (if (>= (count v) (long max-obs))
                                                 (conj (into [] (subvec v half)) o)
                                                 (conj v o))))]
                             (when (< (count after) (count before))
                               (swap! dropped + half))
                             ;; tap> is Clojure's standard "send a value
                             ;; somewhere to look at it" channel — one line,
                             ;; and Portal/Reveal/REBL pick it up with no
                             ;; configuration. Off by default: a firehose
                             ;; into a browser is a denial of service.
                             (when tap? (tap> o))))))))
    (.startAsync rs)
    {:stream rs :obs obs :dropped dropped :max max-obs}))

(defn- start-poll [period-ms keep]
  (let [ring (atom clojure.lang.PersistentQueue/EMPTY)
        run  (atom true)
        tmx  ^com.sun.management.ThreadMXBean
             (java.lang.management.ManagementFactory/getThreadMXBean)
        gcs  (java.lang.management.ManagementFactory/getGarbageCollectorMXBeans)
        th (Thread.
            (fn []
              ;; ids re-read each tick: threads come and go, and a stale
              ;; array silently stops counting the new ones. areduce over
              ;; the long[] rather than (reduce + (map ..)) — that boxed
              ;; every thread's byte count, once a tick, in the thread
              ;; whose job is measuring allocation.
              (let [rt (Runtime/getRuntime)
                    read-alloc (fn ^long []
                                 (let [^longs ids (.getAllThreadIds tmx)]
                                   (areduce ids i acc (long 0)
                                            (+ acc (.getThreadAllocatedBytes tmx (aget ids i))))))]
                ;; seed prev with a REAL reading, not 0. Seeded at 0 the
                ;; first tick reported alloc-rate = cumulative-bytes-since-
                ;; JVM-start, which inflated peak/mean alloc-rate.
                (loop [prev (read-alloc)]
                  (when @run
                    (let [used (- (.totalMemory rt) (.freeMemory rt))
                          alloc (read-alloc)
                          gcn (reduce (fn [^long a ^java.lang.management.GarbageCollectorMXBean g]
                                        (+ a (.getCollectionCount g)))
                                      0 gcs)]
                      (swap! ring (fn [q]
                                    (let [q (conj q #:perf{:t (System/currentTimeMillis)
                                                           :heap used
                                                           :alloc-rate (max 0 (- alloc prev))
                                                           :gc-count gcn})]
                                      (if (> (count q) keep) (pop q) q))))
                      (Thread/sleep (long period-ms))
                      (recur alloc))))))
            "perf-poll")]
    ;; daemon: must never block process exit. v1's non-daemon stream made
    ;; any script that started it appear to hang.
    (.setDaemon th true)
    (.start th)
    {:ring ring :run run :period period-ms}))

(declare describe)

;; A RECORDER, not a value. It holds live atoms and an open
;; RecordingStream: you cannot serialize it, compare it, or send it
;; anywhere. v3 called it Capture and gave it a print-method that made it
;; LOOK like a value, which is worse than being honest — a handle dressed
;; as a value invites you to treat it as one.
;;
;; The value is what `snapshot` returns. Same split Dart's VM Service
;; makes: the service is a connection; snapshots are data.
(defrecord Recorder [started stream poll stopped]
  Object
  (toString [this] (describe this))
  java.io.Closeable
  (close [this]
    (some-> ^jdk.jfr.consumer.RecordingStream (:stream stream) .close)
    (some-> poll :run (reset! false))))

(defn observations-dropped
  "How many observations the ring discarded. Zero unless the cap was hit."
  [recorder]
  (if-let [d (get-in recorder [:stream :dropped])] @d 0))

(defn describe [r]
  (let [d (observations-dropped r)]
    (format "#perf/recorder{:observations %d%s :samples %d :running? %s}"
            (count (if-let [o (get-in r [:stream :obs])] @o []))
            (if (pos? d) (format " :dropped %d" d) "")
            (count (if-let [x (get-in r [:poll :ring])] @x []))
            (nil? (:stopped r)))))

;; A capture is a value, so the REPL prints it — and printing it printed
;; 123 KB of observations. Values should be printable; that is what a
;; print-method is for. The data is still there, behind `observations`.
(defmethod print-method Recorder [r ^java.io.Writer w] (.write w ^String (describe r)))

(defn start
  "Begin recording. Returns a RECORDER — a live handle, not a value.
  It is Closeable, so (with-open [r (start)] ...) works.

  opts: {:types [...] :period-ms 500 :keep 240 :tap? false
         :max-observations 200000}
  With :tap? true every observation is tap>'d — Portal, Reveal and REBL
  render them live, with no integration on either side."
  ([] (start {}))
  ([{:keys [types period-ms keep tap? max-observations]
     :or {period-ms 500 keep 240 tap? false}}]
   (cap/require! :jfr)
   (map->Recorder {:started (System/currentTimeMillis)
                  :stream (start-stream (map ->event (or types (keys event-kinds))) tap?
                                        (or max-observations default-max-observations))
                  :poll (start-poll period-ms keep)})))

(defn stop
  "Stop RECORDER. The data stays readable; stopping closes the stream,
  it does not discard what was recorded."
  [recorder]
  (.close ^java.io.Closeable recorder)
  (assoc recorder :stopped (System/currentTimeMillis)))

(defn observations
  "The facts recorded so far. THE input to perf.query.

  Accepts a live RECORDER or a SNAPSHOT value: snapshot stores the same
  facts under :perf/observations, and a caller holding a snapshot
  naturally reaches for (observations snap). Both feed query."
  [recorder-or-snapshot]
  (or (get recorder-or-snapshot :perf/observations)
      (some-> (get-in recorder-or-snapshot [:stream :obs]) deref)
      []))

(defn samples
  "Time series from the poll mechanism."
  [recorder]
  (if-let [x (get-in recorder [:poll :ring])] (vec @x) []))

(defn period [recorder] (get-in recorder [:poll :period]))

(defn snapshot
  "An immutable VALUE of what RECORDER has seen: plain data, fully
  qualified, with no live objects in it.

  This is the thing you can pr-str over a socket, diff against a later
  one, write to a file, or hand to a client that has never heard of this
  library. The recorder stays behind; the snapshot travels."
  [recorder]
  {:perf/started (:started recorder)
   :perf/stopped (:stopped recorder)
   :perf/at (System/currentTimeMillis)
   :perf/period-ms (period recorder)
   :perf/observations (observations recorder)
   :perf/dropped (observations-dropped recorder)
   :perf/samples (samples recorder)})

(defn census
  "EXACT live-object count by class — stop-the-world ground truth.

  Uses the DiagnosticCommand MBean, NOT (sh \"jcmd\" self ...): a JVM
  attaching to itself deadlocks, because the diagnostic command cannot be
  serviced while the calling thread blocks waiting for it."
  []
  (let [srv (java.lang.management.ManagementFactory/getPlatformMBeanServer)
        on  (javax.management.ObjectName. "com.sun.management:type=DiagnosticCommand")
        out (.invoke srv on "gcClassHistogram"
                     (into-array Object [nil]) (into-array String ["[Ljava.lang.String;"]))]
    (->> (clojure.string/split-lines (str out))
         (drop 3)
         (keep (fn [l]
                 (let [p (remove empty? (clojure.string/split (clojure.string/trim l) #"\s+"))]
                   (when (= 4 (count p))
                     (let [[_ inst bytes cls] p]
                       (when (re-matches #"\d+" inst)
                         #:perf.class{:name (model/demunge cls)
                                      :instances (parse-long inst)
                                      :bytes (parse-long bytes)}))))))
         vec)))

;; ── emit: perf's OWN measurements into the JFR stream ─────────────
;; Everything above READS the JVM's events. This WRITES one: a custom JFR
;; event, created dynamically via EventFactory (no compiled Event subclass),
;; so a weigh factor or an allocation total can land in the SAME recording as
;; ObjectAllocationSample and be correlated in any JFR tool or a capture
;; session. Field types are inferred from the values; the per-(name,shape)
;; EventFactory is cached — creating one defines a class, so a loop must not.

(defonce ^:private factories (atom {}))

(defn- field-type [v]
  (cond (integer? v) Long/TYPE (float? v) Double/TYPE
        (boolean? v) Boolean/TYPE :else String))

(defn- factory-for [ev-name ks vs]
  (let [shape [ev-name (mapv field-type vs)]]
    (or (@factories shape)
        (let [ann [(jdk.jfr.AnnotationElement. jdk.jfr.Name ev-name)
                   (jdk.jfr.AnnotationElement. jdk.jfr.Label ev-name)]
              vds (mapv (fn [k v] (jdk.jfr.ValueDescriptor. (field-type v) (name k))) ks vs)
              f (jdk.jfr.EventFactory/create ann vds)]
          (swap! factories assoc shape f)
          f))))

(defn emit
  "Emit a custom JFR event named EV-NAME carrying the key/value pairs in M,
  so perf's own numbers land in the JFR stream beside the JVM's events —
  correlatable in a `capture` session (add EV-NAME to its :types) or any JFR
  tool. Field types are inferred (long/double/boolean/String). Returns EV-NAME."
  [ev-name m]
  (cap/require! :jfr)
  (let [ks (vec (keys m)) vs (mapv m ks)
        e (.newEvent ^jdk.jfr.EventFactory (factory-for ev-name ks vs))]
    (dotimes [i (count ks)]
      (let [v (nth vs i)]
        (.set e i (cond (integer? v) (long v) (float? v) (double v)
                        (boolean? v) (boolean v) :else (str v)))))
    (.commit e)
    ev-name))
