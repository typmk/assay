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

(defn- attrs-for
  "Kind-specific values as top-level QUALIFIED keys, merged flat.
  Nesting under :attrs is what you do without namespaces; with them the
  flat form is simpler and Datomic-shaped."
  [kind e]
  (case kind
    :perf.kind/alloc
    {:perf.alloc/class (try (some-> (.getValue e "objectClass") .getName) (catch Throwable _ nil))
     :perf.alloc/weight (try (.getLong e "weight") (catch Throwable _ nil))}
    :perf.kind/block
    {:perf.block/duration-ns (try (.toNanos (.getDuration e)) (catch Throwable _ nil))}
    :perf.kind/deopt
    {:perf.deopt/reason (try (.getString e "reason") (catch Throwable _ nil))
     :perf.deopt/action (try (.getString e "action") (catch Throwable _ nil))}
    {}))

(defn- start-stream [types tap?]
  (let [obs (atom [])
        rs  (jdk.jfr.consumer.RecordingStream.)]
    (doseq [t types] (.withStackTrace (.enable rs t)))
    (doseq [t types]
      (let [kind (event-kinds t)]
        (.onEvent rs t (reify java.util.function.Consumer
                         (accept [_ e]
                           (let [o (model/observation kind e (attrs-for kind e))]
                             (swap! obs conj o)
                             ;; tap> is Clojure's standard "send a value
                             ;; somewhere to look at it" channel — one line,
                             ;; and Portal/Reveal/REBL pick it up with no
                             ;; configuration. Off by default: a firehose
                             ;; into a browser is a denial of service.
                             (when tap? (tap> o))))))))
    (.startAsync rs)
    {:stream rs :obs obs}))

(defn- start-poll [period-ms keep]
  (let [ring (atom clojure.lang.PersistentQueue/EMPTY)
        run  (atom true)
        tmx  ^com.sun.management.ThreadMXBean
             (java.lang.management.ManagementFactory/getThreadMXBean)
        gcs  (java.lang.management.ManagementFactory/getGarbageCollectorMXBeans)
        th (Thread.
            (fn []
              (let [rt (Runtime/getRuntime)]
                (loop [prev 0]
                  (when @run
                    (let [used (- (.totalMemory rt) (.freeMemory rt))
                          alloc (reduce + (map #(.getThreadAllocatedBytes tmx %)
                                               (.getAllThreadIds tmx)))]
                      (swap! ring (fn [q]
                                    (let [q (conj q #:perf{:t (System/currentTimeMillis)
                                                           :heap used
                                                           :alloc-rate (max 0 (- alloc prev))
                                                           :gc-count (reduce + (map #(.getCollectionCount %) gcs))})]
                                      (if (> (count q) keep) (pop q) q))))
                      (Thread/sleep period-ms)
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
  (close [this] (some-> stream :stream .close) (some-> poll :run (reset! false))))

(defn describe [r]
  (format "#perf/recorder{:observations %d :samples %d :running? %s}"
          (count (if-let [o (get-in r [:stream :obs])] @o []))
          (count (if-let [x (get-in r [:poll :ring])] @x []))
          (nil? (:stopped r))))

;; A capture is a value, so the REPL prints it — and printing it printed
;; 123 KB of observations. Values should be printable; that is what a
;; print-method is for. The data is still there, behind `observations`.
(defmethod print-method Recorder [r ^java.io.Writer w] (.write w (describe r)))

(defn start
  "Begin recording. Returns a RECORDER — a live handle, not a value.
  It is Closeable, so (with-open [r (start)] ...) works.

  opts: {:types [...] :period-ms 500 :keep 240 :tap? false}
  With :tap? true every observation is tap>'d — Portal, Reveal and REBL
  render them live, with no integration on either side."
  ([] (start {}))
  ([{:keys [types period-ms keep tap?] :or {period-ms 500 keep 240 tap? false}}]
   (cap/require! :jfr)
   (map->Recorder {:started (System/currentTimeMillis)
                  :stream (start-stream (or types (keys event-kinds)) tap?)
                  :poll (start-poll period-ms keep)})))

(defn stop
  "Stop RECORDER. The data stays readable; stopping closes the stream,
  it does not discard what was recorded."
  [recorder]
  (.close ^java.io.Closeable recorder)
  (assoc recorder :stopped (System/currentTimeMillis)))

(defn observations
  "The facts recorded so far. THE input to perf.query."
  [recorder]
  (if-let [o (get-in recorder [:stream :obs])] @o []))

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
