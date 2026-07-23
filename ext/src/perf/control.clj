(ns perf.control
  "Things that CHANGE execution — kept apart from everything that only
  observes it, because the blast radius is different.

  Two mechanisms, and the ordering matters:

    trace!     rebind a Var. Live, reversible, no agent, no flags.
    restarts!  JDI. Catch pre-unwind, force a frame to return.

  I explored JFR method timing (JEP 520), JDI method-entry requests and
  runtime agent attach before noticing Clojure already had the first one.
  A fn is reached through a Var; rebind it and every call site picks up
  the wrapper on its next invocation. That is BEAM's recon_trace
  ergonomics in ten lines. JEP 520 was measured to work only when armed
  at JVM startup — jcmd on a running VM accepted the option and silently
  produced nothing.

  Var rebinding cannot touch what you cannot rebind: JDK internals,
  clojure.lang, direct-linked calls. That is where JDI earns its
  complexity, and nowhere else."
  (:require [perf.capability :as cap]
            [clojure.string :as str]))

;; ── trace: DELETED — use Sayid ──────────────────────────────────
;;
;; This namespace had trace!/calls/timings: rebind a Var, record args and
;; return values into a ring buffer. It worked, and Sayid does the same
;; job strictly better. Measured side by side on the same three fns:
;;
;;   perf.control/trace!   [[6] 12]                 flat, per-var, timing
;;                                                  in a separate map
;;   sayid ws-add-trace-ns!  a nested CALL TREE with
;;                           :args :return :children :depth :started-at
;;                           :ended-at :arg-map :meta — one call, whole ns
;;
;; Sayid also has inner tracing (every intermediate expression, via
;; tools.analyzer.jvm), a query layer, and CIDER integration. It is
;; maintained as mx.cider/sayid; 0.8.0 shipped July 2026.
;;
;; Keeping a worse reimplementation because I wrote it is the sunk-cost
;; version of engineering. `perf.trace` is a bridge to Sayid instead.

;; ── restarts: JDI, pre-unwind ─────────────────────────────────────

(defonce ^:private dbg (atom nil))

(defn- fname [f] (.name (.declaringType (.location f))))

(defn restarts!
  "Attach JDI and arm a restart for EXCEPTION-CLASS.
  Requires the :debug alias — JDI cannot attach to a VM without a jdwp
  agent installed at startup."
  ([] (restarts! "java.lang.Exception"))
  ([exception-class]
   (cap/require! :jdi)
   (when-not @dbg
     (let [vmm  (com.sun.jdi.Bootstrap/virtualMachineManager)
           conn (->> (.attachingConnectors vmm)
                     (filter #(= "com.sun.jdi.SocketAttach"
                                 (.name ^com.sun.jdi.connect.AttachingConnector %)))
                     first)
           args (.defaultArguments ^com.sun.jdi.connect.AttachingConnector conn)]
       (.setValue ^com.sun.jdi.connect.Connector$Argument (.get args "hostname") "127.0.0.1")
       (.setValue ^com.sun.jdi.connect.Connector$Argument (.get args "port")
                  (System/getProperty "jdwp.port"))
       (let [vm      (.attach ^com.sun.jdi.connect.AttachingConnector conn args)
             pending (atom nil)
             rt      (first (.classesByName vm exception-class))
             req     (.createExceptionRequest (.eventRequestManager vm) rt true true)]
         (.setSuspendPolicy req com.sun.jdi.request.EventRequest/SUSPEND_EVENT_THREAD)
         (.enable req)
         (future (let [q (.eventQueue vm)]
                   (loop []
                     (doseq [e (iterator-seq (.eventIterator (.remove q)))]
                       (when (instance? com.sun.jdi.event.ExceptionEvent e)
                         (reset! pending e)))
                     (recur))))
         (reset! dbg {:vm vm :pending pending}))))
   :armed))

(defn- frame-locals
  "Visible locals of a suspended frame, as data.

  CDT (George Jahad, ~2010) could eval arbitrary Clojure in the lexical
  scope of a suspended remote frame — still the high-water mark for JDI
  debugging in Clojure, and dead since swank. This is the cheap half of
  it: read the locals. Evaluating IN the frame needs invokeMethod, which
  deadlocks on a self-attached VM; CDT avoided that by being a separate
  process, which is the architecture to copy if this is ever wanted."
  [^com.sun.jdi.StackFrame f]
  (try
    (into {} (for [[^com.sun.jdi.LocalVariable v val] (.getValues f (.visibleVariables f))]
               [(.name v) (str val)]))
    (catch Throwable _ {})))

(defn pending
  "The suspended failure — its stack is STILL LIVE, with locals.
  This is the thing CIDER structurally cannot give you: by the time
  CIDER shows a stacktrace, the frames are gone."
  ([] (pending 8))
  ([n]
   (if-let [e (some-> @dbg :pending deref)]
     (let [t (.thread e)]
       {:perf.dbg/exception (.name (.referenceType (.exception e)))
        :perf.dbg/thread (.name t)
        :perf.dbg/frames
        (vec (for [^com.sun.jdi.StackFrame f (take n (.frames t))]
               #:perf.dbg{:fn (fname f)
                          :line (.lineNumber (.location f))
                          :locals (frame-locals f)}))})
     :nothing-suspended)))

;; A JDI ObjectReference for an arbitrary value, without invokeMethod.
;;
;; forceEarlyReturn needs a com.sun.jdi.Value. The documented route —
;; ClassType.invokeMethod to call Long.valueOf in the target VM —
;; DEADLOCKS on a self-attached VM: it must resume the suspended thread,
;; and the debugger is in that same process.
;;
;; Self-attach makes the target heap OUR heap, so the object already
;; exists; only the handle is missing. Park it in a field and read it
;; back through JDI: a field read, no invocation, nothing to resume.
(deftype Carrier [^:unsynchronized-mutable ^Object v]
  clojure.lang.IFn (invoke [this x] (set! v x) this)
  clojure.lang.IDeref (deref [_] v))

(defonce ^:private carrier (->Carrier nil))

(defn- object-ref [vm value]
  (carrier value)
  (let [rt (first (.classesByName vm "perf.control.Carrier"))]
    (when rt
      (.getValue (first (.instances rt 1)) (.fieldByName rt "v")))))

(defn- register-boxes!
  "Make the failing class's own loader an INITIATING loader for the boxed
  types, so JDI can resolve the frame's return type.

  JDI resolves a return type through the DECLARING class's loader,
  searching the classes it initiated. A Clojure DynamicClassLoader had
  initiated ~15 classes and never java.lang.Object, so returnType() threw
  ClassNotLoadedException and forceEarlyReturn rejected every non-null
  value. ClassLoader.loadClass does NOT fix it — it is pure Java and
  never reaches the VM's resolve_or_fail. Class.forName(n, false, cl) does."
  [cls-name]
  (try
    (let [dcl (.getClassLoader (Class/forName cls-name false (clojure.lang.RT/baseLoader)))]
      (doseq [c ["java.lang.Object" "java.lang.String" "java.lang.Long"
                 "java.lang.Double" "java.lang.Boolean" "java.lang.Number"]]
        (Class/forName c false dcl))
      true)
    (catch Throwable _ false)))

(defn use-value!
  "CL's use-value: make the innermost frame matching FN-PREFIX return
  VALUE, and continue. Any value — nil, string, number, or a Clojure
  data structure."
  [fn-prefix value]
  (if-let [e (some-> @dbg :pending deref)]
    (let [vm (:vm @dbg) t (.thread e) frames (vec (.frames t))
          idx (first (keep-indexed
                      #(when (str/starts-with? (fname %2) (str fn-prefix)) %1) frames))]
      (if-not idx
        {:error :no-frame-matching :prefix fn-prefix :frames (mapv fname frames)}
        ;; Name the frame BEFORE resuming: a StackFrame is invalid once its
        ;; thread resumes, and reading it in the return map threw
        ;; InvalidStackFrameException — intermittently, racing the resume.
        (let [where (fname (nth frames idx))]
          (register-boxes! where)
          (when (pos? idx) (.popFrames t (nth frames (dec idx))))
          (.forceEarlyReturn t (when (some? value) (object-ref vm value)))
          (.resume t)
          (reset! (:pending @dbg) nil)
          {:restarted where :returned value})))
    :nothing-suspended))

(defn abort!
  "Resume and let the exception propagate normally."
  []
  (when-let [e (some-> @dbg :pending deref)]
    (.resume (.thread e))
    (reset! (:pending @dbg) nil))
  :resumed)
