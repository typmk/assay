(ns assay.remote
  "JDI from a SEPARATE process — the CDT architecture.

  ONE PROTOCOL, TWO BACKENDS. assay.control and assay.remote are the same
  restart protocol (pending -> use-value!) over two JDI transports:
  control self-attaches, remote attaches across a socket. They are kept as
  separate namespaces deliberately — the self-attach path needs deadlock
  workarounds (object-ref's field-read trick) that the cross-process path
  does not, and merging them into one transport-parameterised namespace is
  a rewrite of working JDI code that cannot be validated without a live
  jdwp target. For code you OWN, neither of these is the tool: use
  farolero (see assay.control's docstring). JDI is only for frames nobody
  instrumented.


  CDT (George Jahad, ~2010) debugged a remote VM from a REPL running on
  another VM, and its distinguishing feature was evaluating arbitrary
  Clojure in the lexical scope of a suspended remote frame. It died with
  swank; the idea did not.

  WHY THIS EXISTS SEPARATELY FROM assay.control:

  assay.control self-attaches — the debugger and the target are one JVM.
  That is convenient and it has a hard ceiling: ClassType.invokeMethod
  must RESUME the suspended thread to run the method, and on a
  self-attached VM the debugger is running on that same process. It
  deadlocks. I worked around it for return values with a field-read
  trick (assay.control/object-ref), but you cannot trick your way to
  calling a method.

  From a separate process there is no such problem. invokeMethod works,
  which buys:

    * boxing values properly, rather than the Carrier workaround
    * calling methods on live objects in the target
    * eval-in-frame — CDT's real feature

  USAGE — two JVMs:

    target$  clj -M:debug:assay ...            ; jdwp on 127.0.0.1:8899
    dbg$     clj -M:assay-ext
             (require '[assay.remote :as rem])
             (def vm (rem/attach 8899))
             (rem/arm! vm \"java.lang.ArithmeticException\")
             ;; target hits the failure
             (rem/pending vm)
             (rem/eval-str vm \"amount\")     ; a local, in the frame
             (rem/use-value! vm \"svc$compute\" 0)"
  (:require [clojure.string :as str]))

(defn attach
  "Attach to a JVM listening for jdwp on PORT. Returns a connection map."
  ([port] (attach "127.0.0.1" port))
  ([host port]
   (let [vmm (com.sun.jdi.Bootstrap/virtualMachineManager)
         conn (->> (.attachingConnectors vmm)
                   (filter #(= "com.sun.jdi.SocketAttach"
                               (.name ^com.sun.jdi.connect.AttachingConnector %)))
                   first)
         args (.defaultArguments ^com.sun.jdi.connect.AttachingConnector conn)]
     (.setValue ^com.sun.jdi.connect.Connector$Argument (.get args "hostname") (str host))
     (.setValue ^com.sun.jdi.connect.Connector$Argument (.get args "port") (str port))
     {:assay.remote/vm (.attach ^com.sun.jdi.connect.AttachingConnector conn args)
      :assay.remote/pending (atom nil)
      :assay.remote/host host
      :assay.remote/port port})))

(defn arm!
  "Suspend the target when EXCEPTION-CLASS is thrown, pre-unwind."
  [{:keys [:assay.remote/vm :assay.remote/pending]} exception-class]
  (let [rt  (first (.classesByName vm exception-class))
        req (.createExceptionRequest (.eventRequestManager vm) rt true true)]
    (.setSuspendPolicy req com.sun.jdi.request.EventRequest/SUSPEND_EVENT_THREAD)
    (.enable req)
    (future (let [q (.eventQueue vm)]
              (loop []
                (doseq [e (iterator-seq (.eventIterator (.remove q)))]
                  (when (instance? com.sun.jdi.event.ExceptionEvent e)
                    (reset! pending e)))
                (recur))))
    :armed))

(defn- fname [^com.sun.jdi.StackFrame f]
  (.name (.declaringType (.location f))))

(defn- locals [^com.sun.jdi.StackFrame f]
  (try (into {} (for [[^com.sun.jdi.LocalVariable v val]
                      (.getValues f (.visibleVariables f))]
                  [(.name v) (str val)]))
       (catch Throwable _ {})))

(defn pending
  "The suspended failure in the target, with frames and locals."
  ([conn] (pending conn 8))
  ([{:keys [:assay.remote/pending]} n]
   (if-let [e @pending]
     (let [t (.thread e)]
       {:assay.dbg/exception (.name (.referenceType (.exception e)))
        :assay.dbg/thread (.name t)
        :assay.dbg/frames (vec (for [^com.sun.jdi.StackFrame f (take n (.frames t))]
                                #:assay.dbg{:fn (fname f)
                                           :line (.lineNumber (.location f))
                                           :locals (locals f)}))})
     :nothing-suspended)))

(defn local
  "Read local NAME from frame IDX of the suspended thread — a real JDI
  Value, not its printed form."
  ([conn name] (local conn name 0))
  ([{:keys [:assay.remote/pending]} name idx]
   (when-let [e @pending]
     (let [^com.sun.jdi.StackFrame f (nth (.frames (.thread e)) idx)]
       (when-let [v (.visibleVariableByName f (clojure.core/name name))]
         (.getValue f v))))))

(defn eval-str
  "Call toString() on local NAME in the suspended frame — the smallest
  useful form of CDT's eval-in-frame, and the thing self-attach cannot
  do at all.

  This is invokeMethod: it resumes the target thread to run toString and
  re-suspends it. Safe here ONLY because the debugger is a different
  process. Attempted from a self-attached VM it deadlocks."
  ([conn name] (eval-str conn name 0))
  ([{:keys [:assay.remote/vm :assay.remote/pending] :as conn} name idx]
   (when-let [e @pending]
     (let [t (.thread e)
           v (local conn name idx)]
       (cond
         (nil? v) {:assay.dbg/error :no-such-local :assay.dbg/name name}
         (not (instance? com.sun.jdi.ObjectReference v)) (str v)
         :else
         (let [^com.sun.jdi.ObjectReference o v
               ^com.sun.jdi.ClassType ct (.referenceType o)
               ^com.sun.jdi.Method m (first (.methodsByName ct "toString" "()Ljava/lang/String;"))]
           (str (.invokeMethod o ^com.sun.jdi.ThreadReference t m
                               (java.util.ArrayList.) (int 0)))))))))

(defn box
  "Box V into the target VM by calling valueOf there. This is what
  self-attach could not do — the whole reason assay.control needed the
  Carrier field-read trick."
  [{:keys [:assay.remote/vm :assay.remote/pending]} v]
  (when-let [e @pending]
    (let [t (.thread e)
          [cls sig prim]
          (cond (integer? v) ["java.lang.Long" "(J)Ljava/lang/Long;" (.mirrorOf vm (long v))]
                (float? v) ["java.lang.Double" "(D)Ljava/lang/Double;" (.mirrorOf vm (double v))]
                (boolean? v) ["java.lang.Boolean" "(Z)Ljava/lang/Boolean;" (.mirrorOf vm (boolean v))]
                (string? v) [nil nil (.mirrorOf vm ^String v)]
                :else [nil nil nil])]
      (if (nil? cls)
        prim
        (let [^com.sun.jdi.ClassType ct (first (.classesByName vm cls))
              ^com.sun.jdi.Method m (first (.methodsByName ct "valueOf" sig))]
          (.invokeMethod ct ^com.sun.jdi.ThreadReference t m
                         (java.util.ArrayList. ^java.util.Collection [prim]) (int 0)))))))

(defn- register-boxes!
  "Make the failing class's own loader an INITIATING loader for the boxed
  types, by invoking Class.forName IN THE TARGET.

  JDI resolves a frame's return type through the declaring class's
  loader, searching the classes it initiated. A Clojure DynamicClassLoader
  never initiates java.lang.Object, so forceEarlyReturn rejects every
  non-null value with ClassNotLoadedException.

  assay.control fixes this by calling Class/forName locally — it can,
  because it IS the target. From here the same call has to be made
  remotely, which is exactly the capability a separate process buys."
  [{:keys [:assay.remote/vm]} ^com.sun.jdi.ReferenceType rt ^com.sun.jdi.ThreadReference t]
  (try
    (let [loader (.classLoader rt)
          ^com.sun.jdi.ClassType klass (first (.classesByName vm "java.lang.Class"))
          ^com.sun.jdi.Method for-name
          (first (.methodsByName klass "forName"
                                 "(Ljava/lang/String;ZLjava/lang/ClassLoader;)Ljava/lang/Class;"))]
      (doseq [c ["java.lang.Object" "java.lang.String" "java.lang.Long"
                 "java.lang.Double" "java.lang.Boolean" "java.lang.Number"]]
        (.invokeMethod klass t for-name
                       (java.util.ArrayList. ^java.util.Collection
                        [(.mirrorOf vm ^String c) (.mirrorOf vm false) loader])
                       (int 0)))
      true)
    (catch Throwable _ false)))

(defn use-value!
  "Restart: make the innermost frame matching FN-PREFIX return VALUE.

  The ordering here is all hard-won. Every invokeMethod RESUMES and
  re-suspends the target thread, which invalidates every StackFrame
  obtained before it. So: find the frame, take what survives a resume
  (the ReferenceType), do all the invoking, then re-fetch frames."
  [{:keys [:assay.remote/vm :assay.remote/pending] :as conn} fn-prefix value]
  (if-let [e @pending]
    (let [t (.thread e)
          find-idx (fn [] (first (keep-indexed
                                  #(when (str/starts-with? (fname %2) (str fn-prefix)) %1)
                                  (.frames t))))
          idx (find-idx)]
      (if-not idx
        {:assay.dbg/error :no-frame-matching :assay.dbg/prefix fn-prefix}
        ;; A ReferenceType survives a resume; a StackFrame does not.
        ;; Register against the loader of the frame we will RETURN FROM —
        ;; frame 0 is usually clojure.lang.*, whose app loader already
        ;; sees Object, so registering there fixes nothing.
        (let [rt    (.declaringType (.location (nth (vec (.frames t)) idx)))
              where (.name rt)]
          (register-boxes! conn rt t)
          (let [mirrored (when (some? value) (box conn value))
                frames   (vec (.frames t))          ; re-fetch: invalidated
                idx      (find-idx)]
            (when (pos? idx) (.popFrames t (nth frames (dec idx))))
            (.forceEarlyReturn t mirrored)
            (.resume t)
            (reset! pending nil)
            #:assay.dbg{:restarted where :returned value}))))
    :nothing-suspended))

(defn detach [{:keys [:assay.remote/vm]}] (.dispose vm) :detached)
