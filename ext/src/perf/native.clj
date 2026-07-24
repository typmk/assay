(ns perf.native
  "Native safety in three tiers. The answer is FFM's design, not signals.

  MEASURED, and the ordering is the whole point:

    TIER 1 PREVENTION — FFM converts the classic native memory bugs into
      ordinary Java exceptions BEFORE memory is touched:
        out-of-bounds  -> IndexOutOfBoundsException (naming the segment's
                          real byteSize and the offending offset)
        use-after-free -> IllegalStateException \"Already closed\"
        wrong thread   -> WrongThreadException
      A MemorySegment carries bounds, an Arena lifetime and optional
      thread confinement. Nothing faults.

    TIER 2 RECOVERY — because those are Java exceptions, perf.control's
      restarts apply. Demonstrated end to end: an out-of-bounds native
      read caught pre-unwind, use-value! supplied 0, caller got value=0.

    TIER 3 ISOLATION — `guarded`, a separate process. Only for faults
      INSIDE the native library, where neither of the above can reach.

  There is no tier 4. The JVM REFUSES SIGSEGV/SIGFPE/SIGILL/SIGQUIT
  (\"Signal already used by VM or OS\") because HotSpot implements
  implicit null checks and safepoint polling with deliberate faults it
  catches itself. Taking that handler would break the JVM.

  A segfault is reachable only by opting OUT: MemorySegment/ofAddress
  returns a ZERO-LENGTH segment precisely so it cannot be read, and
  `reinterpret` is a restricted method whose purpose is to disclaim
  safety. Measured, doing that gave SIGSEGV, exit 134, and no Java
  exception at any point. That is the API working, not failing."
  (:require [perf.control :as control]))

(def exceptions
  "The family FFM raises INSTEAD of faulting — what to arm restarts for."
  ["java.lang.IndexOutOfBoundsException"
   "java.lang.IllegalStateException"
   "java.lang.WrongThreadException"])

(defn restarts!
  "Arm tier 2 for the FFM exception family."
  ([] (restarts! (first exceptions)))
  ([exception-class] (control/restarts! exception-class)))

(defn check
  "Tier 1, made explicit: is SEGMENT safe to read at OFFSET for SIZE?
  A pre-flight, rather than discovering it by throwing."
  [^java.lang.foreign.MemorySegment segment offset size]
  (let [sz (.byteSize segment)]
    (cond
      (not (.isAccessibleBy segment (Thread/currentThread)))
      {:safe? false :reason :wrong-thread}
      (not (.isAlive (.scope segment)))
      {:safe? false :reason :arena-closed}
      (or (neg? offset) (> (+ offset size) sz))
      {:safe? false :reason :out-of-bounds :byte-size sz :offset offset :needs size}
      :else {:safe? true :byte-size sz})))

(defn guarded
  "Tier 3: run FORM in a SEPARATE JVM so a native fault kills only that.

  This is not a workaround for a missing restart — the only agent that
  can survive a SIGSEGV and still report on it is a different process, so
  the OS boundary IS the restart, relocated to the one place that can
  enforce it."
  [form]
  (let [^java.util.List cmd ["clj" "-M" "-e" (pr-str `(println (pr-str ~form)))]
        p   (.start (ProcessBuilder. cmd))
        out (slurp (.getInputStream p))
        err (slurp (.getErrorStream p))
        rc  (.waitFor p)]
    (cond
      (zero? rc) {:ok (try (read-string (clojure.string/trim out))
                           (catch Throwable _ (clojure.string/trim out)))}
      ;; 128+n is death by signal n; 134 = SIGABRT, which is what the
      ;; JVM's fatal handler raises after SIGSEGV.
      (> rc 128) {:fault {:exit rc :signal (- rc 128)
                          :note (if (= rc 134)
                                  "SIGABRT — JVM fatal handler, usually after SIGSEGV"
                                  "died by signal")
                          :stderr (subs err 0 (min 400 (count err)))}}
      :else {:error {:exit rc :stderr (subs err 0 (min 400 (count err)))}})))
