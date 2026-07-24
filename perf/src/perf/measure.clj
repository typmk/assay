(ns perf.measure
  "The MEASURE verbs — the ones that RUN your code.

  Separated from perf.code by BLAST RADIUS, the axis worth separating on.
  Reading what the compiler decided (perf.code: notes, types, expand,
  bytecode) cannot hurt you. These EXECUTE: `weigh` runs a form and its
  rewrite in timed loops, `native` spawns a fresh JVM and drives a fn hot,
  `fix` calls weigh. Which namespace you are in tells you what invoking
  something can do.

  All three produce facts the way perf.code does — derived or measured
  live, never remembered. The number weigh returns is the only measured
  magnitude in the toolkit, and it is computed now, for your form."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [perf.capability :as cap]
            [perf.code :as code]))

;; ── rung 4: native ────────────────────────────────────────────────

(defn- munge-method
  "demo.hot/tripled -> demo/hot$tripled.*

  Two things the obvious version gets wrong, both of which the VM rejects
  outright rather than ignoring:

  HotSpot's method pattern separates package from class with '/', not '.'.
  Emitting demo.hot$tripled.invokeStatic gives 'Method pattern uses
  multiple . in pattern' and the JVM refuses to start.

  And the method is wildcarded because which one carries the body depends
  on the hints: a fn with primitive args compiles to invokePrim, without
  them to invokeStatic, and asking for the wrong one prints nothing at all
  while looking like it worked."
  [qualified-sym]
  (str (str/replace (munge (namespace qualified-sym)) "." "/")
       "$" (munge (name qualified-sym)) ".*"))

(defn native
  "Rung 4 — the x86 HotSpot actually emitted for FN-SYM.

  This is the one rung that cannot be done in-process. PrintAssembly is a
  VM flag read at startup, so the JVM you are sitting in was either
  launched with it or cannot be made to comply. So: a fresh JVM, the
  current classpath, the function driven hot enough to reach C2, and
  -XX:CompileCommand=print for that method only.

  CompileCommand=print WITHOUT PrintAssembly is deliberate. With both, the
  VM warns and dumps every method it compiles — thousands of lines of
  java.lang startup before yours.

  FN-SYM must be loadable from the classpath: a fresh JVM cannot see a var
  you defined at the REPL. That is a real constraint, not an oversight.

    (code/native 'my.ns/hot [42])"
  ([fn-sym args] (native fn-sym args {}))
  ([fn-sym args {:keys [warmup] :or {warmup 200000}}]
   (cap/require! :hsdis)
   (let [lib (str (System/getProperty "user.home") "/.clojure/dev/lib")
         driver (pr-str
                 `(do (require '~(symbol (namespace fn-sym)))
                      (let [f# (resolve '~fn-sym)]
                        (dotimes [_# ~warmup] (apply f# ~args))
                        (println "perf.code: warmed"))))
         ;; bound to a hinted local: a ^java.util.List tag on the vector
         ;; LITERAL does not reach constructor resolution, so the ctor
         ;; stayed reflective — in the namespace whose job is reporting
         ;; reflection.
         ^java.util.List cmd ["java" "-XX:+UnlockDiagnosticVMOptions"
                              (str "-XX:CompileCommand=print," (munge-method fn-sym))
                              "-cp" (System/getProperty "java.class.path")
                              "clojure.main" "-e" driver]
         pb (doto (ProcessBuilder. cmd) (.redirectErrorStream true))]
     (.put (.environment pb) "LD_LIBRARY_PATH" lib)
     (let [p (.start pb)
           out (slurp (.getInputStream p))]
       ;; bounded wait + destroy: a wedged or slow warmup would otherwise
       ;; hang the caller and orphan the child JVM. (redirectErrorStream is
       ;; already set, so no stdout/stderr deadlock.)
       (when-not (.waitFor p 120 java.util.concurrent.TimeUnit/SECONDS)
         (.destroyForcibly p))
       (if (str/includes? out "Could not load hsdis")
         {:perf/error :hsdis-not-loaded :perf/searched lib}
         out)))))


;; ── weighing the alternative ──────────────────────────────────────
;;
;; `notes` carries no cost number; it ranks by mechanism. A stored
;; Those are real measurements, but they are measurements of OTHER code —
;; a remembered constant presented next to your form as though it were
;; about your form. That is rank 4 wearing a rank 1 costume.
;;
;; The compiler gives enough to do better. A boxed-math warning names the
;; exact overload it chose:
;;
;;   clojure.lang.Numbers.unchecked_add(java.lang.Object,java.lang.Object)
;;
;; which tells you both the operation and that it took the Object path, so
;; the cheaper path is the primitive overload of the same method. A
;; reflection warning names the member but NOT the receiver type — that is
;; exactly what it could not work out. The runtime values supply it: the
;; class of the argument you pass is what the compiler would have resolved
;; against, had it known.
;;
;; So: generate the alternative, VERIFY it by recompiling and checking the
;; notes actually went away, then measure both. The rewrite is not trusted
;; because it looks right; it is trusted because the compiler stopped
;; complaining and the clock agreed.

(defn- tag-for
  "The hint the compiler would have wanted for a value of this class.
  Clojure has exactly two primitive hints for fn params — ^long and
  ^double. Everything else hints as its class."
  [^Class c]
  (condp = c
    Long 'long, Integer 'long, Short 'long, Byte 'long
    Double 'double, Float 'double
    (symbol (.getName c))))

(defn- primitive-tag? [t] (#{'long 'double} t))

(defn fn-parts
  "Decompose a (fn name? [params] body...) form into {:head :name :params
  :body}, where :body is the SEQ of body forms. The ONE fn-form parser:
  hint-fn and param-tags here, and forms' fn-body/fn-rebody, had each
  open-coded this same anatomy four ways. Homed in measure because forms
  already requires it and measure precedes it in build order — no new edge."
  [form]
  (let [[head & more] form
        named? (symbol? (first more))]
    {:head head
     :name (when named? (first more))
     :params (if named? (second more) (first more))
     :body (if named? (nnext more) (next more))}))

(defn- hint-fn
  "Rewrite (fn [a b] body) as (fn ^ret [^t1 a ^t2 b] body), taking the
  tags from the classes of ARGS and of the value the original returned."
  [form arg-classes ret-class]
  (let [{:keys [head name params body]} (fn-parts form)
        tagged (mapv (fn [p c] (vary-meta p assoc :tag (tag-for c)))
                     params arg-classes)
        ret-tag (tag-for ret-class)
        params' (if (primitive-tag? ret-tag)
                  (vary-meta tagged assoc :tag ret-tag)
                  tagged)]
    (concat [head] (when name [name]) [params'] body)))

(defn- param-tags [form]
  (mapv #(:tag (meta %)) (:params (fn-parts form))))

(defn- num? [x] (number? x))
(defn- vecnum? [x] (and (sequential? x) (every? number? x)))

(defn- ->samples
  "Expand a single arg-tuple into a POOL of tuples the JIT cannot constant-
  fold. The red-team's teardown: a fixed sample arg let HotSpot fold
  (f 3.0) to a literal and a whole pow-vs-mul benchmark measured 0ns. When
  the args are numeric — bare numbers or seqs of numbers — perturb them
  type-preservingly (integers by +k, floats by ×(1+0.03k)) into 8 distinct
  tuples. Non-numeric args cannot be safely perturbed, so they pass through
  as one tuple and weigh flags :fold-risk."
  [args]
  (let [jit (fn [a k]
              (cond (integer? a)   (+ (long a) (long k))
                    (num? a)       (* (double a) (+ 1.0 (* 0.03 k)))
                    (vecnum? a)    (mapv (fn [x] (if (integer? x) (+ (long x) (long k))
                                                     (* (double x) (+ 1.0 (* 0.03 k))))) a)
                    :else a))]
    (if (every? #(or (num? %) (vecnum? %)) args)
      (mapv (fn [k] (mapv #(jit % k) args)) (range 8))
      [(vec args)])))

(defn- alloc-bytes
  "Bytes THIS thread allocated running F once, via the ThreadMXBean counter.
  Kept local to measure — the perf facade and capture read the same counter,
  but depending on either from here would drag their weight onto the measure
  path for two lines of MXBean access. Duplication is the cheaper trade."
  ^long [f]
  (let [tmx ^com.sun.management.ThreadMXBean
        (java.lang.management.ManagementFactory/getThreadMXBean)
        tid (.getId (Thread/currentThread))
        b0 (.getThreadAllocatedBytes tmx tid)]
    (f)
    (- (.getThreadAllocatedBytes tmx tid) b0)))

(defn- runner
  "Eval a no-arg fn that drives FN-FORM over the rotated sample POOL exactly
  REPS times, sinking every result into a primitive accumulator it returns.
  Rotation defeats constant-folding; the returned sink defeats dead-code
  elimination — the two failures the red-team's teardown found in the old
  harness (a fixed arg let HotSpot fold the call to 0ns). The fingerprint
  is zero-alloc for numeric returns (Double/hashCode over a primitive), so
  it does not pollute the allocation measurement.

  One eval'd fn with direct-arity calls (no apply, no closure over the
  args) so a primitive-hinted param keeps the invokePrim path — an earlier
  thunk-closure boxed them and measured 0.14x."
  [fn-form sample-tuples reps sink]
  (let [n (count sample-tuples)
        k (count (first sample-tuples))
        i (gensym "i_") tv (gensym "tv_") t (gensym "t_") acc (gensym "acc_")
        f (gensym "f_")
        call `(~f ~@(map (fn [j] `(nth ~t ~j)) (range k)))
        fold (case sink
               :double `(unchecked-add ~acc (long (Double/hashCode (double ~call))))
               :long   `(unchecked-add ~acc (Long/hashCode (long ~call)))
               `(unchecked-add ~acc (long (System/identityHashCode ~call))))]
    (binding [*warn-on-reflection* false *unchecked-math* false]
      (eval `(fn []
               (let [~tv ~(mapv vec sample-tuples) ~f ~fn-form]
                 (loop [~i 0 ~acc 0]
                   (if (< ~i ~reps)
                     (let [~t (nth ~tv (rem ~i ~n))] (recur (unchecked-inc ~i) ~fold))
                     ~acc))))))))

(defn- bench-form
  "Median-free single timed pass: warm, then time REPS driven iterations,
  return ns/call. weigh calls this per trial and takes the median across
  trials — one shot is an anecdote."
  [fn-form sample-tuples reps sink]
  (let [warm  (runner fn-form sample-tuples 200000 sink)
        drive (runner fn-form sample-tuples reps sink)]
    (warm)
    (let [t0 (System/nanoTime) r (drive) e (- (System/nanoTime) t0)]
      (when (== (double r) ##Inf) (println r))     ; observe the sink
      (/ (double e) reps))))

(defn- alloc-per-call
  "Bytes FN-FORM allocates per call, averaged over the sample pool via
  direct-arity driven calls — so only the fn's own allocation counts, not
  apply's arg-seq. The :double/:long sink allocates nothing, so a
  zero-allocation primitive rewrite reads as zero."
  ^long [fn-form sample-tuples sink]
  (let [reps  (max (count sample-tuples) 2000)
        drive (runner fn-form sample-tuples reps sink)]
    (drive)                                        ; load classes first
    (quot (alloc-bytes drive) reps)))

(defn- sink-for [ret]
  (cond (and (number? ret) (not (integer? ret))) :double
        (integer? ret) :long
        :else :object))

;; The driven timing loop (arg fetch by index + result sink) is not free.
;; The red-team measured its floor at ~13-14ns: an arg-independent constant
;; form reported 13.46ns. Below this, absolute ns is harness-dominated.
(def ^:private driver-floor-ns 15.0)

(defn- round2 ^double [x]
  ;; (double x) because Math/round is overloaded on float and double and
  ;; a Number satisfies neither — reflective, in the namespace that
  ;; reports reflection, for the second time in this file.
  (/ (Math/round (* 100.0 (double x))) 100.0))

(defn same?
  "Do two results agree? Numbers compare by VALUE with a float tolerance —
  clojure.core/= makes (= 6 6.0) false and a primitive rewrite legitimately
  returns 6.0 where the boxed original returned 6, so raw = would report a
  correct rewrite as changing the answer. Sequentials compare element-wise;
  everything else with =.

  The ONE numeric-tolerant equality for the toolkit: weigh's same-result
  check AND forms' outcome oracle. It lived twice (measure/same? scalar-only,
  forms/num=? with the sequential branch) and the copies had already
  diverged; this is the superset, homed here because forms already requires
  measure."
  [a b]
  (cond
    (and (number? a) (number? b))
    (or (== a b) (< (Math/abs (- (double a) (double b))) 1e-9))
    (and (sequential? a) (sequential? b) (= (count a) (count b)))
    (every? true? (map same? a b))
    :else (= a b)))

(defn weigh
  "Measure the form as written against the form the compiler wanted, using
  ARGS as the sample call.

  Returns the factor for THIS code, not a remembered constant. The
  `:verified` key is the load-bearing one: it is true only when the
  rewrite actually silenced the notes, so a rewrite that looked plausible
  but changed nothing reports itself instead of quietly inflating.

    (code/weigh '(fn [a b] (+ a b)) [3 4])

  When the compiler is SILENT — idiomatic Clojure over seqs, persistent
  vectors and boxed collection math produces no note to derive a rewrite
  from, which is exactly where the biggest wins hide — pass your own
  candidate as :against. weigh then holds it to the same bar it holds an
  auto-derived rewrite: same result (numeric tolerance), timed against the
  original over TRIALS, verdict gated on the spread.

    (weigh '(reduce + (map * a b)) '{...}      ; the boxed original
           {:against '(let [^doubles a a ...] ...)})  ; your primitive rewrite"
  ([form args] (weigh form args {}))
  ([form args {:keys [reps trials against] :or {reps 2000000 trials 5}}]
   (if against
     ;; EXPLICIT-alternative path. The notes gate does not apply: the whole
     ;; point is to measure a rewrite the compiler had nothing to say about.
     ;; The discipline is unchanged — the clock decides, same-result guards
     ;; correctness, the spread decides whether the run can tell at all.
     (let [f   (binding [*warn-on-reflection* false *unchecked-math* false] (eval form))
           g   (binding [*warn-on-reflection* false *unchecked-math* false] (eval against))
           ret (apply f args) ret' (apply g args)
           samples (->samples args)
           sink (sink-for ret)
           pairs (vec (for [_ (range trials)]
                        [(bench-form form samples reps sink)
                         (bench-form against samples reps sink)]))
           med (fn [xs] (nth (sort xs) (quot (count xs) 2)))
           factors (mapv (fn [[a b]] (/ a b)) pairs)
           lo (apply min factors) hi (apply max factors)
           a-form (alloc-per-call form samples sink)
           a-alt  (alloc-per-call against samples sink)
           as-w (round2 (med (map first pairs)))
           alt  (round2 (med (map second pairs)))]
       #:perf.weigh{:as-written-ns  as-w
                    :alternative-ns alt
                    :factor (round2 (med factors))
                    :factor-range [(round2 lo) (round2 hi)]
                    :as-written-bytes a-form
                    :alternative-bytes a-alt
                    :bytes-saved (- a-form a-alt)
                    :trials trials
                    :verdict (cond (> lo 1.05) :perf.weigh/alternative-is-faster
                                   (< hi 0.95) :perf.weigh/alternative-is-slower
                                   :else :perf.weigh/inconclusive)
                    :basis :perf.cost.basis/measured
                    :reps reps
                    :samples-used (count samples)
                    :fold-risk (< (count samples) 2)
                    ;; the driver loop (arg fetch + sink) has a ~13ns floor —
                    ;; red-team measured it. Below it, ABSOLUTE ns is
                    ;; harness-dominated (the ratio still cancels the common
                    ;; overhead, so :factor stays meaningful). Say so.
                    :below-floor? (< (min as-w alt) driver-floor-ns)
                    :same-result (same? ret ret')
                    :alternative against})
   (let [before (code/notes* form)]
     (if (empty? before)
       #:perf.weigh{:notes-before 0
                    :verdict :perf.weigh/nothing-to-compare}
       (let [f (binding [*warn-on-reflection* false] (eval form))
             ret (apply f args)
             rewrite (hint-fn form (mapv class args) (class ret))
             after (code/notes* rewrite)
             samples (->samples args)
             sink (sink-for ret)
             ;; TRIALS, not one shot. Three runs of one small form gave
             ;; 1.06, 0.79 and 1.54 — one of them claiming the FIXED
             ;; version was slower. For a difference this small the noise
             ;; exceeds the effect, and a single confident number would be
             ;; the exact failure this tool exists to prevent. So:
             ;; alternate the two, take medians, and report the spread.
             pairs (vec (for [_ (range trials)]
                          [(bench-form form samples reps sink)
                           (bench-form rewrite samples reps sink)]))
             med (fn [xs] (nth (sort xs) (quot (count xs) 2)))
             factors (mapv (fn [[a b]] (/ a b)) pairs)
             as-written (med (map first pairs))
             rewritten (med (map second pairs))
             lo (apply min factors) hi (apply max factors)]
         #:perf.weigh{:as-written-ns (round2 as-written)
                      :rewritten-ns (round2 rewritten)
                      :factor (round2 (med factors))
                      :factor-range [(round2 lo) (round2 hi)]
                      :trials trials
                      ;; The verdict is about RESOLUTION, not just size: if
                      ;; the spread straddles parity the run cannot tell
                      ;; you which is faster, and should say so rather
                      ;; than quote its median with a straight face.
                      :verdict (cond (> lo 1.05) :perf.weigh/hinting-is-faster
                                     (< hi 0.95) :perf.weigh/hinting-is-slower
                                     :else :perf.weigh/inconclusive)
                      :basis :perf.cost.basis/measured
                      :reps reps
                      :notes-before (count before)
                      :notes-after (count after)
                      :verified (zero? (count after))
                      :same-result (= ret (apply (binding [*warn-on-reflection* false]
                                                    (eval rewrite)) args))
                      ;; tags spelled out: they live in metadata, which
                      ;; pr-str hides, so the rewrite prints identical to
                      ;; the original and looks like nothing happened.
                      :hints (param-tags rewrite)
                      :rewrite rewrite}))))))


(defn cost
  "Absolute cost of FN-FORM called with ARGS: bytes and ns PER CALL, measured
  through the SAME direct-arity driver weigh uses — no apply-boxing (a
  primitive-hinted param keeps its invokePrim path), the result sunk so a
  strict body's allocation is counted and a zero-alloc primitive reads zero,
  the sample pool jittered so HotSpot cannot constant-fold. This is the ONE
  honest single-form cost primitive; do not hand-roll another (an earlier
  #(apply f args) thunk added a ~120 B ChunkedSeq floor to every form).

  ns comes from bench-form (200k-iteration warmup → C2 steady state), so
  bytes and ns are read in the SAME regime. QUICK? skips the timed pass
  (bytes only) for callers that must be cheap.

  A LAZY return is NOT silently realised — forcing it would count realisation
  the caller has not asked for. Instead :perf.cost/lazy? flags it, so a
  measured `(map * a b)` reports honestly that the number is construction
  cost, not the traversal."
  ([fn-form args] (cost fn-form args {}))
  ([fn-form args {:keys [quick? reps] :or {reps 200000}}]
   (let [samples (->samples args)
         ret     (apply (eval fn-form) (first samples))     ; type the sink
         sink    (sink-for ret)
         lazy?   (and (instance? clojure.lang.IPending ret)
                      (not (realized? ^clojure.lang.IPending ret)))
         bytes   (alloc-per-call fn-form samples sink)]
     (cond-> #:perf.cost{:bytes bytes :sink sink}
       lazy?        (assoc :perf.cost/lazy? true)
       (not quick?) (assoc :perf.cost/ns (bench-form fn-form samples reps sink))))))

;; ── applying it ───────────────────────────────────────────────────

(defn fix
  "The verified rewrite for FORM, as data AND as source text.

  rustc emits a suggested replacement with an applicability, and
  `cargo fix` applies it. Every suggestion here carried an
  `:applicability` copied from rustc with nothing on the other end to act
  on it. `weigh` already produces a rewrite that recompiled clean and
  returned the same value; this is that rewrite, handed back in a form you
  can paste or an editor can insert.

  `:source` exists because the hints live in METADATA, which pr-str hides
  — the rewrite prints identical to the original and looks like nothing
  happened. Printed with *print-meta*, the ^long shows up.

  nil when there is nothing to fix, or when the rewrite did not verify."
  ([form args] (fix form args {}))
  ([form args opts]
   (let [w (weigh form args (merge {:trials 1 :reps 200000} opts))]
     (when (:perf.weigh/verified w)
       #:perf.fix{:form (:perf.weigh/rewrite w)
                  ;; Reader metadata stripped: every form carries :line and
                  ;; :column, and printing those beside the ^long turns a
                  ;; paste-able suggestion into a mess.
                  :source (binding [*print-meta* true]
                            (pr-str (walk/postwalk
                                     (fn [x]
                                       (if-let [m (meta x)]
                                         (let [keep (select-keys m [:tag])]
                                           (with-meta x (not-empty keep)))
                                         x))
                                     (:perf.weigh/rewrite w))))
                  :hints (:perf.weigh/hints w)
                  :same-result (:perf.weigh/same-result w)
                  :notes-before (:perf.weigh/notes-before w)
                  :notes-after (:perf.weigh/notes-after w)}))))

