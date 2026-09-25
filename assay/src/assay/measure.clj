(ns assay.measure
  "The MEASURE verbs — the ones that RUN your code.

  Separated from assay.code by BLAST RADIUS, the axis worth separating on.
  Reading what the compiler decided (assay.code: notes, types, expand,
  bytecode) cannot hurt you. These EXECUTE: `weigh` runs a form and its
  rewrite in timed loops, `native` spawns a fresh JVM and drives a fn hot,
  `fix` calls weigh. Which namespace you are in tells you what invoking
  something can do.

  All three produce facts the way assay.code does — derived or measured
  live, never remembered. The number weigh returns is the only measured
  magnitude in the toolkit, and it is computed now, for your form."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [assay.capability :as cap]
            [assay.code :as code]))

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
                        (println "assay.code: warmed"))))
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
         {:assay/error :hsdis-not-loaded :assay/searched lib}
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

;; WHICH primitive hints exist is not remembered here, it is read off
;; clojure.lang.IFn. Clojure emits one nested interface per primitive
;; signature it supports — IFn$LO, IFn$DDL and so on — where each letter
;; is a parameter or return type. Collect the distinct letters across all
;; 358 of them and you get exactly #{L D O}: long, double, Object. That is
;; the runtime stating its own capability, and it stays correct if a
;; future Clojure adds a third.
(def ^:private prim-hints
  (delay
    (let [letters (->> (.getClasses clojure.lang.IFn)
                       (map #(.getSimpleName ^Class %))
                       (filter #(re-matches #"[LDO]+" %))
                       (mapcat seq)
                       set)]
      (cond-> #{}
        (letters \L) (conj 'long)
        (letters \D) (conj 'double)))))

(defn- tag-for
  "The hint the compiler would have wanted for a value of this class.
  The primitive hints come from `prim-hints` (read off IFn); which of
  them a boxed class widens to comes from the JVM's own numeric tower —
  integral boxes widen to the integral hint, floating to the floating
  one. Everything else hints as its class."
  [^Class c]
  (let [hints @prim-hints
        integral? (contains? #{Long Integer Short Byte} c)
        floating? (contains? #{Double Float} c)]
    (cond (and integral? (hints 'long))   'long
          (and floating? (hints 'double)) 'double
          :else (symbol (.getName c)))))

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

(defn allocated
  "EXACT bytes THIS thread allocated calling F once — the ThreadMXBean
  counter, not sampled. Warm it first: a cold run allocates for classloading
  and JIT. The one implementation: the assay facade delegates here, which
  costs the facade nothing since it already requires measure."
  ^long [f]
  (let [tmx ^com.sun.management.ThreadMXBean
        (java.lang.management.ManagementFactory/getThreadMXBean)
        b0 (.getCurrentThreadAllocatedBytes tmx)]
    (f)
    (- (.getCurrentThreadAllocatedBytes tmx) b0)))

;; ── the harness is criterium's; only the thunk is ours ────────────
;;
;; What used to live here was a hand-rolled benchmarking engine: its own
;; warmup, its own iteration count, its own median-of-trials, its own
;; dead-code and constant-fold defences. That is JMH's and criterium's
;; job, both of which do it better and have been attacked by more people.
;; The red-team broke this harness twice (a fixed sample arg let HotSpot
;; fold a call to 0ns), it carried a frozen 15ns floor from another
;; machine, and it measured a ~42ns floor here — high enough that boxed
;; (+ a b) against a primitive rewrite came back unresolvable.
;;
;; So it is gone, and criterium — already a global dep, already behind
;; assay/bench — owns warmup, sample count, bootstrapped confidence
;; intervals and outlier detection.
;;
;; EXACTLY TWO THINGS STAY, because criterium cannot do them:
;;
;;   1. DIRECT-ARITY CALLS. criterium takes a thunk. Reaching a fn through
;;      `apply` boxes its arguments, which is the precise property being
;;      measured — an earlier closure-thunk did that and reported 0.14x.
;;      So the thunk is eval'd with the call spelled out.
;;   2. THE SAMPLE POOL. One fixed argument is constant-foldable. The
;;      thunk walks the whole pool per invocation, so the per-call figure
;;      is criterium's mean divided by the pool size.

(defn- pooled-thunk
  "A no-arg fn walking the sample POOL once with direct-arity calls,
  folding each result into a primitive accumulator it returns. Hand this
  to criterium; divide its mean by (count sample-tuples)."
  [fn-form sample-tuples sink]
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
      (eval `(let [~tv ~(mapv vec sample-tuples) ~f ~fn-form]
               (fn []
                 (loop [~i 0 ~acc 0]
                   (if (< ~i ~n)
                     (let [~t (nth ~tv ~i)] (recur (unchecked-inc ~i) ~fold))
                     ~acc))))))))

(defn- bench*
  "ns per call and a bootstrapped 95% interval, from criterium.
  Returns [mean-ns lo-ns hi-ns]."
  [fn-form sample-tuples sink]
  (let [th (pooled-thunk fn-form sample-tuples sink)
        n  (count sample-tuples)
        r  ((code/resolve! 'criterium.core/quick-benchmark*) th {})
        [m [lo hi]] (:mean r)
        per (fn [s] (/ (* 1e9 (double s)) n))]
    [(per m) (per lo) (per hi)]))

;; ── ABBA, because order is a confound ─────────────────────────────
;;
;; Benchmarking arm A and then arm B is not a fair comparison: the JVM is
;; warmer, or hotter, or differently scheduled by the time B runs, and
;; that difference is charged to B. MEASURED: weighing a form against
;; ITSELF this way returned `alternative-is-slower` — a false positive on
;; the one input whose answer is known in advance, which is exactly the
;; failure this namespace exists to prevent. The paired design that
;; preceded criterium cancelled this by alternating within each trial;
;; delegating the statistics threw the cancellation out with it.
;;
;; So run the arms A B B A. Under drift that is linear across the run,
;; A's two measurements are centred at (t1+t4)/2 and B's at (t2+t3)/2 —
;; the same instant — so drift cancels in the means instead of landing on
;; whichever arm went second. The interval is the UNION of each arm's two
;; runs: two runs that disagree should widen the interval and make the
;; verdict inconclusive, which is the honest outcome when order and
;; effect cannot be told apart.

(defn- bench-counterbalanced
  "Both arms, measured A B B A. Returns [a b], each [mean lo hi]."
  [form-a form-b sample-tuples sink]
  (let [a1 (bench* form-a sample-tuples sink)
        b1 (bench* form-b sample-tuples sink)
        b2 (bench* form-b sample-tuples sink)
        a2 (bench* form-a sample-tuples sink)
        combine (fn [[m1 l1 h1] [m2 l2 h2]]
                  [(/ (+ m1 m2) 2.0) (min l1 l2) (max h1 h2)])]
    [(combine a1 a2) (combine b1 b2)]))

(defn- alloc-per-call
  "Bytes FN-FORM allocates per call, over one pass of the sample pool via
  direct-arity calls — so only the fn's own allocation counts, not apply's
  arg-seq. The :double/:long sink allocates nothing, so a
  zero-allocation primitive rewrite reads as zero."
  ^long [fn-form sample-tuples sink]
  (let [n     (count sample-tuples)
        drive (pooled-thunk fn-form sample-tuples sink)]
    (dotimes [_ 100] (drive))                      ; load classes first
    (quot (allocated drive) n)))

(defn- sink-for [ret]
  (cond (and (number? ret) (not (integer? ret))) :double
        (integer? ret) :long
        :else :object))

(defn- round2 ^double [x]
  ;; (double x) because Math/round is overloaded on float and double and
  ;; a Number satisfies neither — reflective, in the namespace that
  ;; reports reflection, for the second time in this file.
  (/ (Math/round (* 100.0 (double x))) 100.0))

;; ── self-calibration ──────────────────────────────────────────────
;;
;; ── how the verdict is decided, in four versions ──────────────────
;;
;; Each replaced the last, and the last one owns no statistics at all.
;;
;;   1. A frozen +/-5% band and a 15.0ns driver floor — measured once, on
;;      one machine, in a past run, and stamped into defs. Exactly the
;;      practice assay.code records having purged from note costs ("a
;;      rank-5 constant wearing a rank-1 label, in the tool built to
;;      catch exactly that"), applied to what the tool REPORTS but never
;;      to what decides how it reports.
;;   2. A calibration pass at first use, deriving the band from the
;;      harness weighed against itself. At least this machine — but still
;;      a constant, frozen 200ms ago instead of a year ago, measured at
;;      T0 and applied at T1 across which JIT state, frequency scaling
;;      and thermals drift. Cold it reported a 137.86ns floor against a
;;      known 13.46; warmed it claimed a 1.28 band on an idle 20-core
;;      box, asserting the machine could not resolve 28%.
;;   3. Paired trials, no band: alternate the two forms within each trial
;;      so common-mode drift cancels, and call it only when every pair
;;      agreed in sign. Sound, and constant-free.
;;   4. THIS. criterium bootstraps a confidence interval per arm and the
;;      verdict is whether the two intervals overlap.
;;
;; 3 was correct and still lost, which is the point worth recording. It
;; was a statistics engine written here — resampling, spread, an implied
;; significance level of 0.5^trials — competing with one that has had
;; far more eyes on it and does outlier detection and bootstrapping
;; properly. The paired design cancelled drift; criterium's interval
;; measures it. Neither needs a threshold, and only one of them is this
;; codebase's job to maintain.

(defn- verdict-of
  "Faster / slower / inconclusive from whether the two bootstrapped
  confidence intervals OVERLAP. No threshold: if the intervals are
  disjoint the run can tell them apart, and if they are not it cannot.
  criterium bootstraps the interval from the samples it took, so the
  resolution is a property of the measurement rather than a number
  chosen in advance."
  [[_ a-lo a-hi] [_ b-lo b-hi] faster slower]
  (cond (> a-lo b-hi) faster
        (> b-lo a-hi) slower
        :else :assay.weigh/inconclusive))

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
    ;; The tolerance is the ULP at the magnitude being compared, not a
    ;; 1e-9 literal. Two reasons, and the second is a bug the constant
    ;; was hiding: ulp is the actual resolution of a double, so this is
    ;; derived rather than chosen — and it SCALES. Measured: ulp(1e9) is
    ;; 1.19e-7, forty times larger than 1e-9, so two adjacent
    ;; representable doubles near a billion differ by more than the old
    ;; epsilon allowed and a legitimate rewrite was reported as changing
    ;; the answer. Near zero it tightens for the same reason.
    (or (== a b)
        (let [x (double a) y (double b)
              scale (Math/max (Math/abs x) (Math/abs y))]
          (<= (Math/abs (- x y)) (Math/ulp scale))))
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
  ([form args {:keys [against]}]
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
           [a b] (bench-counterbalanced form against samples sink)
           ;; the harness's own cost, measured in this run alongside the
           ;; two arms, not remembered from another machine
           floor (first (bench* '(fn [& _] 1) samples :long))
           a-form (alloc-per-call form samples sink)
           a-alt  (alloc-per-call against samples sink)
           as-w (round2 (first a))
           alt  (round2 (first b))]
       #:assay.weigh{:as-written-ns  as-w
                    :alternative-ns alt
                    :factor (round2 (/ (first a) (first b)))
                    ;; the ratio's range from the two INTERVALS, worst and
                    ;; best case, rather than from a spread of our own
                    :factor-range [(round2 (/ (nth a 1) (nth b 2)))
                                   (round2 (/ (nth a 2) (nth b 1)))]
                    :as-written-ci [(round2 (nth a 1)) (round2 (nth a 2))]
                    :alternative-ci [(round2 (nth b 1)) (round2 (nth b 2))]
                    :as-written-bytes a-form
                    :alternative-bytes a-alt
                    :bytes-saved (- a-form a-alt)
                    :verdict (verdict-of a b
                                         :assay.weigh/alternative-is-faster
                                         :assay.weigh/alternative-is-slower)
                    :basis :assay.cost.basis/measured
                    :engine :assay.engine/criterium
                    :samples-used (count samples)
                    :fold-risk (< (count samples) 2)
                    ;; Below the harness floor, ABSOLUTE ns is
                    ;; harness-dominated (the ratio still cancels the
                    ;; common overhead, so :factor stays meaningful).
                    :floor-ns (round2 floor)
                    :below-floor? (< (min as-w alt) floor)
                    :same-result (same? ret ret')
                    :alternative against})
   (let [before (code/notes* form)]
     (if (empty? before)
       #:assay.weigh{:notes-before 0
                    :verdict :assay.weigh/nothing-to-compare}
       (let [f (binding [*warn-on-reflection* false] (eval form))
             ret (apply f args)
             rewrite (hint-fn form (mapv class args) (class ret))
             after (code/notes* rewrite)
             samples (->samples args)
             sink (sink-for ret)
             ;; NOT ONE SHOT. Three runs of one small form once gave 1.06,
             ;; 0.79 and 1.54 — one of them claiming the FIXED version was
             ;; slower. For a difference this small the noise exceeds the
             ;; effect, and a single confident number is the exact failure
             ;; this tool exists to prevent. criterium answers that with a
             ;; bootstrapped interval per arm; the verdict is then simply
             ;; whether the two intervals overlap.
             [a b] (bench-counterbalanced form rewrite samples sink)]
         #:assay.weigh{:as-written-ns (round2 (first a))
                      :rewritten-ns (round2 (first b))
                      :factor (round2 (/ (first a) (first b)))
                      :factor-range [(round2 (/ (nth a 1) (nth b 2)))
                                     (round2 (/ (nth a 2) (nth b 1)))]
                      :as-written-ci [(round2 (nth a 1)) (round2 (nth a 2))]
                      :rewritten-ci [(round2 (nth b 1)) (round2 (nth b 2))]
                      ;; The verdict is about RESOLUTION, not just size: if
                      ;; the intervals overlap the run cannot tell you
                      ;; which is faster, and should say so rather than
                      ;; quote its point estimate with a straight face.
                      :verdict (verdict-of a b
                                           :assay.weigh/hinting-is-faster
                                           :assay.weigh/hinting-is-slower)
                      :basis :assay.cost.basis/measured
                      :engine :assay.engine/criterium
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

  ns comes from criterium through the same pooled direct-arity thunk the
  allocation figure uses, so bytes and ns are read in the SAME regime.
  QUICK? skips the timed pass (bytes only) for callers that must be cheap.

  A LAZY return is NOT silently realised — forcing it would count realisation
  the caller has not asked for. Instead :assay.cost/lazy? flags it, so a
  measured `(map * a b)` reports honestly that the number is construction
  cost, not the traversal."
  ([fn-form args] (cost fn-form args {}))
  ([fn-form args {:keys [quick?]}]
   (let [samples (->samples args)
         ret     (apply (eval fn-form) (first samples))     ; type the sink
         sink    (sink-for ret)
         lazy?   (and (instance? clojure.lang.IPending ret)
                      (not (realized? ^clojure.lang.IPending ret)))
         bytes   (alloc-per-call fn-form samples sink)]
     (cond-> #:assay.cost{:bytes bytes :sink sink}
       lazy?        (assoc :assay.cost/lazy? true)
       (not quick?) (assoc :assay.cost/ns (first (bench* fn-form samples sink)))))))

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
   (let [w (weigh form args opts)]
     (when (:assay.weigh/verified w)
       #:assay.fix{:form (:assay.weigh/rewrite w)
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
                                     (:assay.weigh/rewrite w))))
                  :hints (:assay.weigh/hints w)
                  :same-result (:assay.weigh/same-result w)
                  :notes-before (:assay.weigh/notes-before w)
                  :notes-after (:assay.weigh/notes-after w)}))))


;; ── handing off to JMH ────────────────────────────────────────────
;;
;; The division of labour after the harness was cut. assay's unique half is
;; DERIVING the alternative and VERIFYING it — the compiler disclosed what
;; it refused, the rewrite silenced the notes, the values still match.
;; None of that is a benchmark. criterium then gives an in-process number
;; good enough to rank candidates, which is what `discover` needs.
;;
;; What criterium in-process cannot give is fork isolation: a fresh JVM
;; per arm, so the first arm cannot warm, pollute the profile of, or
;; deoptimise the second. That is JMH's whole reason to exist, and it is
;; not worth rebuilding here — this session already deleted one hand-rolled
;; benchmarking engine.
;;
;; So `handoff` emits what a JMH harness needs and nothing more: the two
;; forms with their hints spelled out (the tags live in metadata and
;; pr-str hides them, so a rewrite prints identical to the original), the
;; sample args, and assay's own verdict for comparison. No JMH schema is
;; invented here — the shape a runner wants is the runner's business, and
;; guessing at one is how you ship something that never ran.

(defn handoff
  "Everything a JMH (or any out-of-process) harness needs for FORM and
  its verified rewrite, as data. nil when there is nothing to hand off.

    (measure/handoff '(fn [a b] (+ a b)) [3 4])"
  ([form args] (handoff form args {}))
  ([form args opts]
   (let [w (weigh form args opts)
         alt (or (:assay.weigh/rewrite w) (:assay.weigh/alternative w))]
     (when alt
       #:assay.handoff{:baseline form
                      :candidate alt
                      :hints (param-tags alt)
                      :args args
                      ;; source, because the hints are metadata and
                      ;; pr-str drops them — the candidate would print
                      ;; identical to the baseline and look like a no-op
                      :candidate-source (binding [*print-meta* true] (pr-str alt))
                      :verified (:assay.weigh/verified w)
                      :in-process #:assay.handoff{:engine (:assay.weigh/engine w)
                                                 :verdict (:assay.weigh/verdict w)
                                                 :factor (:assay.weigh/factor w)
                                                 :factor-range (:assay.weigh/factor-range w)}
                      :why "criterium measures in-process; JMH forks a JVM per arm"}))))
