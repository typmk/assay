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

(defn- hint-fn
  "Rewrite (fn [a b] body) as (fn ^ret [^t1 a ^t2 b] body), taking the
  tags from the classes of ARGS and of the value the original returned."
  [form arg-classes ret-class]
  (let [[hd & more] form
        [fname params body] (if (symbol? (first more))
                              [(first more) (second more) (nnext more)]
                              [nil (first more) (next more)])
        tagged (mapv (fn [p c] (vary-meta p assoc :tag (tag-for c)))
                     params arg-classes)
        ret-tag (tag-for ret-class)
        params' (if (primitive-tag? ret-tag)
                  (vary-meta tagged assoc :tag ret-tag)
                  tagged)]
    (concat [hd] (when fname [fname]) [params'] body)))

(defn- param-tags [form]
  (let [more (next form)
        params (if (symbol? (first more)) (second more) (first more))]
    (mapv #(:tag (meta %)) params)))

(defn- bench-form
  "Build and eval a self-contained timing loop for FN-FORM called on ARGS.

  The whole loop is generated as ONE form so the sample arguments stay
  let-locals in the same scope as the call. Two earlier versions got this
  wrong in opposite directions and both inverted the result:

    * closing over the args in a (fn [] (f a b)) thunk BOXES them —
      closed-over primitives become Object fields, so a primitive-hinted
      fn cannot take the invokePrim path and measured SLOWER than the
      boxed version it was supposed to beat (0.14x).
    * hinting those locals instead throws outright: you cannot ^long a
      local whose initialiser is already a primitive.

  Neither is a fact about Clojure's performance. Both were the harness
  measuring its own call site."
  [fn-form args reps]
  (let [syms (mapv (fn [i] (gensym (str "a" i "_"))) (range (count args)))]
    ;; warnings off while benchmarking: `notes` already reported them, and
    ;; re-emitting one per trial makes it look like the tool found five
    ;; problems instead of measuring one five times.
    (binding [*warn-on-reflection* false *unchecked-math* false]
      (eval `(fn []
             (let [f# ~fn-form
                   ~@(interleave syms (map (fn [a] `(quote ~a)) args))]
               (dotimes [_# 200000] (f# ~@syms))
               (let [t0# (System/nanoTime)]
                 (dotimes [_# ~reps] (f# ~@syms))
                 (/ (double (- (System/nanoTime) t0#)) ~reps))))))))

(defn- round2 ^double [x]
  ;; (double x) because Math/round is overloaded on float and double and
  ;; a Number satisfies neither — reflective, in the namespace that
  ;; reports reflection, for the second time in this file.
  (/ (Math/round (* 100.0 (double x))) 100.0))

(defn weigh
  "Measure the form as written against the form the compiler wanted, using
  ARGS as the sample call.

  Returns the factor for THIS code, not a remembered constant. The
  `:verified` key is the load-bearing one: it is true only when the
  rewrite actually silenced the notes, so a rewrite that looked plausible
  but changed nothing reports itself instead of quietly inflating.

    (code/weigh '(fn [a b] (+ a b)) [3 4])"
  ([form args] (weigh form args {}))
  ([form args {:keys [reps trials] :or {reps 2000000 trials 5}}]
   (let [before (code/notes* form)]
     (if (empty? before)
       #:perf.weigh{:notes-before 0
                    :verdict :perf.weigh/nothing-to-compare}
       (let [f (binding [*warn-on-reflection* false] (eval form))
             ret (apply f args)
             rewrite (hint-fn form (mapv class args) (class ret))
             after (code/notes* rewrite)
             ;; TRIALS, not one shot. Three runs of one small form gave
             ;; 1.06, 0.79 and 1.54 — one of them claiming the FIXED
             ;; version was slower. For a difference this small the noise
             ;; exceeds the effect, and a single confident number would be
             ;; the exact failure this tool exists to prevent. So:
             ;; alternate the two, take medians, and report the spread.
             pairs (vec (for [_ (range trials)]
                          [((bench-form form args reps))
                           ((bench-form rewrite args reps))]))
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
                      :rewrite rewrite})))))


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

