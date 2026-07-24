(ns perf.code
  "The ladder: what you wrote → what the compiler gave up on → what it
  emitted → what the CPU runs.

  Julia has this and it is the single best thing about working there:
  @code_lowered, @code_typed, @code_warntype, @code_llvm, @code_native —
  one macro per rung, same prefix, no build step. SBCL has the other half:
  efficiency notes that arrive automatically at compile time and carry
  COST NUMBERS, so you know a GENERIC-+ cost 10 and the inline float path
  cost 2.

  Clojure has the information and does not surface it. `*warn-on-reflection*`
  and `*unchecked-math* :warn-on-boxed` are the compiler telling you it had
  to take a slow path — the same signal SBCL emits — but they go to *err*
  as prose, unranked, one line at a time, and only if you remembered to
  bind them.

  So:

    (code/notes '(defn f [s] (.length s)))
    ;; => [#:perf.note{:code :perf.note/reflection :cost 202 ...}]

    (code/expand '(when x 1))       ; what the macros produced
    (code/types  form [3 4])        ; what it concluded about types
    (code/notes  form)              ; what it could not do, ranked by cost
    (code/java   form)              ; the Java it emitted
    (code/bytecode form)            ; the bytecode

  and the MEASURE verbs — the ones that run your code — live next door in
  perf.measure, separated by blast radius:

    (measure/weigh  form [3 4])     ; the cheap path vs the expensive one
    (measure/fix    form [3 4])     ; the verified rewrite, paste-able
    (measure/native 'my.ns/f [42])  ; the x86, via hsdis

  And the three gaps the comparison left open, closed the same way each
  time — by using information already in hand rather than adding surface:

    (code/watch!)                   ; notes for EVERYTHING compiled, as
                                    ; SBCL does by default. No compiler
                                    ; hook exists; the switches and the
                                    ; stream they write to are plain vars,
                                    ; so root-bind them and tee the stream.

    :perf.note/taken / :refused     ; SBCL prints the path taken beside
                                    ; the path refused. The boxed-math
                                    ; message names the overload chosen,
                                    ; and the JVM knows the others — so
                                    ; they are READ OFF THE CLASS, not
                                    ; remembered in a table.

    (code/fix form args)            ; rustc suggests and cargo fix APPLIES.
                                    ; Every suggestion here carried an
                                    ; :applicability copied from rustc with
                                    ; nothing on the other end. `weigh`
                                    ; already produced a rewrite that
                                    ; recompiled clean and returned the
                                    ; same value; `fix` hands it back as
                                    ; source you can paste.

  NOTES ARE RANKED, but by MECHANISM, not by a stored magnitude. SBCL
  prints notes in source order; a hot loop with one reflective call and
  nine boxed additions reads as ten equal complaints. Reflection resolves
  a method by name on every call; boxing allocates per op; reflection is
  categorically heavier, so it sorts first. That ordering is structural
  and stable — see `kind-rank`.

  There is NO stored cost number. An earlier version stamped every note
  with a remembered 202x / 1.25x tagged :measured — numbers measured once,
  on this machine, in a past run, and frozen. For any note today those are
  recalled, not measured. The measured factor for YOUR form is `weigh`'s
  job: it derives the alternative from what the compiler disclosed,
  verifies the rewrite by recompiling until the notes go silent, and times
  both, live. That number is the only measured one and it is never stored."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [perf.capability :as cap]))

;; ── note ordering ─────────────────────────────────────────────────
;;
;; This used to be a table of MEASURED magnitudes — reflection 202x,
;; boxing 1.25x — stamped onto every note with a :cost-basis :measured
;; tag. But those numbers were measured ONCE, on this machine, in a past
;; run, and frozen into a def. For any given note today they are recalled,
;; not measured: a rank-5 constant wearing a rank-1 label, in the tool
;; built to catch exactly that. Gone.
;;
;; What remains is one ordering, and it is STRUCTURAL, not a magnitude.
;; Reflection resolves a method by name and signature on every call — a
;; lookup plus an access check. Boxing allocates one small object per
;; operation. Reflection is categorically heavier by mechanism, stable
;; across machines, so notes sort reflection first. That is a rank-3
;; structural claim about how the two work, not "202x".
;;
;; The actual factor for YOUR form comes from `weigh`, which runs both the
;; form and its hinted rewrite and times them. That number is the only
;; measured one, it is derived live, and it is never stored.

(def ^:private kind-order
  [:perf.note/reflection :perf.note/boxed-math :perf.note/boxed-body])

(defn kind-rank
  "Ordinal for sorting notes worst-first, by MECHANISM (see kind-order) —
  not a magnitude. Lower is worse. The measured factor is `weigh`'s job."
  [note-code]
  (or (first (keep-indexed (fn [i k] (when (= k note-code) i)) kind-order))
      99))

;; ── naming ────────────────────────────────────────────────────────
;; Used by both the notes rung and the types rung, so it lives above
;; both rather than beside whichever one happened to need it first.

(def ^:private prim-names
  {Long/TYPE 'long Double/TYPE 'double Integer/TYPE 'int Float/TYPE 'float
   Boolean/TYPE 'boolean Character/TYPE 'char Byte/TYPE 'byte
   Short/TYPE 'short Void/TYPE 'void})

(defn- tname [^Class c] (or (prim-names c) (symbol (.getSimpleName c))))

;; ── rung 1: notes ─────────────────────────────────────────────────

(def ^:private warning-re
  ;; "Reflection warning, NO_SOURCE_PATH:9:50 - reference to field length
  ;;  can't be resolved."
  ;; "Boxed math warning, file.clj:13:48 - call: public static ..."
  #"(?m)^(Reflection|Boxed math) warning, ([^:]*):(\d+):(\d+) - (.*)$")

(def ^:private code-of
  {"Reflection" :perf.note/reflection
   "Boxed math" :perf.note/boxed-math})

;; ── what the compiler took, and what it could have taken ──────────
;;
;; SBCL prints "forced to do GENERIC-+ (cost 10)" beside "unable to do
;; inline float arithmetic (cost 2)" — the path taken against the path
;; refused, from its own cost model, free on every compile. That is a
;; better answer than a lookup table, and Clojure discloses enough to
;; reach it: a boxed-math warning names the exact overload chosen,
;;
;;   clojure.lang.Numbers.unchecked_add(java.lang.Object,java.lang.Object)
;;
;; and the JVM will tell you what the other overloads of that method are.
;; So the alternatives are READ OFF THE CLASS, not remembered. No cost
;; model of our own is invented — the fact reported is "you got the
;; Object overload; these primitive ones exist", which is checkable.

(def ^:private call-re
  ;; Anchored on the call at the END, not on a token count at the start:
  ;; the message is "call: public static java.lang.Number
  ;; clojure.lang.Numbers.unchecked_add(...)" and counting modifiers and
  ;; return types ahead of the class is a way to be wrong about a
  ;; signature you did not anticipate.
  #"([\w.$]+)\.(\w+)\(([^)]*)\)\s*\.?\s*$")

(defn- overloads
  "Every overload of CLS.METHOD, flagged with which one was selected."
  [cls method taken-params]
  (try
    (let [k (Class/forName cls)]
      (->> (.getDeclaredMethods k)
           (filter #(= method (.getName ^java.lang.reflect.Method %)))
           (map (fn [^java.lang.reflect.Method m]
                  (let [ps (mapv tname (.getParameterTypes m))]
                    #:perf.overload{:params ps
                                    :returns (tname (.getReturnType m))
                                    :taken? (= ps taken-params)
                                    :primitive? (every? '#{long double int float
                                                           boolean char byte short} ps)})))
           (sort-by (comp str :perf.overload/params))
           vec))
    (catch Throwable _ nil)))

(defn- alternatives
  "For a boxed-math message, the overload taken and the ones refused."
  [detail]
  (when-let [[_ cls method params] (re-find call-re detail)]
    (let [taken (mapv #(-> % (str/split #"\.") last symbol)
                      (remove str/blank? (str/split params #",")))
          all (overloads cls method taken)]
      (when (seq all)
        ;; Only the FULLY primitive overloads of the same arity. Numbers
        ;; carries every mixed pairing — (Object,long), (double,Object)
        ;; and six more — and listing all eight buries the two that are
        ;; the actual target. SBCL names the one alternative it wanted;
        ;; this names the ones that would remove the boxing entirely.
        #:perf.note{:taken (str method "(" (str/join "," taken) ")")
                    :refused (vec (for [o all
                                        :when (and (not (:perf.overload/taken? o))
                                                   (:perf.overload/primitive? o)
                                                   (= (count (:perf.overload/params o)) (count taken)))]
                                    (str method "(" (str/join "," (:perf.overload/params o))
                                         ")->" (:perf.overload/returns o))))}))))

(defn- parse-warnings [s]
  (for [[_ kind file line col detail] (re-seq warning-re s)
        :let [c (code-of kind)]]
    (merge
     #:perf.note{:code c
                 :severity :perf.severity/warning
                 :span {:perf/file (when-not (= file "NO_SOURCE_PATH") file)
                        :perf/line (parse-long line)
                        :perf/col (parse-long col)}
                 :message (str/replace detail #"\.$" "")
                 ;; No :cost. Everything on a note is the compiler's own
                 ;; string (:message), the emitted signature, or the
                 ;; overloads read off the class — derived, not stored. The
                 ;; measured factor is a `weigh` away, live.
                 }
      (alternatives detail))))


;; ── why this parses prose, which looks like laziness ───────────────
;;
;; It is fair to ask whether the JVM/Clojure world offers a structured
;; channel for this instead of a regex over *err*. It does — Eastwood and
;; clj-kondo exist — and it was tried here and rejected on measurement.
;;
;; clojure.tools.analyzer.jvm carries both facts in its typed AST:
;;
;;   (.length s)          :op :host-interop                  <- unresolved
;;   (.length ^String s)  :op :instance-call :validated? true :tag int
;;   (+ ^long a ^long b)  :op :static-call Numbers/add :tag long
;;
;; Three reasons it is not used, all measured on this machine:
;;
;;   1. IT IS LESS COMPLETE. On (fn [a b] (+ (* a b) a)) the analyzer gave
;;      :tag java.lang.Number for `multiply` and :tag NIL for `add` — no
;;      tag at all on a node where the compiler definitely emitted boxed
;;      math. It is a separate implementation of Clojure's analysis, and
;;      where the two disagree the compiler is right by definition.
;;   2. It is slower — 8.1 ms against 5.2 ms over the same four forms,
;;      which is the same reason Eastwood measured 73 ms against 4.4 ms.
;;   3. It costs four dependencies (tools.analyzer.jvm, core.memoize,
;;      core.cache, data.priority-map) to reimplement a fact the compiler
;;      already reports.
;;
;; So the string IS the API here, not for want of looking.
;;
;; THE ONE THING THE ANALYZER HAD, AND THE HAZARD IT EXPOSED: analysing a
;; form does not run it; `notes*` EVALUATES the form it is asked to
;; inspect. Measured — analysing (do (swap! a conj :x) ..) left the atom
;; empty, eval'ing it did not. For a defn or a fn that is harmless, and
;; those are what this is for. For a form with side effects it is not, and
;; the mitigation is one character: wrap it. (notes '(fn [] (spit ...)))
;; evaluates to a function and calls nothing.

(defn notes*
  "Compile FORM with every compiler advisory switched on and return what
  the compiler could not do, as data, ranked by measured cost.

  This is the SBCL move, and the mechanism is deliberately dumb: bind
  *warn-on-reflection* and *unchecked-math*, capture *err*, parse. Clojure
  emits these as prose and offers no structured channel — there is no
  compiler hook to subscribe to, so the string IS the API.

  NOT NEW. Eastwood has had :reflection and :boxed-math linters for years,
  on the same mechanism, and its `lint` already returns maps with file,
  line and column separated. What is kept here is the cost on each note
  and the ranking by it — Eastwood reports uniformly, with no severity —
  and form granularity, because Eastwood lints namespaces and a ladder
  rung has to take a form. For a whole project, use Eastwood.

  Measured against it on the same four functions: identical findings, five
  for five. Eastwood 73 ms to this 4.4 ms, which is not a point in this
  namespace's favour — it builds a full tools.analyzer AST and runs thirty
  linters off it, against two patterns grepped out of stderr here.

  Compiles in a THROWAWAY namespace. Evaluating a defn to inspect it
  should not define it in yours, and an earlier version did exactly that.

  IT EVALUATES THE FORM. That is how the compiler is made to report, and
  it means side effects in the form happen. Give it definitions — a defn
  or a fn — which is what it is for. To inspect something effectful,
  wrap it: (notes '(fn [] (spit path x))) builds a function and calls
  nothing. See the note above on why the analyzer, which would avoid
  this, is not used.

  Returns [] when there is nothing to say. A diagnostic that always fires
  is noise, and noise gets filtered wholesale."
  [form]
  (let [w (java.io.StringWriter.)
        tmp (create-ns (gensym "perf.code.probe"))]
    (try
      (binding [*warn-on-reflection* true
                *unchecked-math* :warn-on-boxed
                *err* w
                *ns* tmp]
        (refer-clojure)
        (try (eval form) (catch Throwable _ nil)))
      (finally (remove-ns (ns-name tmp))))
    (vec (sort-by #(kind-rank (:perf.note/code %)) (parse-warnings (str w))))))

(defmacro notes
  "Compile-time notes for FORM, ranked by cost. See `notes*`.

    (code/notes '(defn f [s] (.length s)))"
  [form]
  `(notes* ~(if (and (seq? form) (= 'quote (first form))) form `'~form)))

(defn explain
  "A thin conduit for the DERIVED strings on a note — the compiler's own
  message, the signature it emitted, the overloads it took and refused,
  and the cost number. perf authors none of this text; it passes through
  what the compiler and the class already said. The data is the data; if
  you want it rendered some other way, read the map. Primary interface is
  the map, not this."
  [ns]
  ;; sequential?, not vector?. Anything that filters or takes hands you a
  ;; lazy seq, which vector? rejects — so it got wrapped in a vector and
  ;; iterated as ONE note whose every key was nil. `(explain (take 1 ns))`
  ;; threw a NullPointerException out of `name`.
  (let [ns (if (sequential? ns) ns [ns])]
    (with-out-str
      (doseq [n ns]
        ;; Ordered fetch-first, like SBCL's `describe`: the compiler's own
        ;; message, then what it EMITTED and the overloads it took/refused —
        ;; the raw machine facts — THEN the measured cost, and the canned
        ;; `why` prose last of the data lines because it is the only
        ;; interpretation in the note. The fact leads; the explanation
        ;; trails.
        (printf "note[%s]: %s\n" (name (:perf.note/code n)) (:perf.note/message n))
        (let [{:perf/keys [file line col]} (:perf.note/span n)]
          (printf "  --> %s:%s:%s\n" (or file "<form>") line col))
        (when-let [e (:perf.note/emitted n)] (printf "   = emitted: %s\n" e))
        (when-let [t (:perf.note/taken n)] (printf "   = took:    %s\n" t))
        (when-let [r (seq (:perf.note/refused n))]
          (printf "   = refused: %s\n" (str/join ", " r)))
        (when-let [v (:perf.note/var n)] (printf "   = var: %s\n" v))
        (println)))))

;; ── rungs 0, 2, 3 ─────────────────────────────────────────────────
;;
;; Macros because they consume the FORM, not its value. That is the real
;; test for a macro, and these pass it.

;; PUBLIC, and it has to be: `java` and `bytecode` expand into the CALLER's
;; namespace, so a private helper here is unresolvable there. The facade
;; carried the same latent bug — its macros were only ever called from
;; inside their own namespace, where the privacy never bit.
(defn resolve!
  "Resolve SYM, loading its namespace, or throw with what is missing."
  [sym]
  (or (requiring-resolve sym)
      (throw (ex-info "not on the classpath" {:sym sym}))))

(defmacro expand
  "Rung 0 — what the macros produced, before the compiler sees it.
  Julia's @code_lowered."
  [form]
  `(clojure.walk/macroexpand-all '~form))

(defmacro java
  "Rung 2 — the Java the Clojure compiler emitted. Where reflection and
  boxing become visible as code rather than as a warning."
  [form]
  `((perf.code/resolve! 'clj-java-decompiler.core/decompile-form) {} '~form))

(defmacro bytecode
  "Rung 3 — JVM bytecode."
  [form]
  `((perf.code/resolve! 'clj-java-decompiler.core/decompile-form) {:decompiler :bytecode} '~form))

;; ── rung 1a: types ────────────────────────────────────────────────
;;
;; I claimed Clojure had no type-inference display to show, and that
;; closing the @code_warntype gap meant taking on tools.analyzer.jvm.
;; Wrong on both counts. Clojure has a type surface layered on the JVM's,
;; and the compiler's conclusion is not hidden — it is the SIGNATURE IT
;; EMITTED, readable off the compiled class with no analyser at all:
;;
;;   (defn boxed [a b] (+ a b))              invokeStatic [Object Object] -> Object
;;   (defn prim ^long [^long a ^long b] ..)  invokeStatic [long long] -> long
;;                                           invokePrim   [long long] -> long
;;
;; Object in that signature IS "the compiler could not work out a type
;; here", which is exactly what @code_warntype colours red. And because
;; the JVM dispatches on it, the inference is not advisory — the runtime
;; already responds to it in performance, which is why the same fact shows
;; up again downstream as a boxed-math note and again as a slower number.
;;
;; With sample values it gets sharper still: inferred vs ACTUAL. "The
;; compiler said Object; every value you passed was a Long" is the
;; actionable form, and it is what `weigh` then uses to build the rewrite.

(defn emitted-signature
  "The signatures the compiler actually emitted for a fn value or var.

  This is the whole Clojure-side type surface: the compiler's one
  load-bearing decision is PRIMITIVE or BOXED, and it is readable straight
  off the class without running anything."
  [f]
  (let [obj (if (var? f) @f f)]
    (->> (.getDeclaredMethods ^Class (class obj))
         (filter #(#{"invokeStatic" "invoke" "invokePrim"}
                   (.getName ^java.lang.reflect.Method %)))
         (map (fn [^java.lang.reflect.Method m]
                #:perf.types{:method (.getName m)
                             :params (mapv tname (.getParameterTypes m))
                             :returns (tname (.getReturnType m))}))
         (sort-by :perf.types/method)
         vec)))

(defn types
  "What the compiler concluded about types, and — given ARGS — what the
  values actually are. Clojure's @code_warntype.

  `:unresolved` lists the positions the compiler left as Object. Those are
  the red ones: every downstream symptom, the boxed-math note and the
  slower measurement, is the same fact seen further along.

    (code/types '(fn [a b] (+ a b)) [3 4])
    ;; :params [Object Object] :actual [Long Long] :unresolved [0 1 :return]"
  ([form] (types form nil))
  ([form args]
   (let [f (eval form)
         sigs (emitted-signature f)
         main (or (first (filter #(= "invokePrim" (:perf.types/method %)) sigs))
                  (first (filter #(= "invokeStatic" (:perf.types/method %)) sigs))
                  (first sigs))
         params (:perf.types/params main)
         returns (:perf.types/returns main)
         obj? #(= 'Object %)
         unresolved (cond-> (vec (keep-indexed #(when (obj? %2) %1) params))
                      (obj? returns) (conj :return))
         ret (when args (apply f args))]
     (cond-> #:perf.types{:emitted sigs
                          :params params
                          :returns returns
                          :unresolved unresolved
                          :verdict (cond (empty? unresolved) :perf.types/resolved
                                         (= (count unresolved)
                                            (inc (count params))) :perf.types/all-boxed
                                         :else :perf.types/partial)}
       args (assoc :perf.types/actual (mapv #(tname (class %)) args)
                   :perf.types/actual-returns (tname (class ret)))))))

;; ── always-on ─────────────────────────────────────────────────────
;;
;; SBCL's notes arrive on EVERY compile without being asked. `notes` is
;; something you call on a form you already suspect, and the gap between
;; "available" and "automatic" is most of the value — you cannot suspect
;; the line you did not think about.
;;
;; Clojure exposes no compiler hook, but the two switches and the stream
;; they write to are all plain vars with root bindings. So: turn the
;; switches on at the root and tee the stream. Everything compiled from
;; then on — by you, by `require`, by the REPL — accumulates notes, and
;; stderr still gets its output, so nothing that was reading it breaks.

(defonce ^:private watch-state (atom nil))

(defn- tee-writer [^java.io.Writer original ^StringBuffer sink]
  (proxy [java.io.Writer] []
    (write
      ([x]
       (if (integer? x)
         (do (.write original (int x)) (.append sink (char x)))
         (let [s (str x)] (.write original s) (.append sink s))))
      ([x off len]
       (if (string? x)
         (do (.write original ^String x (int off) (int len))
             (.append sink (subs x off (+ off len))))
         (do (.write original ^chars x (int off) (int len))
             (.append sink (String. ^chars x (int off) (int len)))))))
    (flush [] (.flush original))
    (close [] (.flush original))))

(defn watch!
  "Collect notes for EVERYTHING compiled from now on. SBCL's default.

  Alters the ROOT bindings of *warn-on-reflection*, *unchecked-math* and
  *err*, so it affects every thread and every later `require`. stderr
  still receives everything it did before — the writer tees rather than
  swallows, because a diagnostic tool that silently eats your stack traces
  has made things worse.

  Reversible with `unwatch!`. Idempotent."
  []
  (or @watch-state
      (let [sink (StringBuffer.)
            original *err*
            prev {:warn (.getRawRoot #'*warn-on-reflection*)
                  :math (.getRawRoot #'*unchecked-math*)
                  :err original
                  :sink sink}]
        (alter-var-root #'*warn-on-reflection* (constantly true))
        (alter-var-root #'*unchecked-math* (constantly :warn-on-boxed))
        (alter-var-root #'*err* (constantly (tee-writer original sink)))
        ;; AND the thread-locals. clojure.main installs thread-local
        ;; bindings for exactly these vars, and a thread-local shadows the
        ;; root — so altering the root alone left the calling REPL
        ;; unaffected and watch! silently caught only the reflection
        ;; warnings that happened to be enabled already. set! throws when
        ;; there is no thread-local frame to write to, which is fine and
        ;; means the root is already the value being read.
        (doseq [v [#'*warn-on-reflection* #'*unchecked-math* #'*err*]]
          (try (var-set v (.getRawRoot ^clojure.lang.Var v)) (catch Throwable _ nil)))
        (reset! watch-state prev)
        :watching)))

(defn watched
  "Notes accumulated since `watch!`, ranked by cost. Cheap to call."
  []
  (if-let [{:keys [sink]} @watch-state]
    (do
      ;; Flush FIRST. The compiler's writes sit in the PrintWriter until
      ;; something forces them out, so reading the sink straight away
      ;; reported one note where unwatch! a moment later reported two —
      ;; the tool under-counting because it asked too early.
      (.flush ^java.io.Writer *err*)
      (vec (sort-by #(kind-rank (:perf.note/code %))
                    (parse-warnings (str sink)))))
    []))

(defn unwatch!
  "Restore the root bindings `watch!` changed, and return what it saw."
  []
  (if-let [{:keys [warn math err]} @watch-state]
    (let [ns (watched)]
      (alter-var-root #'*warn-on-reflection* (constantly warn))
      (alter-var-root #'*unchecked-math* (constantly math))
      (alter-var-root #'*err* (constantly err))
      (doseq [v [#'*warn-on-reflection* #'*unchecked-math* #'*err*]]
        (try (var-set v (.getRawRoot ^clojure.lang.Var v)) (catch Throwable _ nil)))
      (reset! watch-state nil)
      ns)
    []))

(defn ladder
  "Every rung for one form, as data. The whole point of the namespace in
  one call — what you wrote, what the macros made of it, and what the
  compiler could not do about it.

  The native rung is NOT included: it needs a loadable var and a fresh
  JVM, and silently spawning one from a convenience function is how you
  get a REPL that mysteriously takes nine seconds."
  [form]
  #:perf.code{:source form
              :expanded (walk/macroexpand-all form)
              :notes (notes* form)})
