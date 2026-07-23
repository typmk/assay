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

    (code/expand '(when x 1))     ; rung 0 — what the macros produced
    (code/notes  form)            ; rung 1 — what the compiler could not do
    (code/java   form)            ; rung 2 — the Java it emitted
    (code/bytecode form)          ; rung 3 — the bytecode
    (code/native 'my.ns/f [42])   ; rung 4 — the x86, via hsdis

  NOTES ARE RANKED BY COST. SBCL prints notes in source order; a hot loop
  with one reflective call and nine boxed additions reads as ten equal
  complaints. Reflection measured 202x here and boxing 1.25x, so the
  reflective call is 160x more worth fixing and sorts first."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [perf.capability :as cap]))

;; ── the cost model ────────────────────────────────────────────────
;;
;; SBCL's costs are internal units from its own compiler model. These are
;; MEASURED multipliers from this machine, which is a different and in one
;; respect better basis: the number means "this many times slower than the
;; fixed version", and it came from running both.
;;
;; :basis is on every entry because a cost with no provenance is a number
;; someone made up, and those spread.

(def costs
  #:perf.note{:reflection
              #:perf.cost{:factor 202
                          :basis :perf.cost.basis/measured
                          :from "1554.93 ns -> 7.70 ns, this machine"
                          :why "the method is resolved by NAME at every single call"}
              :boxed-math
              #:perf.cost{:factor 1.25
                          :basis :perf.cost.basis/measured
                          :from "primitive vs boxed arithmetic loop"
                          :why "each intermediate allocates a Long or Double"}})

(defn cost
  "The measured cost factor for a note code, or nil."
  [note-code]
  (get-in costs [note-code :perf.cost/factor]))

;; ── rung 1: notes ─────────────────────────────────────────────────

(def ^:private warning-re
  ;; "Reflection warning, NO_SOURCE_PATH:9:50 - reference to field length
  ;;  can't be resolved."
  ;; "Boxed math warning, file.clj:13:48 - call: public static ..."
  #"(?m)^(Reflection|Boxed math) warning, ([^:]*):(\d+):(\d+) - (.*)$")

(def ^:private code-of
  {"Reflection" :perf.note/reflection
   "Boxed math" :perf.note/boxed-math})

(defn- suggestion-for [note-code detail]
  (case note-code
    :perf.note/reflection
    {:perf.note/with "type-hint the target so the call resolves at compile time"
     :perf.note/applicability :perf.note.applicability/maybe
     :perf.note/example "(.length ^String s)"}
    :perf.note/boxed-math
    {:perf.note/with "hint the params and return primitive"
     :perf.note/applicability :perf.note.applicability/maybe
     :perf.note/example "(defn f ^long [^long n] ...)"}
    nil))

(defn- parse-warnings [s]
  (for [[_ kind file line col detail] (re-seq warning-re s)
        :let [c (code-of kind)]]
    #:perf.note{:code c
                :severity :perf.severity/warning
                :span {:perf/file (when-not (= file "NO_SOURCE_PATH") file)
                       :perf/line (parse-long line)
                       :perf/col (parse-long col)}
                :message (str/replace detail #"\.$" "")
                :cost (cost c)
                :cost-basis (get-in costs [c :perf.cost/basis])
                :why (get-in costs [c :perf.cost/why])
                :suggestion (suggestion-for c detail)}))

(defn notes*
  "Compile FORM with every compiler advisory switched on and return what
  the compiler could not do, as data, ranked by measured cost.

  This is the SBCL move, and the mechanism is deliberately dumb: bind
  *warn-on-reflection* and *unchecked-math*, capture *err*, parse. Clojure
  emits these as prose and offers no structured channel — there is no
  compiler hook to subscribe to, so the string IS the API.

  Compiles in a THROWAWAY namespace. Evaluating a defn to inspect it
  should not define it in yours, and an earlier version did exactly that.

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
    (vec (sort-by #(- (or (:perf.note/cost %) 0)) (parse-warnings (str w))))))

(defmacro notes
  "Compile-time notes for FORM, ranked by cost. See `notes*`.

    (code/notes '(defn f [s] (.length s)))"
  [form]
  `(notes* ~(if (and (seq? form) (= 'quote (first form))) form `'~form)))

(defn explain
  "Render notes the way rustc and SBCL do — one block per note, worst
  first. The data is still the data; this is one rendering of it."
  [ns]
  (let [ns (if (vector? ns) ns [ns])]
    (with-out-str
      (doseq [n ns]
        (printf "note[%s]: %s\n" (name (:perf.note/code n)) (:perf.note/message n))
        (let [{:perf/keys [file line col]} (:perf.note/span n)]
          (printf "  --> %s:%s:%s\n" (or file "<form>") line col))
        (when-let [c (:perf.note/cost n)]
          (printf "   = cost: %sx  (%s)\n" c (name (:perf.note/cost-basis n))))
        (when-let [w (:perf.note/why n)] (printf "   = why: %s\n" w))
        (when-let [s (:perf.note/suggestion n)]
          (printf "  help: %s\n        %s\n"
                  (:perf.note/with s) (:perf.note/example s)))
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
       (.waitFor p)
       (if (str/includes? out "Could not load hsdis")
         {:perf/error :hsdis-not-loaded :perf/searched lib}
         out)))))

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
