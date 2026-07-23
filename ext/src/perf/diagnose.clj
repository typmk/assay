(ns perf.diagnose
  "Diagnostics for COMPILED VARS, in the same shape as every other fact
  this library emits.

  Copied from rustc, which emits spans, labels and suggestions as JSON and
  lets rustfix, the CLI and rust-analyzer all render the same record. v1
  returned prose — `:advice \"body is boxed. If these are numbers, hint
  ^long/^double...\"` — and then I hand-wrote a second formatter for
  flymake and a third for the REPL. Three renderings of one fact.

  SCOPE. `perf.code/notes` reads a FORM as it compiles, by capturing what
  the compiler complained about. This reads a VAR that is already
  compiled, off its emitted signature, with nothing to run and no source
  file needed. Different inputs, different moments — a REPL where the code
  arrived by eval has vars but no source paths, and that is the case this
  covers. Eastwood covers the source-file case better; see PRIOR-ART.md.

  ONE SCHEMA. This namespace used to return unqualified keys — :severity,
  :code, :span, :message — while the rest of the library returned
  qualified ones, which is the exact collision perf.model's docstring
  argues against at length. It also meant a converter from
  perf.code/emitted-signature's qualified keys back to unqualified ones,
  purely to feed its own predicate, and a second rustc-shaped renderer
  beside perf.code/explain. All three were the same mistake wearing
  different hats. Now: #:perf.note{...} like everything else, no adapter,
  and `perf.code/explain` renders these too."
  (:require [clojure.string :as str]
            [perf.code :as code]))

(defn boxing
  "Diagnostic for a fn var: primitive body, or boxed?

  Reads `perf.code/types`, which is the same Object-versus-primitive
  question asked one rung up the ladder. This used to recompute it from a
  locally-converted copy of the signature.

  Returns nil when there is nothing to say — a diagnostic that always
  fires is noise, and noise gets filtered out wholesale."
  [v]
  (let [m (meta v)
        t (code/types @v)]
    (when (seq (:perf.types/unresolved t))
      #:perf.note{:code :perf.note/boxed-body
                  :severity :perf.severity/warning
                  :span {:perf/file (:file m)
                         :perf/line (:line m)
                         :perf/col (or (:column m) 1)}
                  :message "compiler emitted a boxed body"
                  ;; Cost comes from perf.code/costs, so the number a
                  ;; diagnostic quotes and the number `notes` ranks by
                  ;; cannot drift apart. It used to be a prose string here
                  ;; and a different prose string there, which is how two
                  ;; sources of one truth begin.
                  :cost (code/cost :perf.note/boxed-body)
                  :cost-basis :perf.cost.basis/measured
                  :why (get-in code/costs [:perf.note/boxed-body :perf.cost/why])
                  :emitted (format "(%s)%s"
                                   (str/join "," (:perf.types/params t))
                                   (:perf.types/returns t))
                  :unresolved (:perf.types/unresolved t)
                  :suggestion #:perf.note{:with "hint the params and return primitive"
                                          :example (str "(defn ^long " (:name m) " ^long [...] ...)")
                                          :applicability :perf.note.applicability/maybe}})))

(defn scan
  "Diagnostics for every OPTED-IN fn in NS, worst cost first.

  Opt-in is copied from SBCL, which only emits efficiency notes where you
  wrote (declare (optimize speed)). Most Clojure fns are legitimately
  Object->Object; flagging them all would bury the signal, and an
  advisory channel that cries wolf gets ignored — which is worse than not
  having one.

    (defn ^:perf hot-path ^long [^long n] ...)"
  ([] (scan *ns*))
  ([ns]
   (->> (ns-publics (the-ns ns))
        (keep (fn [[sym v]]
                (when (and (:perf (meta v)) (fn? @v))
                  (some-> (boxing v)
                          (assoc :perf.note/var (symbol (str ns) (str sym)))))))
        (sort-by #(vector (- (or (:perf.note/cost %) 0))
                          (or (get-in % [:perf.note/span :perf/line]) 0)))
        vec)))
