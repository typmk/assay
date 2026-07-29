(ns assay.diagnose
  "Diagnostics for COMPILED VARS, in the same shape as every other fact
  this library emits.

  Copied from rustc, which emits spans, labels and suggestions as JSON and
  lets rustfix, the CLI and rust-analyzer all render the same record. v1
  returned prose — `:advice \"body is boxed. If these are numbers, hint
  ^long/^double...\"` — and then I hand-wrote a second formatter for
  flymake and a third for the REPL. Three renderings of one fact.

  SCOPE. `assay.code/notes` reads a FORM as it compiles, by capturing what
  the compiler complained about. This reads a VAR that is already
  compiled, off its emitted signature, with nothing to run and no source
  file needed. Different inputs, different moments — a REPL where the code
  arrived by eval has vars but no source paths, and that is the case this
  covers. Eastwood covers the source-file case better; see PRIOR-ART.md.

  ONE SCHEMA. This namespace used to return unqualified keys — :severity,
  :code, :span, :message — while the rest of the library returned
  qualified ones, which is the exact collision assay.model's docstring
  argues against at length. It also meant a converter from
  assay.code/emitted-signature's qualified keys back to unqualified ones,
  purely to feed its own predicate, and a second rustc-shaped renderer
  beside assay.code/print-notes. All three were the same mistake wearing
  different hats. Now: #:assay.note{...} like everything else, no adapter,
  and `assay.code/print-notes` renders these too."
  (:require [clojure.string :as str]
            [assay.code :as code]))

(defn boxing
  "Diagnostic for a fn var: primitive body, or boxed?

  Reads `assay.code/types`, which is the same Object-versus-primitive
  question asked one rung up the ladder. This used to recompute it from a
  locally-converted copy of the signature.

  Returns nil when there is nothing to say — a diagnostic that always
  fires is noise, and noise gets filtered out wholesale."
  [v]
  (let [m (meta v)
        t (code/types @v)]
    (when (seq (:assay.types/unresolved t))
      #:assay.note{:code :assay.note/boxed-body
                  :severity :assay.severity/warning
                  :span {:assay/file (:file m)
                         :assay/line (:line m)
                         :assay/col (or (:column m) 1)}
                  ;; The MESSAGE is the derived signature itself, not assay
                  ;; prose. "compiler emitted a boxed body" was assay's
                  ;; sentence; "(Object,Object)Object" is what the compiler
                  ;; actually emitted, read off the class. The fact is the
                  ;; message. No :why, no :suggestion — the signature and
                  ;; the unresolved positions already say everything, and
                  ;; the fix is the same as `notes` shows: hint them.
                  :message (format "(%s)%s"
                                   (str/join "," (:assay.types/params t))
                                   (:assay.types/returns t))
                  :emitted (format "(%s)%s"
                                   (str/join "," (:assay.types/params t))
                                   (:assay.types/returns t))
                  ;; No stored cost — the signature above IS the fact, and
                  ;; the measured factor is `assay.measure/weigh`'s job, live.
                  :unresolved (:assay.types/unresolved t)})))

(defn scan
  "Diagnostics for every OPTED-IN fn in NS, worst cost first.

  Opt-in is copied from SBCL, which only emits efficiency notes where you
  wrote (declare (optimize speed)). Most Clojure fns are legitimately
  Object->Object; flagging them all would bury the signal, and an
  advisory channel that cries wolf gets ignored — which is worse than not
  having one.

    (defn ^:assay hot-path ^long [^long n] ...)"
  ([] (scan *ns*))
  ([ns]
   (->> (ns-publics (the-ns ns))
        (keep (fn [[sym v]]
                (when (and (:assay (meta v)) (fn? @v))
                  (some-> (boxing v)
                          (assoc :assay.note/var (symbol (str ns) (str sym)))))))
        (sort-by #(vector (code/kind-rank (:assay.note/code %))
                          (or (get-in % [:assay.note/span :assay/line]) 0)))
        vec)))
