(ns perf.diagnose
  "Diagnostics as structured data with machine-applicable fixes.

  Copied from rustc, which emits spans, labels and suggestions as JSON and
  lets rustfix, the CLI and rust-analyzer all render the same record. v1
  returned prose — `:advice \"body is boxed. If these are numbers, hint
  ^long/^double...\"` — and then I hand-wrote a second formatter for
  flymake and a third for the REPL. Three renderings of one fact.

  A diagnostic:

    {:severity   :warning | :note
     :code       :boxed-arithmetic | :reflection
     :span       {:file :line :col}
     :message    short, structured
     :evidence   {what was measured}
     :suggestion {:replace s :with s :applicability :machine|:maybe}}

  Prose is a rendering. So is a flymake overlay.

  SCOPE, after perf.code existed: this namespace reads a COMPILED VAR —
  what signature the compiler actually emitted, available without running
  anything. `perf.code/notes` reads a FORM as it compiles. Different
  inputs, different moments, both useful.

  What is gone is `reflection`, which took a form, eval'd it with
  *warn-on-reflection* bound, and regex'd *err*. That is precisely
  `perf.code/notes*`, except it found one code instead of two, returned
  prose instead of a cost, and did not rank. Keeping a worse copy because
  it was written first is the sunk-cost version of engineering."
  (:require [clojure.string :as str]
            [perf.code :as code]))

(defn signature
  "Emitted method signatures for a fn var — delegated to
  `perf.code/emitted-signature`, which is the same read of the same class.
  This namespace kept its own copy, with its own primitive-name table and
  its own five reflective calls."
  [v]
  (mapv (fn [m]
          {:method (:perf.types/method m)
           :params (mapv str (:perf.types/params m))
           :returns (str (:perf.types/returns m))
           :primitive? (and (seq (:perf.types/params m))
                            (not-any? #(= 'Object %) (:perf.types/params m))
                            (not= 'Object (:perf.types/returns m)))})
        (code/emitted-signature v)))

(defn boxing
  "Diagnostic for a fn var: primitive body or boxed?

  Returns nil when there is nothing to say — a diagnostic that always
  fires is noise, and noise gets filtered out wholesale."
  [v]
  (let [m  (meta v)
        ss (signature v)
        st (first (filter #(= "invokeStatic" (:method %)) ss))]
    (when (and st (not (:primitive? st)))
      {:severity :warning
       :code :boxed-arithmetic
       :span {:file (:file m) :line (:line m) :col (or (:column m) 1)}
       :message "compiler emitted a boxed body"
       ;; Cost comes from perf.code/costs, so the number a diagnostic
       ;; quotes and the number `notes` ranks by cannot drift apart. It
       ;; used to be a prose string here and a different prose string
       ;; there, which is how two sources of one truth begin.
       :cost (code/cost :perf.note/boxed-math)
       :cost-basis :perf.cost.basis/measured
       :evidence {:emitted (format "(%s)%s" (str/join "," (:params st)) (:returns st))
                  :wanted "primitive signature, e.g. (J)J"
                  :compare {:perf.note/reflection (code/cost :perf.note/reflection)
                            :perf.note/boxed-math (code/cost :perf.note/boxed-math)}}
       :suggestion {:replace (str (:name m))
                    :with (str "^long " (:name m))
                    :applicability :maybe}})))

(defn scan
  "Diagnostics for every OPTED-IN fn in NS.

  Opt-in is copied from SBCL, which only emits efficiency notes where you
  wrote (declare (optimize speed)). Most Clojure fns are legitimately
  Object->Object; flagging them all would bury the signal, and an
  advisory channel that cries wolf gets ignored — which is worse than
  not having one.

    (defn ^:perf hot-path ^long [^long n] ...)"
  ([] (scan *ns*))
  ([ns]
   (->> (ns-publics (the-ns ns))
        (keep (fn [[sym v]]
                (when (and (:perf (meta v)) (fn? @v))
                  (some-> (boxing v) (assoc :var (symbol (str ns) (str sym)))))))
        (sort-by #(get-in % [:span :line]))
        vec)))

(defn render
  "One rendering of a diagnostic, rustc-shaped. Clients that want
  something else (flymake, LSP, JSON) consume the map instead."
  [{:keys [severity code span message evidence suggestion cost]}]
  (with-out-str
    (printf "%s[%s]: %s\n" (name severity) (name code) message)
    (when (:line span) (printf "  --> %s:%s\n" (or (:file span) "?") (:line span)))
    (when cost (printf "   = cost: %sx (measured)\n" cost))
    (doseq [[k v] evidence] (printf "   = %s: %s\n" (name k) v))
    (when suggestion
      (printf "help: %s\n" (or (:with suggestion) "")))))
