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

  Prose is a rendering. So is a flymake overlay."
  (:require [clojure.string :as str]))

(def ^:private prim
  {Long/TYPE "long" Double/TYPE "double" Integer/TYPE "int"
   Boolean/TYPE "boolean" Void/TYPE "void" Float/TYPE "float"
   Character/TYPE "char" Byte/TYPE "byte" Short/TYPE "short"})

(defn- tname [^Class c] (or (prim c) (.getSimpleName c)))

(defn signature
  "Emitted method signatures for a fn var.

  This is the whole Clojure-side compile-time surface: the compiler's one
  load-bearing decision is PRIMITIVE or BOXED, and it is readable straight
  off invokeStatic without running anything."
  [v]
  (let [obj (if (var? v) @v v)]
    (->> (.getDeclaredMethods (class obj))
         (filter #(#{"invokeStatic" "invoke" "invokePrim"} (.getName %)))
         (map (fn [m]
                (let [ps (vec (.getParameterTypes m)) ret (.getReturnType m)]
                  {:method (.getName m)
                   :params (mapv tname ps)
                   :returns (tname ret)
                   :primitive? (and (seq ps)
                                    (every? #(.isPrimitive ^Class %) ps)
                                    (.isPrimitive ret))})))
         (sort-by :method)
         vec)))

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
       :evidence {:emitted (format "(%s)%s" (str/join "," (:params st)) (:returns st))
                  :wanted "primitive signature, e.g. (J)J"
                  :measured-cost "1.25x — real but small; check reflection first, measured 202x"}
       :suggestion {:replace (str (:name m))
                    :with (str "^long " (:name m))
                    :applicability :maybe}})))

(defn reflection
  "Does FORM compile to a reflective call? Measured at 202x on this
  machine — the highest-value compile-time signal in the language, and
  invisible unless asked for."
  [form]
  (let [w (java.io.StringWriter.)]
    (binding [*warn-on-reflection* true *err* w]
      (try (eval form) (catch Throwable _ nil)))
    (let [s (str w)]
      (when (re-find #"(?i)reflection warning" s)
        {:severity :warning
         :code :reflection
         :message "reflective call — resolved by name at every invocation"
         :evidence {:warnings (str/split-lines s)
                    :measured-cost "202x on this machine (1554.93ns -> 7.70ns)"}
         :suggestion {:with "add a type hint to the target"
                      :applicability :maybe}}))))

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
  [{:keys [severity code span message evidence suggestion]}]
  (with-out-str
    (printf "%s[%s]: %s\n" (name severity) (name code) message)
    (when (:line span) (printf "  --> %s:%s\n" (or (:file span) "?") (:line span)))
    (doseq [[k v] evidence] (printf "   = %s: %s\n" (name k) v))
    (when suggestion
      (printf "help: %s\n" (or (:with suggestion) "")))))
