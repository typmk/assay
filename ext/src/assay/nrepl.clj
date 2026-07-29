(ns assay.nrepl
  "nREPL middleware — the seam that makes assay a well-behaved CIDER citizen
  and dissolves the two-repo drift.

  gna-assay.el currently builds Clojure strings and evals them, which is why
  a renamed or deleted fn shipped a live break twice this session: the
  client reached past any contract. An OP is a contract — named, versioned,
  self-describing. A client sends {:op \"assay/notes\" :form \"...\"} and gets
  data back; if the op's shape changes, the descriptor changes with it.

  NOT AOT'd, NOT in the jar. Middleware is loaded into a running nREPL
  server, which already has nrepl on its classpath (cider-nrepl brings it),
  so this ships as source and is added to the middleware stack:

    ;; deps.edn / nrepl config
    :middleware [assay.nrepl/wrap-assay]

  Every op is a thin pass-through to the pure functions. assay.nrepl adds no
  behaviour — it is transport, the same way assay's renderers are conduits.
  The value returned is the EDN the function already produces; nrepl ships
  it as :assay/value, and the client reads it with the same reader it uses
  for everything else."
  (:require [nrepl.middleware :as mw]
            [nrepl.misc :refer [response-for]]
            [nrepl.transport :as transport]
            [assay.code :as code]
            [assay.diagnose :as diagnose]
            [assay.explain :as explain]
            [assay.forms :as forms]
            [assay.measure :as measure]
            [assay.query :as query]
            [assay.repl :as repl]))

(defn- read-form [s] (read-string s))
(defn- read-args [s] (when s (vec (read-string s))))

;; The op table. Each entry: a fn of the request map -> the value to ship.
;; The functions are assay's own — notes*, types, weigh live where they
;; live; this only routes.
(defn- as-int [x d] (if x (Integer/parseInt (str x)) d))
(defn- resolve-in [ns sym] (ns-resolve (symbol (or ns "user")) (symbol sym)))

(def ^:private ops
  {"assay/notes"      (fn [{:keys [form]}] (code/notes* (read-form form)))
   "assay/types"      (fn [{:keys [form args]}] (code/types (read-form form) (read-args args)))
   "assay/weigh"      (fn [{:keys [form args]}] (measure/weigh (read-form form) (read-args args)))
   "assay/fix"        (fn [{:keys [form args]}] (measure/fix (read-form form) (read-args args)))
   ;; the writing-note engine: generate alternative writings of a fn FORM from
   ;; its own structure and rank them cheapest-synonym-first. Powers the
   ;; :assay.note/writing editor surface (gna-assay-writing).
   "assay/writing"    (fn [{:keys [form args]}] (forms/discover (read-form form) (read-args args)))
   ;; the spine: one subject, four oracles. explain = full reading (structure/
   ;; cost/type/outcome + blind? verdict); summary = the compile-only eldoc glance.
   "assay/explain"    (fn [{:keys [form args]}] (explain/explain (read-form form) (read-args args)))
   "assay/summary-of" (fn [{:keys [form args]}] (explain/summary (read-form form) (read-args args)))
   "assay/summary"    (fn [_] (repl/summary))
   "assay/describe"   (fn [_] (query/describe (repl/summary)))
   "assay/allocation" (fn [{:keys [n]}] (repl/allocation (as-int n 15)))
   "assay/blocking"   (fn [{:keys [n]}] (repl/blocking (as-int n 15)))
   "assay/deopts"     (fn [{:keys [n]}] (repl/deopts (as-int n 15)))
   "assay/callers"    (fn [{:keys [sym n]}] (repl/callers sym (as-int n 10)))
   "assay/boxing"     (fn [{:keys [ns sym]}] (some-> (resolve-in ns sym) diagnose/boxing))
   "assay/scan"       (fn [{:keys [ns]}] (diagnose/scan (symbol (or ns "user"))))
   "assay/by-fn"      (fn [{:keys [ns]}]
                       (query/by-fn (repl/obs)
                                    (diagnose/scan (symbol (or ns "user")))))})

(defn wrap-assay
  "Middleware handling the assay/* ops. Anything else falls through."
  [handler]
  (fn [{:keys [op transport] :as msg}]
    (if-let [f (ops op)]
      (transport/send
       transport
       (response-for
        msg :status :done
        ;; :assay/value carries the pr-str'd data — the client reads it with
        ;; the reader it already uses. An op that throws returns the
        ;; exception message, the one thing assay is allowed to author.
        :assay/value (try (pr-str (f msg))
                         (catch Throwable e (pr-str {:assay/error (.getMessage e)})))))
      (handler msg))))

(mw/set-descriptor!
 #'wrap-assay
 {:requires #{}
  :expects #{}
  :handles (into {}
                 (for [op (keys ops)]
                   [op {:doc (str op " — see assay docstrings; returns :assay/value as EDN")
                        :requires {} :returns {"assay/value" "EDN string"}}]))})
