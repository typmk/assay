(ns perf.nrepl
  "nREPL middleware — the seam that makes perf a well-behaved CIDER citizen
  and dissolves the two-repo drift.

  gna-perf.el currently builds Clojure strings and evals them, which is why
  a renamed or deleted fn shipped a live break twice this session: the
  client reached past any contract. An OP is a contract — named, versioned,
  self-describing. A client sends {:op \"perf/notes\" :form \"...\"} and gets
  data back; if the op's shape changes, the descriptor changes with it.

  NOT AOT'd, NOT in the jar. Middleware is loaded into a running nREPL
  server, which already has nrepl on its classpath (cider-nrepl brings it),
  so this ships as source and is added to the middleware stack:

    ;; deps.edn / nrepl config
    :middleware [perf.nrepl/wrap-perf]

  Every op is a thin pass-through to the pure functions. perf.nrepl adds no
  behaviour — it is transport, the same way perf's renderers are conduits.
  The value returned is the EDN the function already produces; nrepl ships
  it as :perf/value, and the client reads it with the same reader it uses
  for everything else."
  (:require [nrepl.middleware :as mw]
            [nrepl.misc :refer [response-for]]
            [nrepl.transport :as transport]
            [perf.code :as code]
            [perf.diagnose :as diagnose]
            [perf.forms :as forms]
            [perf.measure :as measure]
            [perf.query :as query]
            [perf.repl :as repl]))

(defn- read-form [s] (read-string s))
(defn- read-args [s] (when s (vec (read-string s))))

;; The op table. Each entry: a fn of the request map -> the value to ship.
;; The functions are perf's own — notes*, types, weigh live where they
;; live; this only routes.
(defn- as-int [x d] (if x (Integer/parseInt (str x)) d))
(defn- resolve-in [ns sym] (ns-resolve (symbol (or ns "user")) (symbol sym)))

(def ^:private ops
  {"perf/notes"      (fn [{:keys [form]}] (code/notes* (read-form form)))
   "perf/types"      (fn [{:keys [form args]}] (code/types (read-form form) (read-args args)))
   "perf/weigh"      (fn [{:keys [form args]}] (measure/weigh (read-form form) (read-args args)))
   "perf/fix"        (fn [{:keys [form args]}] (measure/fix (read-form form) (read-args args)))
   ;; the writing-note engine: generate alternative writings of a fn FORM from
   ;; its own structure and rank them cheapest-synonym-first. Powers the
   ;; :perf.note/writing editor surface (gna-perf-writing).
   "perf/writing"    (fn [{:keys [form args]}] (forms/discover (read-form form) (read-args args)))
   "perf/summary"    (fn [_] (repl/summary))
   "perf/describe"   (fn [_] (query/describe (repl/summary)))
   "perf/allocation" (fn [{:keys [n]}] (repl/allocation (as-int n 15)))
   "perf/blocking"   (fn [{:keys [n]}] (repl/blocking (as-int n 15)))
   "perf/deopts"     (fn [{:keys [n]}] (repl/deopts (as-int n 15)))
   "perf/callers"    (fn [{:keys [sym n]}] (repl/callers sym (as-int n 10)))
   "perf/boxing"     (fn [{:keys [ns sym]}] (some-> (resolve-in ns sym) diagnose/boxing))
   "perf/scan"       (fn [{:keys [ns]}] (diagnose/scan (symbol (or ns "user"))))
   "perf/by-fn"      (fn [{:keys [ns]}]
                       (query/by-fn (repl/obs)
                                    (diagnose/scan (symbol (or ns "user")))))})

(defn wrap-perf
  "Middleware handling the perf/* ops. Anything else falls through."
  [handler]
  (fn [{:keys [op transport] :as msg}]
    (if-let [f (ops op)]
      (transport/send
       transport
       (response-for
        msg :status :done
        ;; :perf/value carries the pr-str'd data — the client reads it with
        ;; the reader it already uses. An op that throws returns the
        ;; exception message, the one thing perf is allowed to author.
        :perf/value (try (pr-str (f msg))
                         (catch Throwable e (pr-str {:perf/error (.getMessage e)})))))
      (handler msg))))

(mw/set-descriptor!
 #'wrap-perf
 {:requires #{}
  :expects #{}
  :handles (into {}
                 (for [op (keys ops)]
                   [op {:doc (str op " — see perf docstrings; returns :perf/value as EDN")
                        :requires {} :returns {"perf/value" "EDN string"}}]))})
