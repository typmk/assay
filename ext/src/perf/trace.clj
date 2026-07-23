(ns perf.trace
  "Bridge to Sayid. Same shape as perf.flow, and for the same reason.

  perf says WHERE the cost is; Sayid says WHAT the values were. The
  handoff is one function: a perf site names a var, Sayid traces it.

    (->> (perf.repl/allocation) first perf.trace/trace-site!)
    (perf.trace/tree)

  WHY A BRIDGE AND NOT AN IMPLEMENTATION: perf.control had a hand-rolled
  Var-rebinding tracer. Compared side by side, Sayid returned a nested
  call tree with :args :return :children :depth :started-at :ended-at
  from a single ws-add-trace-ns!, where mine returned a flat per-var ring
  buffer. It also has inner tracing via tools.analyzer.jvm. The
  reimplementation was deleted.

  Nothing here is a dependency — everything resolves lazily."
  (:require [clojure.string :as str]))

(defn- sd [sym]
  (try (requiring-resolve (symbol "sayid.core" (name sym))) (catch Throwable _ nil)))

(defn available? [] (some? (sd 'ws-add-trace-fn!*)))

(defn- site->var-sym
  "perf site -> var symbol. Frames demunge to ns/name, anonymous fns to
  ns/outer/fn--123 — the var is `outer`."
  [site]
  (when-let [f (:perf/fn site)]
    (let [[ns-part rest-part] (str/split (str f) #"/" 2)
          base (first (str/split (or rest-part "") #"/"))]
      (when (and (seq ns-part) (seq base)) (symbol ns-part base)))))

(defn init! []
  (if-let [f (sd 'ws-init!)] (f) {:perf.trace/error :sayid-not-on-classpath}))

(defn trace-site!
  "Trace the var behind a perf SITE (or a whole result row) with Sayid."
  [site-or-row]
  (let [site (or (:perf/site site-or-row) site-or-row)
        sym (site->var-sym site)]
    (cond
      (not (available?)) {:perf.trace/error :sayid-not-on-classpath
                          :perf.trace/remedy "add mx.cider/sayid"}
      (nil? sym) {:perf.trace/error :no-var-in-site :perf/site site}
      :else
      ;; ws-add-trace-fn!* takes the SYMBOL — but the symbol must resolve,
      ;; or sayid NPEs deep inside trace-var* reading .ns of a nil var.
      ;; A perf site can legitimately name a frame with no var: an eval
      ;; wrapper, an anonymous fn, a JDK frame. So checking resolution is
      ;; a real failure mode, not a formality.
      (if (try (requiring-resolve sym) (catch Throwable _ nil))
        (do ((sd 'ws-add-trace-fn!*) sym) sym)
        {:perf.trace/error :var-not-resolvable :perf/sym sym}))))

(defn trace-hot!
  "Trace the top N sites from a perf result — profile, then record values."
  ([rows] (trace-hot! rows 3))
  ([rows n] (mapv trace-site! (take n rows))))

(defn tree
  "Sayid's recorded call tree, as data."
  [] (if-let [f (sd 'ws-deref!)] (f) {:perf.trace/error :sayid-not-on-classpath}))

(defn print-tree []
  (if-let [f (sd 'ws-print)] (f) {:perf.trace/error :sayid-not-on-classpath}))

(defn clear!
  "Reset Sayid's workspace. Named clear!, not reset! — the latter shadows
  clojure.core/reset! in every namespace that refers this one."
  [] (when-let [f (sd 'ws-reset!)] (f)))
