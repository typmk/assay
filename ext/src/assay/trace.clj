(ns assay.trace
  "Bridge to Sayid. Same shape as assay.flow, and for the same reason.

  assay says WHERE the cost is; Sayid says WHAT the values were. The
  handoff is one function: a assay site names a var, Sayid traces it.

    (->> (assay.repl/allocation) first assay.trace/trace-site!)
    (assay.trace/tree)

  WHY A BRIDGE AND NOT AN IMPLEMENTATION: assay.control had a hand-rolled
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
  "assay site -> var symbol. Frames demunge to ns/name, anonymous fns to
  ns/outer/fn--123 — the var is `outer`."
  [site]
  (when-let [f (:assay/fn site)]
    (let [[ns-part rest-part] (str/split (str f) #"/" 2)
          base (first (str/split (or rest-part "") #"/"))]
      (when (and (seq ns-part) (seq base)) (symbol ns-part base)))))

(defn init! []
  (if-let [f (sd 'ws-init!)] (f) {:assay.trace/error :sayid-not-on-classpath}))

(defn trace-site!
  "Trace the var behind a assay SITE (or a whole result row) with Sayid."
  [site-or-row]
  (let [site (or (:assay/site site-or-row) site-or-row)
        sym (site->var-sym site)]
    (cond
      (not (available?)) {:assay.trace/error :sayid-not-on-classpath
                          :assay.trace/remedy "add mx.cider/sayid"}
      (nil? sym) {:assay.trace/error :no-var-in-site :assay/site site}
      :else
      ;; ws-add-trace-fn!* takes the SYMBOL — but the symbol must resolve,
      ;; or sayid NPEs deep inside trace-var* reading .ns of a nil var.
      ;; A assay site can legitimately name a frame with no var: an eval
      ;; wrapper, an anonymous fn, a JDK frame. So checking resolution is
      ;; a real failure mode, not a formality.
      (if (try (requiring-resolve sym) (catch Throwable _ nil))
        (do ((sd 'ws-add-trace-fn!*) sym) sym)
        {:assay.trace/error :var-not-resolvable :assay/sym sym}))))

(defn trace-hot!
  "Trace the top N sites from a assay result — profile, then record values."
  ([rows] (trace-hot! rows 3))
  ([rows n] (mapv trace-site! (take n rows))))

(defn tree
  "Sayid's recorded call tree, as data."
  [] (if-let [f (sd 'ws-deref!)] (f) {:assay.trace/error :sayid-not-on-classpath}))

(defn print-tree []
  (if-let [f (sd 'ws-print)] (f) {:assay.trace/error :sayid-not-on-classpath}))

(defn clear!
  "Reset Sayid's workspace. Named clear!, not reset! — the latter shadows
  clojure.core/reset! in every namespace that refers this one."
  [] (when-let [f (sd 'ws-reset!)] (f)))
