(ns assay.flow
  "Bridge to FlowStorm. Optional, lazy, and deliberately thin.

  THE DIVISION OF LABOUR
    assay       answers WHERE the cost is — which fn allocates, blocks,
               deopts, and how much. Cheap, sampled, always-on-able.
    FlowStorm  answers WHAT HAPPENED to the values in that fn — every
               expression's value, steppable forwards and backwards.
               Expensive, exhaustive, aimed at one place.

  They are complementary and independent: profiling narrows a program to
  one function, and value-recording explains that function. Neither needs
  to know about the other, which is why this namespace is 60 lines and
  not a framework.

  WHY THERE IS NO DEEP INTEGRATION, and shouldn't be:

  ClojureStorm is a COMPILER — a fork of the Clojure compiler that
  instruments every form as it compiles. You get it by swapping the
  clojure.jar your project uses, in a deps alias. It is a different JVM
  configuration, not a library you call. So `assay` cannot depend on it,
  and pretending otherwise would produce an integration that only works
  when someone has already arranged the thing it claims to arrange.

  What CAN be bridged is the handoff: assay produces a site
  (:assay/site {:assay/fn \"user/handle-request\"}); FlowStorm instruments
  a var. `instrument-site!` is that one function.

  NOTHING HERE IS A DEPENDENCY. Everything resolves lazily, and
  `available?` tells you the truth instead of throwing."
  (:require [clojure.string :as str]))

(defn- fs [sym]
  (try (requiring-resolve (symbol "flow-storm.api" (name sym)))
       (catch Throwable _ nil)))

(defn available?
  "Is FlowStorm on the classpath?"
  []
  (some? (fs 'instrument-var-clj)))

(defn- site->var-sym
  "assay site -> the var symbol FlowStorm wants.

  Frames demunge to ns/name, and anonymous fns to ns/outer/fn--123 — the
  var is `outer`. Same collapse the call-graph needed."
  [site]
  (when-let [f (:assay/fn site)]
    (let [[ns-part rest-part] (str/split (str f) #"/" 2)
          base (first (str/split (or rest-part "") #"/"))]
      (when (and (seq ns-part) (seq base))
        (symbol ns-part base)))))

(defn instrument-site!
  "Instrument the var behind a assay SITE (or a whole observation/row).

  The handoff: profiling found the hot function; this records its values.

    (->> (assay.repl/allocation) first assay.flow/instrument-site!)

  Returns the var symbol, or a map explaining why not."
  [site-or-row]
  (let [site (or (:assay/site site-or-row) site-or-row)
        sym  (site->var-sym site)]
    (cond
      (not (available?)) {:assay.flow/error :not-on-classpath
                          :assay.flow/remedy "add com.github.flow-storm/flow-storm-dbg"}
      (nil? sym) {:assay.flow/error :no-var-in-site :assay/site site}
      :else (do ((fs 'instrument-var-clj) sym {}) sym))))

(defn uninstrument-site! [site-or-row]
  (let [site (or (:assay/site site-or-row) site-or-row)]
    (when-let [sym (site->var-sym site)]
      (when (available?) ((fs 'uninstrument-var-clj) sym {}))
      sym)))

(defn instrument-hot!
  "Instrument the top N sites from a assay result — the whole workflow in
  one call: profile, take the worst, record their values.

  This is the entire integration. Anything more would be inventing a
  relationship the two tools do not have."
  ([rows] (instrument-hot! rows 3))
  ([rows n] (mapv instrument-site! (take n rows))))

(defn ui!
  "Open the FlowStorm UI, if present."
  []
  (if-let [f (fs 'local-connect)] (f {}) {:assay.flow/error :not-on-classpath}))
