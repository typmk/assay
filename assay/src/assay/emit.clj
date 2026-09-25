(ns assay.emit
  "assay's OWN measurements into the JFR stream. assay.capture READS the
  JVM's events; this WRITES one: a custom JFR event, created dynamically via
  EventFactory (no compiled Event subclass), so a weigh factor or an
  allocation total lands in the SAME recording as ObjectAllocationSample and
  can be correlated in any JFR tool or a capture session. Field types are
  inferred from the values; the per-(name,shape) EventFactory is cached —
  creating one defines a class, so a loop must not."
  (:require [assay.capability :as cap]))

(defonce ^:private factories (atom {}))

(defn- field-type [v]
  (cond (integer? v) Long/TYPE (float? v) Double/TYPE
        (boolean? v) Boolean/TYPE :else String))

(defn- factory-for [ev-name ks vs]
  (let [shape [ev-name (mapv field-type vs)]]
    (or (@factories shape)
        (let [ann [(jdk.jfr.AnnotationElement. jdk.jfr.Name ev-name)
                   (jdk.jfr.AnnotationElement. jdk.jfr.Label ev-name)]
              vds (mapv (fn [k v] (jdk.jfr.ValueDescriptor. (field-type v) (name k))) ks vs)
              f (jdk.jfr.EventFactory/create ann vds)]
          (swap! factories assoc shape f)
          f))))

(defn emit
  "Emit a custom JFR event named EV-NAME carrying the key/value pairs in M,
  so assay's own numbers land in the JFR stream beside the JVM's events —
  correlatable in a `capture` session (add EV-NAME to its :types) or any JFR
  tool. Field types are inferred (long/double/boolean/String). Returns EV-NAME."
  [ev-name m]
  (cap/require! :jfr)
  (let [ks (vec (keys m)) vs (mapv m ks)
        e (.newEvent ^jdk.jfr.EventFactory (factory-for ev-name ks vs))]
    (dotimes [i (count ks)]
      (let [v (nth vs i)]
        (.set e i (cond (integer? v) (long v) (float? v) (double v)
                        (boolean? v) (boolean v) :else (str v)))))
    (.commit e)
    ev-name))
