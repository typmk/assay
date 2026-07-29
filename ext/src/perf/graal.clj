(ns perf.graal
  "The compiler's STATIC type lattice — Graal's Stamps, read as a library.

  perf.jvmci reads what types DID flow (the JIT's runtime profile). This is
  the other half: what the compiler can PROVE about a method before it runs —
  Graal builds a StructuredGraph and annotates every value node with a Stamp
  (its inferred type and, for integers, its value range). That Stamp lattice
  is exactly SBCL's static type derivation — `(integer 0 255)`, `single-float`,
  a nullable-or-not reference — and here it is on the JVM, per node, read live.

    (stamps java.lang.Integer \"sum\" [Integer/TYPE Integer/TYPE])
    ;; => ParameterNode {:kind :int :bits 32 :lo -2147483648 :hi 2147483647} ...
    ;;    AddNode       {:kind :int :bits 32 ...}

  GRAAL ONLY, and needs JARGRAAL, not libgraal: the Java graph-builder classes
  only drive compilation when Graal runs in-process (-XX:-UseJVMCINativeLibrary).
  Launch with the :graal alias (see deps.edn) — a JVMCI runtime, jargraal, and
  the jdk.graal.compiler exports. Its jdk.graal.compiler imports will not COMPILE
  on a stock JVM, so like perf.jvmci it is source-only, loaded on demand on
  GraalVM, never in the AOT jar.

  Reads the graph straight off the initial parse. Even the bare parse infers
  real ranges locally — measured: `(b & 0xFF)` stamps as [0,255], array length
  as [0,MAX_INT], a string param as non-null-exact. Running a CanonicalizerPhase
  afterward would propagate stamps GLOBALLY through constants and conditionals
  across the whole graph — the next layer, not built here."
  (:import [jdk.vm.ci.runtime JVMCI]
           [jdk.vm.ci.meta MetaAccessProvider ResolvedJavaMethod]
           [jdk.graal.compiler.api.runtime GraalJVMCICompiler]
           [jdk.graal.compiler.hotspot HotSpotGraalRuntimeProvider HotSpotGraphBuilderPhase]
           [jdk.graal.compiler.hotspot.meta HotSpotProviders]
           [jdk.graal.compiler.nodes StructuredGraph StructuredGraph$Builder
            StructuredGraph$AllowAssumptions ValueNode NodeView]
           [jdk.graal.compiler.nodes.graphbuilderconf GraphBuilderConfiguration]
           [jdk.graal.compiler.debug DebugContext]
           [jdk.graal.compiler.options OptionValues]
           [jdk.graal.compiler.phases OptimisticOptimizations]
           [jdk.graal.compiler.phases.util Providers]
           [jdk.graal.compiler.phases.tiers HighTierContext]
           [jdk.graal.compiler.core.common.type Stamp IntegerStamp FloatStamp
            AbstractObjectStamp VoidStamp]))

;; ── the Graal runtime is process-global; borrow it once ──────────────
(defn- runtime-provider ^HotSpotGraalRuntimeProvider []
  (let [c (.getCompiler (JVMCI/getRuntime))]
    (when-not (instance? GraalJVMCICompiler c)
      (throw (ex-info "JVMCI compiler is libgraal (a native stub), not jargraal — relaunch with the :graal alias (it sets -XX:-UseJVMCINativeLibrary so Graal runs in-process)"
                      {:compiler (.getName (class c))})))
    (.getGraalRuntime ^GraalJVMCICompiler c)))

(def ^:private cached (atom nil))

(defn- ctx-bundle
  "The reusable pieces: providers, options, meta-access. Cached — they are
  the same for the whole process."
  []
  (or @cached
      (let [rtp (runtime-provider)
            backend (.getHostBackend rtp)
            providers (.getProviders backend)
            options (.getOptions rtp)]
        (reset! cached {:providers providers
                        :options options
                        :meta (.getMetaAccess ^HotSpotProviders providers)}))))

(defn- build-graph ^StructuredGraph [^ResolvedJavaMethod rm]
  (let [{:keys [^HotSpotProviders providers ^OptionValues options]} (ctx-bundle)
        dbg (DebugContext/disabled options)
        graph (-> (StructuredGraph$Builder. options dbg StructuredGraph$AllowAssumptions/YES)
                  (.method rm)
                  (.build))
        config (-> (GraphBuilderConfiguration/getDefault (.getGraphBuilderPlugins providers))
                   (.withEagerResolving true))
        phase (HotSpotGraphBuilderPhase. config)
        ctx (HighTierContext. ^Providers providers nil OptimisticOptimizations/ALL)]
    (.apply phase graph ctx)
    graph))

(defn- stamp->data
  "Structify a Graal Stamp into the type/range lattice as plain data. Integers
  carry [lo hi] and bit-width (the range lattice); floats the same plus NaN
  info; references their type, whether exact, whether non-null."
  [^Stamp s]
  (cond
    (nil? s) nil
    (instance? IntegerStamp s)
    (let [s ^IntegerStamp s]
      #:perf.graal{:kind :int :bits (.getBits s) :lo (.lowerBound s) :hi (.upperBound s)})
    (instance? FloatStamp s)
    (let [s ^FloatStamp s]
      #:perf.graal{:kind :float :bits (.getBits s) :lo (.lowerBound s) :hi (.upperBound s)
                   :non-nan (.isNonNaN s)})
    (instance? AbstractObjectStamp s)
    (let [s ^AbstractObjectStamp s]
      #:perf.graal{:kind :object
                   :type (some-> (.type s) .toJavaName)
                   :exact (.isExactType s)
                   :non-null (.nonNull s)})
    (instance? VoidStamp s) #:perf.graal{:kind :void}
    :else #:perf.graal{:kind :other :repr (str s)}))

(defn- node-stamps [^StructuredGraph graph]
  (->> (.getNodes graph)
       (.iterator)
       iterator-seq
       (keep (fn [n]
               (when (instance? ValueNode n)
                 (let [st (stamp->data (.stamp ^ValueNode n NodeView/DEFAULT))]
                   (assoc st
                          :perf.graal/node (.getSimpleName (class n))
                          :perf.graal/id (.getId ^jdk.graal.compiler.graph.Node n))))))
       vec))

;; ── public: read the static Stamp lattice of a method ────────────────
(defn stamps
  "The static Stamp lattice of a Java method — one entry per value node in
  Graal's parsed graph, each carrying the compiler's inferred type and range.
  KLASS is a Class, MNAME its method name, ARG-TYPES the parameter Class vector
  (use Integer/TYPE etc. for primitives). This is Graal's compile-time type
  derivation, the JVM analogue of SBCL's `(integer lo hi)` / `single-float`."
  ([^Class klass mname] (stamps klass mname []))
  ([^Class klass mname arg-types]
   (let [{:keys [^MetaAccessProvider meta]} (ctx-bundle)
         jm (if (= mname "<init>")
              (.getDeclaredConstructor klass (into-array Class arg-types))
              (.getDeclaredMethod klass mname (into-array Class arg-types)))
         rm (.lookupJavaMethod meta jm)]
     (node-stamps (build-graph rm)))))

(defn stamps-of
  "The static Stamp lattice of a Clojure FN's compiled body. Reads the invoke /
  invokeStatic / invokePrim method carrying the body. NOTE: a plain fn takes
  Object args, so its parameter stamps are Object — only PRIMITIVE-hinted fns
  (invokePrim) or reflected static methods expose integer/float range stamps.
  The honest read: this shows what the compiler knows, which for boxed args is
  'a non-null Object', and that itself is the point perf keeps making."
  [f]
  (let [{:keys [^MetaAccessProvider meta]} (ctx-bundle)
        m (->> (.getDeclaredMethods (class f))
               (filter #(#{"invokePrim" "invokeStatic" "invoke"} (.getName ^java.lang.reflect.Method %)))
               (sort-by #(case (.getName ^java.lang.reflect.Method %)
                           "invokePrim" 0 "invokeStatic" 1 2))
               first)]
    (when m
      (node-stamps (build-graph (.lookupJavaMethod meta m))))))

;; ── IGV handoff ───────────────────────────────────────────────────
;;
;; Getting a graph into the Ideal Graph Visualizer is a six-flag hunt, and
;; getting it WRONG is silent — which is the whole reason this exists.
;;
;; MEASURED on GraalVM CE 25.0.2, and every line of it was a surprise:
;;   * -XX:-UseJVMCINativeLibrary is REQUIRED. libgraal is the default and
;;     produced no dumps at all.
;;   * -Djdk.graal.Dump=:1 plus -Djdk.graal.DumpPath=DIR works, and needs
;;     no PrintGraph setting — File is already the default.
;;   * -Djdk.graal.MethodFilter is where it goes wrong quietly. Both
;;     `user$hot*` and `user$hot.*` matched NOTHING and produced zero
;;     files, while dropping the filter produced 200 .bgv files of JDK
;;     internals. A filter that matches nothing and a run that dumped
;;     nothing look identical from the outside.
;;
;; So the filter is not used. Dump everything, then select by FILE NAME —
;; Graal writes the method signature into it
;; ("HotSpotCompilation-1018[Long.hashCode(long)int].bgv"), so the
;; selection is a string match on ground truth rather than a guess at a
;; filter dialect. Slower and correct beats fast and silently empty.

(defn igv-flags
  "The JVM flags that actually produce IGV graphs, as a vector.
  DIR is where the .bgv files land. Verified on GraalVM CE 25.0.2."
  [dir]
  ["-XX:-UseJVMCINativeLibrary"
   "-Djdk.graal.Dump=:1"
   (str "-Djdk.graal.DumpPath=" dir)])

(defn igv-command
  "A ready `clojure` command line that runs DRIVER-FORM with graph
  dumping on. Paste it, then File > Open in IGV on the .bgv you want —
  or call `dumps` to narrow them down first."
  [dir driver-form]
  (str "clojure "
       (clojure.string/join " " (map #(str "-J" %) (igv-flags dir)))
       " -M -e " (pr-str (pr-str driver-form))))

(defn dumps
  "The .bgv files under DIR, newest first, optionally only those whose
  method signature contains MATCH. Graal puts the signature in the file
  name, so this filters on what was actually compiled rather than on a
  filter expression that may match nothing."
  ([dir] (dumps dir nil))
  ([dir match]
   (->> (file-seq (java.io.File. ^String dir))
        (filter #(.isFile ^java.io.File %))
        (filter #(clojure.string/ends-with? (.getName ^java.io.File %) ".bgv"))
        (filter #(or (nil? match)
                     (clojure.string/includes? (.getName ^java.io.File %) (str match))))
        (sort-by #(- (.lastModified ^java.io.File %)))
        (mapv #(.getPath ^java.io.File %)))))
