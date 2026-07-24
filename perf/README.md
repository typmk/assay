# perf

Observe a running JVM from a Clojure REPL. Facts in, data out.

```clojure
(require '[perf.repl :as r])

(r/start!)                    ; ... run a workload ...
(r/allocation)
;; => [#:perf{:fn "app/handle-request", :line 6, :n 403} ...]

(r/summary)
;; => #:perf{:samples .. :verdict :perf.verdict/floor-rising
;;           :perf.heap/floor-mb .. :top-alloc [...] :top-blocked [...]}
```

Everything returns plain maps with qualified keys. There is no UI, no
agent, no compiler swap, and nothing to install into the process you are
watching beyond a REPL.

## Why it exists

Clojure has excellent profiling tools — see [PRIOR-ART.md](PRIOR-ART.md),
which is not a formality; most of what this does has been done, including
querying JFR as a table (jfr-analytics; the JDK's own `jfr query`/`jfr
view`). What those give you is SQL rows or a CLI view; what was missing
for me was JFR events as **Clojure data** — maps with demunged Clojure
frames, `datafy`/`nav` to the defining var, queried in-process — so a
profile composes with the rest of the Clojure data ecosystem.

So: one recorder, observations as data, views as queries.

```clojure
(require '[perf.query :as q] '[perf.capture :as capture])

(def obs (capture/observations r))

;; the built-in views are five-line reductions
(q/allocation obs)
(q/blocking obs)

;; and anything else you can phrase
(->> obs
     (filter #(= "worker-3" (:perf/thread %)))
     (q/blocking))
```

## Install

Not published. `{:local/root "..."}` for now.

## The three mechanisms

There are three ways to get data out of a running JVM, and this exposes
exactly those:

| | what | cost |
|---|---|---|
| **stream** | JFR events with stacks → observations | sampled, 3.4 µs/event |
| **poll** | MXBeans over time → heap, alloc rate, GC | negligible |
| **census** | exact live-object count by class | stop-the-world |

That per-event figure comes from an A/B on ~15k real JFR events (commit
94980a9): roughly 3 µs/event after the reflection-and-caching work, down
from ~110 µs before — a ~30x reduction. Rerun the A/B for a current
number on your machine; the figure here is illustrative, not a live
guarantee, and the exact event count and microseconds vary by run.

The observation log is a ring capped at 200k (`:max-observations`). Past
that the oldest half is dropped, `:perf/dropped` is non-zero and
`summary` reports `:perf/coverage :perf.coverage/partial`. Counts stay
correctly ranked against each other but stop being totals.

Sampled and exact numbers never share a column. JFR's allocation `weight`
is a *relative* sample weight — measured here, 639 samples came within
1.13× of the truth while 10 samples extrapolated to 65 GB. So allocation
is ranked by **sample count**, which is proportional to allocation
pressure and has no tail.

## Recorder vs snapshot

A recorder is a live handle: open stream, running thread, `Closeable`.
A **snapshot** is a value.

```clojure
(def r (perf/watch))
(def snap (capture/snapshot r))

(spit "prod.edn" (pr-str snap))       ; ships over a socket, 76 KB
(perf/summary (edn/read-string ...))  ; analysed anywhere, later
```

This is the split Dart's VM Service makes: the service is a connection,
snapshots are data. Take a snapshot in production, read it on your
laptop, with the service long gone.

## Remote

Because the capability lives in the process and nREPL is the protocol,
you can profile a service that knows nothing about this library:

```clojure
;; from a separate process, over nREPL
(ask conn "(do (require '[perf.repl :as r]) (r/start!))")
(ask conn "(first (perf.repl/allocation))")
;; => #:perf{:fn "user/handle-request", :line 6, :n 403}
(ask conn "(perf.repl/mark!)")   ; ... later ...
(ask conn "(take 2 (perf.repl/growth))")
;; => ({:perf.class/name "clojure.lang.PersistentArrayMap"
;;      :perf/d-instances 37953 :perf/d-bytes 1214496})
```

## The ladder

What you wrote, what the compiler gave up on, what it emitted, what the
CPU runs. Julia's `@code_*` family is the model; SBCL supplies the other
half, efficiency notes that arrive automatically and carry cost numbers.

```clojure
(require '[perf.code :as code])

(code/expand '(when x 1))        ; => (if x (do 1))
(code/notes  '(defn f [s] (.length s)))
(code/java     (fn [^long n] (* n 3)))
(code/bytecode (fn [^long n] (* n 3)))
(code/native 'my.ns/tripled [7]) ; x86, via hsdis, in a fresh JVM
```

`notes` is the piece Clojure does not give you. `*warn-on-reflection*` and
`*unchecked-math* :warn-on-boxed` are the compiler saying it had to take a
slow path — the same signal SBCL emits — but they go to `*err*` as prose,
unranked, and only if you remembered to bind them. So:

```clojure
(code/notes '(defn g [a b s] (+ (* a b) (.length s))))
;; => [#:perf.note{:code :perf.note/reflection :span {...} :message "..." :took ... :refused ...}
;;     #:perf.note{:code :perf.note/boxed-math :span {...} :message "..." :took ... :refused ...}
;;     #:perf.note{:code :perf.note/boxed-math :span {...} ...}]
```

**Ranked by MECHANISM, worst first** — which SBCL does not do (it prints
in source order, so one reflective call and nine boxed additions read as
ten equal complaints). Reflection resolves a method by name on every
call; boxing allocates per op; reflection is categorically heavier, so it
sorts first — a structural claim (`kind-rank`), not a stored magnitude. A
note carries NO cost number. For the measured factor on your actual form,
`measure/weigh` runs both paths and times them, live.

Compilation happens in a throwaway namespace — inspecting a `defn` should
not define it in yours.

`native` is the one rung that needs a second JVM: `PrintAssembly` is read
at VM startup, so the process you are sitting in either has it or cannot
be made to. It also needs a var loadable from the classpath, which a
REPL-defined fn is not.

## Browsing

Observations are `Navigable`. Nav a `:perf/site` or a stack frame and you
get the var; a var datafies to its source location.

```clojure
(clojure.datafy/nav o :perf/site (:perf/site o))
;; => #:perf{:var user/allocs, :file "...", :line 4, :arglists ([n])}
```

Portal, Reveal and REBL render that without knowing this library exists.
`(capture/start {:tap? true})` sends every observation to `tap>` — off by
default, because a firehose into a browser is a denial of service.

## Two things worth knowing

**Attribution.** Each observation carries `:perf/site` *and* `:perf/via`.
A megamorphic Clojure call deopts inside `clojure.lang.Var`; `(vec ...)`
allocates inside `clojure.core/vec`. Both are true, and neither is a line
you can edit — so the nearest frame you own and the mechanism are both
recorded, and you choose.

**Leaks are a floor, not a peak.** `:perf/verdict` compares the *minimum*
heap of the first half of the window to the second. A healthy sawtooth
returns to the same baseline; a rising floor is the leak. The peak tells
you nothing either way.

## Extensions

`perf-ext` is a separate artifact, because none of it is needed to
observe a running JVM:

- `perf.diagnose` — compile-time cost notes (boxing, reflection) as
  structured diagnostics with machine-applicable fixes, rustc-shaped
- `perf.control` — live Var tracing; JDI restarts that catch a failure
  pre-unwind and resume it with a value
- `perf.native` — FFM safety: prevent / recover / isolate
- `perf.flow` — a one-function bridge to FlowStorm

They are the ext AOT classes that would otherwise load on every JVM start. `-M:perf-ext` when you want them.

## Status

Personal tooling, measured but not battle-tested. Every performance claim
in the source carries the number that produced it, and several comments
exist because a plausible assumption turned out to be wrong.
