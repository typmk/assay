# perf

Observe a running JVM from a Clojure REPL. Facts in, data out.

```clojure
(require '[perf.repl :as r])

(r/start!)                    ; ... run a workload ...
(r/allocation)
;; => [#:perf{:fn "app/handle-request", :line 6, :n 403} ...]

(r/summary)
;; => #:perf{:verdict :perf.verdict/floor-rising
;;           :heap {...} :top-alloc [...] :top-blocked [...]}
```

Everything returns plain maps with qualified keys. There is no UI, no
agent, no compiler swap, and nothing to install into the process you are
watching beyond a REPL.

## Why it exists

Clojure has excellent profiling tools — see [PRIOR-ART.md](PRIOR-ART.md),
which is not a formality; most of what this does has been done. What was
missing for me was a **queryable fact table**: JFR already emits typed
events with stack traces, and every tool I found reduced them to a fixed
report. Fixed reports throw away every question you did not anticipate.

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
| **stream** | JFR events with stacks → observations | sampled, ~1% |
| **poll** | MXBeans over time → heap, alloc rate, GC | negligible |
| **census** | exact live-object count by class | stop-the-world |

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

They were 65 of 217 AOT classes loaded on every JVM start before the
split. `-M:perf-ext` when you want them.

## Status

Personal tooling, measured but not battle-tested. Every performance claim
in the source carries the number that produced it, and several comments
exist because a plausible assumption turned out to be wrong.
