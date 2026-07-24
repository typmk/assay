# Prior art

Researched after building, which is the wrong order. Most of what this
does has been done, often better. This file exists so the README does not
have to imply otherwise.

## Directly overlapping

### Sayid — https://github.com/clojure-emacs/sayid

**The closest prior art, and it is closer than I expected.** An
"omniscient debugger and profiler" that intercepts and records the inputs
and outputs of functions, lets you select what to trace by var or
namespace, and displays, queries and profiles the result. Version 0.7
made the trace itself data — `sayid.data/trace-data` returns the call
tree as plain Clojure data with the captured values. 0.8 shipped in July
2026; it is maintained under `mx.cider/sayid`.

`perf.control` had a hand-rolled Var-rebinding tracer. **It has been
deleted.** Measured side by side on the same three functions: mine
returned `[[6] 12]` — flat, per-var, timing in a separate map; Sayid
returned a nested call tree with `:args :return :children :depth
:started-at :ended-at :arg-map :meta` from a single `ws-add-trace-ns!`.
It also has inner tracing (every intermediate expression, via
`tools.analyzer.jvm`), a query layer and CIDER integration.

`perf.trace` is now a bridge: profile finds the site, Sayid records its
values. Keeping a worse reimplementation because I wrote it would have
been the sunk-cost version of engineering.

What `perf` does that Sayid does not: JFR-sourced allocation, blocking
and deoptimisation events. Different data entirely.

### Tufte — https://github.com/taoensso/tufte

Form-level profiling "without the low-level JVM noise", with metrics as
**Clojure maps that are easily aggregated, analysed, logged and
serialized**. Thread-local and dynamic profiling.

Tufte reached the data-not-reports conclusion long before I did, and its
scope — *your* `p`-wrapped forms — is deliberately the complement of
this: Tufte measures what you instrument, `perf` samples what the JVM
does. Different question, same philosophy.

### clj-async-profiler — https://github.com/clojure-goes-fast/clj-async-profiler

Embedded async-profiler with interactive flamegraphs, low enough overhead
for production, covering CPU, allocation, locks and context switches.
`perf` calls it for flamegraphs rather than reimplementing it. Its
`serve-ui` remains better than anything here for exploring a profile.

### criterium, clj-memory-meter, clj-java-decompiler, jvm-alloc-rate-meter

The clojure-goes-fast family. `perf` depends on the first three.

I listed `jvm-alloc-rate-meter` as a duplication and then checked, which
is the right order and not the one I used elsewhere: it reports
allocation RATE only (verified, 268 MB/s under load). `capture/poll`
also samples the heap, and the post-GC heap FLOOR is what the leak
verdict reads. Not a duplication — the claim was wrong.

### FlowStorm — https://github.com/flow-storm/flow-storm-debugger

Expression-level value recording with time-travel, via **ClojureStorm**,
a fork of the Clojure compiler. Strictly more powerful than anything in
`perf.control`, and on a different axis from JFR sampling.
`perf.flow` is a 60-line bridge and deliberately nothing more.

### CDT — https://github.com/GeorgeJahad/cdt

**The prior art for `perf.control` and `perf.remote`, which I missed on
the first pass because I searched for profilers rather than debuggers.**

George Jahad, ~2010: a set of Clojure functions using JDI to debug a
remote VM from a REPL running on *another* VM. Breakpoints, catching
exceptions, examining frames and locals, stepping, an Emacs front end
(swank-cdt, later folded into swank-clojure). Its distinguishing feature
was **evaluating arbitrary Clojure in the lexical scope of a suspended
remote frame** — still the high-water mark for JDI debugging in Clojure.

Dead: `cdt/cdt` no longer resolves, and ritz went the same way with
swank. The ideas did not die with the code, and two of them were adopted:

  * **frame locals** on the suspended stack (`perf.control/pending`)
  * **the separate-process architecture** (`perf.remote`), which is not a
    stylistic choice. ClassType.invokeMethod must RESUME the suspended
    thread to run a method; on a self-attached VM the debugger is on that
    same process and it deadlocks. That single fact is why `perf.control`
    needed a field-read trick to return a boxed value at all, and why
    eval-in-frame was impossible there. From a separate process both work
    — verified: read `amount`/`rate` out of a live frame, box 42 through
    a real Long.valueOf call, and restart the frame.

What appears NOT to be in CDT: forceEarlyReturn restarts — resuming a
suspended failure by making the frame return a value. CDT catches,
inspects and evaluates; I found no mention of continuing with a value.

### Eastwood — https://github.com/jonase/eastwood

**Prior art for `perf.code/notes`, found only because Apollo asked "surely
this already exists?" — which is the question I should have asked before
writing it, for the third time in this project.**

Eastwood is a lint tool built on tools.analyzer.jvm, and it has shipped
`:reflection` ("Reflection warnings from the Clojure compiler") and
`:boxed-math` ("Boxed math compiler warnings") linters for years. Its
mechanism is the same one here: pattern-matching the compiler's messages.
And it already returns data — `lint` "returns a map containing data
structures describing any warnings or errors encountered. For example,
file names, line numbers, and column numbers are all available
separately, requiring no parsing of strings."

So "compiler warnings as structured data" is not new, and `notes` is
substantially Eastwood's two linters at form granularity.

**Run side by side, Eastwood 1.4.3 / Clojure 1.12.4, on the same four
functions** (one reflective, one boxed, one clean, one both):

  * **The findings are identical.** Eastwood: 3 boxed-math + 2 reflection.
    `notes`: reflective->[reflection], boxed->[boxed-math x2], clean->[],
    mixed->[reflection boxed-math]. Same five, same lines. No disagreement
    to adjudicate.
  * **Eastwood 73 ms, notes 4.4 ms**, both warm. Not a fair fight and not
    a point in my favour: Eastwood builds a full tools.analyzer AST and
    can run thirty linters off it. `notes` evals a form and greps stderr
    for two patterns. Different amounts of work.
  * **Thirty linters against two.** `performance`, `wrong-tag`,
    `unused-ret-vals`, `constant-test`, `suspicious-expression`,
    `local-shadows-var` and more. Nothing here approaches that.
  * **Its data API did not work in my hands.** `eastwood.lint/lint` is
    documented to return "a map containing data structures"; it returned
    `{:warnings 0 :err true}` with an NPE from `effective-namespaces`,
    with and without `default-opts` merged. Going through a custom
    reporter, the `note` multimethod received only banner strings — the
    warnings travel a protocol method that needs the whole reporter
    implemented. The printing path is correct and complete; getting the
    data out is work. Recorded as observed on this machine, not as a
    verdict on the library.
  * **Eastwood needs source files.** It lints namespaces off `:source-paths`.
    A ladder rung has to take a form you just typed.

What Eastwood's documentation does NOT describe, and what is kept here:

  * **cost on each note, and ranking by it.** Eastwood reports warnings
    uniformly with no severity or priority. Reflection measured 202x
    against boxing at 1.25x, so ordering is the difference between one fix
    and ten.
  * **`weigh`** — deriving the alternative from what the warning
    disclosed, verifying it by recompiling until the notes go away, and
    timing both. Found nothing doing this.
  * **form granularity.** Eastwood lints NAMESPACES; `notes` takes a form
    at the REPL, which is what a ladder rung has to do.

For scanning a whole project, use Eastwood. It is the better tool for
that job, and `perf.diagnose/scan` is the part of this that genuinely
duplicates it.

### clj-java-decompiler / Clojure Goes Fast

The blog makes the argument `perf.code/types` acts on — that checking
"whether you are really using primitive math and unboxed types is more
reliable than compiler warnings" — and clj-java-decompiler gives you the
Java to read. Reading the emitted signature as DATA
(`emitted-signature`) is the small remaining step, and the decompiler
does the heavy lifting for the rungs either side of it.

### farolero — https://github.com/IGJoshua/farolero

**Prior art for the half of `perf.control/restarts!` that should never
have been JDI.** I recorded "native restarts" as a platform limit — the
condition system being a language feature in SBCL, LispWorks and Genera,
and the JVM having none. Wrong. CL's conditions are dynamic binding plus
non-local exit; Clojure has both; it is a library question, and
org.suskalo/farolero (same author as coffi) is that library, with the
full surface: restart-case, handler-bind, invoke-restart, use-value,
store-value, compute-restarts, find-restart, and block/return-from/
tagbody/go.

Verified against the identical test given to SBCL: normal 25, resumed 42.
With five intervening frames the handler ran on a live 51-frame stack —
before unwinding, which is the property that makes conditions worth
having rather than a fancy try/catch.

JDI keeps exactly the case farolero cannot reach, and it is the same case
SBCL reserves for its own debugger: farolero needs the restart
ESTABLISHED at the call site, so resuming a frame inside a third-party
library, clojure.lang or a JDK internal is sb-debug's return-from-frame,
not a language gap. Two tiers; the cheap one is farolero.

### coffi — https://github.com/IGJoshua/coffi

`org.suskalo/coffi`, a maintained wrapper over java.lang.foreign,
described as the fastest FFI available to Clojure. Does not overlap
`perf.native` (three safety functions, not FFI plumbing) — but it is
direct prior art for `~/GitHub/lab/src/lab/ffm.clj`, a hand-rolled
`load-lib`/`downcall`/`call` wrapper. Verified equivalent on the same
calls:

    (ffi/load-library "/lib64/libc.so.6")
    (ffi/defcfn strlen "strlen" [::mem/c-string] ::mem/long)
    (strlen "hello world")   ;=> 11

coffi adds typed layouts, struct serdes, callbacks and static variables.
`lab.ffm` is untracked, so this is a recommendation, not a rewrite.

### tools.jcmd.jfr — `io.github.bsless/tools.jcmd.jfr`

Invokes JFR from inside the JVM in Clojure. Prior art for the JFR
plumbing.

## Adjacent

- **Portal / Reveal / REBL** — data browsers. `perf` implements
  `datafy`/`nav` so these work rather than shipping a UI.
- **µ/log** — event logging with pluggable publishers; the
  "instrumentation emits events, transport is separate" shape.
- **metrics-clojure**, **iapetos** — Dropwizard/Prometheus metrics, for
  aggregate operational monitoring rather than per-site attribution.
- **JDK Mission Control**, **VisualVM**, **YourKit**, **JProfiler** —
  what you should use for serious JFR analysis. `jfr print` and JMC beat
  anything here for a recorded file.

## So what, if anything, is new

Narrow, and worth stating narrowly:

1. **JFR observations as a queryable fact table with `datafy`/`nav`.** I
   found no Clojure library that keeps JFR events as data with normalised
   stacks and lets you query them. Every tool I found reduces them to a
   fixed report.

2. **`:perf/site` vs `:perf/via`.** Recording both the nearest frame you
   own and the mechanism where it actually happened. Standard profilers
   pick one; picking the mechanism gives you `clojure.lang.Var`, and
   picking your frame hides that the call went megamorphic.

3. **Deoptimisation surfaced per Clojure fn.** HotSpot's `class_check`
   and `bimorphic_or_optimized_type_check` events, attributed to the
   function that provoked them. No static analyser can produce this,
   because it depends on the data you actually ran.

4. **Snapshot as a shippable value.** JFR files already travel; a
   snapshot is EDN a Clojure REPL can query without JMC.

That is a smaller contribution than the line count suggests, which is the
honest summary of building first and researching second.
