#!/usr/bin/env bash
# Build the AOT jars. assay is always-on (global :deps); ext is opt-in.
#
# WHY AOT: shipped as SOURCE, assay compiles at every JVM start on this
# machine — measured ~1.0s added to every clj, every script, every build.
# AOT-compiled it costs ~0.2s.
#
# WHY CLJ_CONFIG: the user-level deps.edn puts the PREVIOUS jars on the
# classpath, so `compile` would load the already-AOT'd namespaces and emit
# nothing. A tool's output must not be an input to the build that makes it.
#
# While HACKING, skip this: (load-file ".../src/assay/query.clj") in a REPL
# overrides the AOT'd namespace with no rebuild.
set -euo pipefail
cd "$(dirname "$0")"
mkdir -p .buildcfg && echo '{}' > .buildcfg/deps.edn

build () {                       # dir  jar  min-classes  ns...
  local dir=$1 jar=$2 min=$3; shift 3
  rm -rf "$dir/classes" && mkdir -p "$dir/classes"
  # $PWD is evaluated AFTER the cd, so it already includes $dir
  # stdout to /dev/null, but NOT stderr, and the compile must succeed —
  # a docstring with an unescaped quote fails here (twice now), and
  # swallowing stderr let the class-count guard pass on a STALE jar.
  ( cd "$dir" && CLJ_CONFIG=../.buildcfg clj -M -e \
      "(binding [*compile-path* \"$PWD/classes\"] (doseq [n '($*)] (compile n)))" >/dev/null ) \
    || { echo "REFUSING $jar: compile failed (see error above)" >&2; exit 1; }
  jar cf "$jar" -C "$dir/classes" .
  [ -d "$dir/src" ] && (cd "$dir/src" && find . -name 'user.clj' -exec jar uf "../../$jar" {} \; ) || true
  local n; n=$(jar tf "$jar" | grep -c '\.class$')
  [ "$n" -lt "$min" ] && { echo "REFUSING $jar: only $n classes — compile failed" >&2; exit 1; }
  echo "$jar: $n classes, $(stat -c%s "$jar") bytes"
}

build assay assay.jar 100 assay.model assay.capability assay.capture assay.emit assay.query assay.code assay.watch assay.measure assay.range assay.forms assay.repl assay.explain assay
build ext  assay-ext.jar 40 assay.diagnose assay.control assay.native assay.flow assay.trace assay.remote assay.jit
