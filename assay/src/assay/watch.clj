(ns assay.watch
  "Compiler notes for EVERYTHING compiled from now on — the stateful half of
  assay.code, which stays pure over a form.

  SBCL's notes arrive on EVERY compile without being asked. `notes` is
  something you call on a form you already suspect, and the gap between
  \"available\" and \"automatic\" is most of the value — you cannot suspect
  the line you did not think about.

  Clojure exposes no compiler hook, but the two switches and the stream
  they write to are all plain vars with root bindings. So: turn the
  switches on at the root and tee the stream. Everything compiled from
  then on — by you, by `require`, by the REPL — accumulates notes, and
  stderr still gets its output, so nothing that was reading it breaks."
  (:require [assay.code :as code]))

(defonce ^:private watch-state (atom nil))

;; Serializes watch!/unwatch!. Without it, two threads racing could each
;; capture the OTHER's already-tee'd *err* as its "original", so unwatch!
;; restored *err* to a tee proxy instead of the real PrintWriter — global
;; stderr stranded in an orphaned sink forever, with watch-state reading
;; back nil (looking clean). watch! is a REPL tool rarely raced, but the
;; failure is unrecoverable, so the cheap lock is worth it.
(defonce ^:private watch-lock (Object.))

;; Cap the tee sink. It accumulated ALL *err* traffic — a leak-finder that
;; leaks — and `watched` re-parsed the whole buffer each call. Past the
;; cap the oldest half is dropped (may clip one warning at the boundary,
;; acceptable); recent notes, which is what you asked watch! for, stay.
(def ^:private sink-cap 262144)

(defn- trim! [^StringBuffer sink]
  (when (> (.length sink) (int sink-cap))
    (.delete sink (int 0) (int (quot sink-cap 2)))))

(defn- tee-writer [^java.io.Writer original ^StringBuffer sink]
  (proxy [java.io.Writer] []
    (write
      ([x]
       (if (integer? x)
         (do (.write original (int x)) (.append sink (char x)))
         (let [s (str x)] (.write original s) (.append sink s)))
       (trim! sink))
      ([x off len]
       (if (string? x)
         (do (.write original ^String x (int off) (int len))
             (.append sink (subs x off (+ off len))))
         (do (.write original ^chars x (int off) (int len))
             (.append sink (String. ^chars x (int off) (int len)))))
       (trim! sink)))
    (flush [] (.flush original))
    (close [] (.flush original))))

(defn watch!
  "Collect notes for EVERYTHING compiled from now on. SBCL's default.

  Alters the ROOT bindings of *warn-on-reflection*, *unchecked-math* and
  *err*, so it affects every thread and every later `require`. stderr
  still receives everything it did before — the writer tees rather than
  swallows, because a diagnostic tool that silently eats your stack traces
  has made things worse.

  Reversible with `unwatch!`. Idempotent. Serialized with `unwatch!`."
  []
  (or @watch-state
   (locking watch-lock
    (or @watch-state
      (let [sink (StringBuffer.)
            original *err*
            prev {:warn (.getRawRoot #'*warn-on-reflection*)
                  :math (.getRawRoot #'*unchecked-math*)
                  :err original
                  :sink sink}]
        (alter-var-root #'*warn-on-reflection* (constantly true))
        (alter-var-root #'*unchecked-math* (constantly :warn-on-boxed))
        (alter-var-root #'*err* (constantly (tee-writer original sink)))
        ;; AND the thread-locals. clojure.main installs thread-local
        ;; bindings for exactly these vars, and a thread-local shadows the
        ;; root — so altering the root alone left the calling REPL
        ;; unaffected and watch! silently caught only the reflection
        ;; warnings that happened to be enabled already. set! throws when
        ;; there is no thread-local frame to write to, which is fine and
        ;; means the root is already the value being read.
        (doseq [v [#'*warn-on-reflection* #'*unchecked-math* #'*err*]]
          (try (var-set v (.getRawRoot ^clojure.lang.Var v)) (catch Throwable _ nil)))
        (reset! watch-state prev)
        :watching)))))

(defn watching?
  "Is watch! currently on? Reads the actual state, not the output — a
  client that inferred on/off from (seq (watched)) could not turn watch
  OFF until a note had accrued."
  []
  (some? @watch-state))

(defn watched
  "Notes accumulated since `watch!`, ranked by mechanism. Cheap to call."
  []
  (if-let [{:keys [sink]} @watch-state]
    (do
      ;; Flush FIRST. The compiler's writes sit in the PrintWriter until
      ;; something forces them out, so reading the sink straight away
      ;; reported one note where unwatch! a moment later reported two —
      ;; the tool under-counting because it asked too early.
      (.flush ^java.io.Writer *err*)
      (code/report (code/parse-warnings (str sink))))
    []))

(defn unwatch!
  "Restore the root bindings `watch!` changed, and return what it saw.
  Serialized with `watch!`."
  []
  (locking watch-lock
   (if-let [{:keys [warn math err]} @watch-state]
    (let [ns (watched)]
      (alter-var-root #'*warn-on-reflection* (constantly warn))
      (alter-var-root #'*unchecked-math* (constantly math))
      (alter-var-root #'*err* (constantly err))
      (doseq [v [#'*warn-on-reflection* #'*unchecked-math* #'*err*]]
        (try (var-set v (.getRawRoot ^clojure.lang.Var v)) (catch Throwable _ nil)))
      (reset! watch-state nil)
      ns)
    [])))
