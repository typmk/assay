;; Auto-loaded by Clojure at REPL start, because ~/.clojure/dev/assay.jar is
;; on the classpath of every project via the top-level :deps entry in
;; ~/.clojure/deps.edn.
;;
;; This runs in EVERY Clojure process on this machine, including builds and
;; CI if they run here. So it must not fail: a throw here would break
;; unrelated projects.
;;
;; But it must not be SILENT either. The original version swallowed the
;; throwable entirely, and twice in one session that hid a plain syntax
;; error in assay.clj — the symptom was "No such namespace: assay" with no
;; hint of why, in a completely different file. Reporting to *err* keeps
;; the safety and drops the mystery.

(ns user)

(try
  ;; `assay` has no hard dependencies — its helpers resolve lazily — so this
  ;; is cheap. The libraries load on first actual use.
  (require 'assay)
  (catch Throwable t
    (binding [*out* *err*]
      (println "user.clj: could not load `assay`:"
               (.getName (class t)) (.getMessage t))
      (when-let [c (.getCause t)]
        (println "  cause:" (.getName (class c)) (.getMessage c))))))
