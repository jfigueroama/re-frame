(ns re-frame-lib.test-runner
  (:refer-clojure :exclude (set-print-fn!))
  (:require
    [cljs.test :as cljs-test :include-macros true]
    [jx.reporter.karma :as karma :include-macros true]
    ;; Test Namespaces -------------------------------
    ;[re-frame.interceptor-test]
    [re-frame-lib.state-test]
    [re-frame-lib.subs-test]
    [re-frame-lib.async-test-flow.unit.async-test-flow]))
    ;[re-frame.fx-test]
    ;[re-frame.trace-test]
    ;[re-frame.restore-test]
    

(enable-console-print!)

;; ---- BROWSER based tests ----------------------------------------------------
(defn ^:export set-print-fn! [f]
  (set! cljs.core.*print-fn* f))


(defn ^:export run-html-tests []
  (cljs-test/run-tests
    ;'re-frame.interceptor-test
    're-frame-lib.state-test
    're-frame-lib.subs-test
    're-frame-lib.async-test-flow.unit.async-test-flow))
    ;'re-frame.fx-test
    ;'re-frame.trace-test
    ;'re-frame.restore-test
    

;; ---- KARMA  -----------------------------------------------------------------

#_(defn ^:export run-karma [karma]
   (karma/run-tests
     karma
     're-frame.interceptor-test
     're-frame.subs-test
     're-frame.fx-test
     're-frame.trace-test
     're-frame.restore-test))
