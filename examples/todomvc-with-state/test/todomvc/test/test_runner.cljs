(ns todomvc.test.test-runner
  (:require
   [cljs-test-display.core]
   [clojure.test :as t :refer [run-tests]]
   [functional.todomvc.example]
   [matcher-combinators.cljs-test]
   [matcher-combinators.printer]
   [unit.todomvc.core]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod cljs.test/report [:cljs-test-display.core/default :matcher-combinators/mismatch]
  [m]
 ; cljs-test-display
  (t/inc-report-counter! :fail)
  (cljs-test-display.core/add-fail-node! m)
 ; console write with color and format
  (println "\nFAIL in" (t/testing-vars-str m))
  (when (seq (:testing-contexts (t/get-current-env)))
    (println (t/testing-contexts-str)))
  (when-let [message (:message m)]
    (println message))
  (println "mismatch:")
  (matcher-combinators.printer/pretty-print (:markup m)))


(defn ^:dev/after-load ^:export main
  []
  (when (js/document.getElementById "app-tests")
    (run-tests
     (cljs-test-display.core/init! "app")
     'functional.todomvc.example
     'unit.todomvc.core)))

