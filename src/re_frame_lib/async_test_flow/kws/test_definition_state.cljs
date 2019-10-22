(ns re-frame-lib.async-test-flow.kws.test-definition-state
  (:require
    [cljs.spec.alpha :as spec]
    [nedap.utils.spec.predicates :as predicates]
    [nedap.speced.def :as speced :refer-macros [def-with-doc]]))

(spec/def ::status
  #{:NOT-STARTED :RUNNING :TIMEOUT :EXECUTED :TESTED :ERROR :DONE})


(spec/def ::started-at
  (spec/nilable inst?))

(def-with-doc ::db
  "Reframe db before the event is dispatched"
  map?)

(def-with-doc ::ex
  "An exception in CLJS can be anything"
  any?)

(def-with-doc ::test-definition-state
  "Defines a state of a test definition"
  (spec/keys :req-un [::status
                      ::db
                      ::started-at
                      ::ex]))

