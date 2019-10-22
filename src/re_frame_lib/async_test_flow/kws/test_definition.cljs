(ns re-frame-lib.async-test-flow.kws.test-definition
  (:require
    [cljs.spec.alpha :as spec]
    [nedap.utils.spec.predicates :as predicates]
    [nedap.speced.def :as speced :refer-macros [def-with-doc]]))

(speced/defn re-frame-event? [^vector? v] (keyword? (first v)))

(def-with-doc ::dispatch
 "A thing that could be dispatching following the re-frame-lib approach:
  a function that receives the state or a re-frame vector event."
 (spec/or :fn fn?
          :dispatch-vector (spec/and vector? re-frame-event?)))

(def-with-doc ::wait-until
  "Says when the test are going to be called"
  fn?)

(def-with-doc ::wait-until-fallback
  "Fn to be called if the wait-until fn is not sent. It will receive the old-db and the current re-frame db"
  fn?)

(def-with-doc ::timeout
  "Timeout for this particular test definition"
  (spec/nilable predicates/pos-integer?))

(def-with-doc ::test
  "Function that will receive the db of the state and will actually
  run the tests with `is` or `are`"
  fn?)


(def-with-doc ::test-definition
  "Defines a step in the flow that can be tested"
  (spec/keys :req-un [::dispatch]
             :opt-un [::wait-until
                      ::wait-until-fallback
                      ::test
                      ::timeout]))

