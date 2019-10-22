(ns re-frame-lib.async-test-flow.kws
  (:require
    [cljs.spec.alpha :as spec]
    [re-frame-lib.base :as re-frame-lib]
    [nedap.utils.spec.predicates :as predicates]
    [re-frame-lib.async-test-flow.kws.test-definition :as test-definition]
    [re-frame-lib.async-test-flow.kws.test-definition-state :as test-definition-state]))

(spec/def ::test-timeout
  ::test-definition/timeout)

(spec/def ::test-definition 
  (spec/or :map ::test-definition/test-definition
           :dispatable ::test-definition/dispatch))

(spec/def ::test-definition-with-state
  (spec/merge ::test-definition/test-definition
              ::test-definition-state/test-definition-state))

(spec/def ::test-definitions
  (spec/coll-of ::test-definition :into []))

(spec/def ::test-definitions-with-state
  (spec/coll-of ::test-definition-with-state :into []))

;  - - - - - 

(spec/def ::state re-frame-lib/state?)
(spec/def ::test-started-at
  inst?)
(spec/def ::done ifn?)
(spec/def ::current-idx predicates/nat-integer?)

(spec/def ::spawn-timeout
  predicates/nat-integer?)

(spec/def ::on-error fn?)
(spec/def ::on-step fn?)
(spec/def ::on-success fn?)

(spec/def ::main-options
  (spec/keys :opt-in [::test-timeout
                      ::test-started-at
                      ::spawn-timeout
                      ::on-error]))

(spec/def ::general-state-input
  (spec/merge ::main-options
              (spec/keys ::req-un [::done
                                   ::state])))


(spec/def ::general-state
  (spec/keys :req-un [::test-timeout
                      ::test-started-at
                      ::spawn-timeout
                      ::state
                      ::done
                      ::current-idx
                      ::on-error
                      ::on-step
                      ::on-success]))

