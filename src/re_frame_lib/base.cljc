(ns re-frame-lib.base
  "Contains the very basic functions to have a separate state for
   a re-frame execution.
  The main point of re-frame-lib is to easy to remove most of the
  global state required by re-frame. At some point it is done,
  except for facilities that may be considered equal for all the
  re-frame executions like the executor, console, tracing, etc.")

(def state-keys
  "Keys that compose the state hash map. It can be more."
  [:app-db :query->reaction :kind->id->handler  ; storage
   :event-queue :handling                       ; events
   :trace-id :trace-enabled? :trace-cbs :trace-traces])

(defn state-for-testing
  "This state is only for testing the implementation of re-frame-lib.
  For normal usage you should use re-frame-lib.core/new-state."
  []
  {:app-db (atom {})
   :query->reaction (atom {})
   :kind->id->handler (atom {})
   :event-queue nil
   :handling (atom nil)
   :trace-id (atom 0)
   :trace-enabled? false
   :trace-cbs (atom {})
   :trace-traces (atom [])})
   

(defn state?
  [state]
  (every? (partial contains? state) state-keys))

