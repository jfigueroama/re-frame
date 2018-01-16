(ns re-frame-lib.base)

(def state-keys
  [:app-db :query->reaction :kind->id->handler :event-queue :handling
   :interceptors 
   :trace-id :trace-enabled? :trace-cbs :trace-traces
   ])

(defn state-for-testing
  []
  {:app-db (atom {})
   :query->reaction (atom {})
   :kind->id->handler (atom {})
   :interceptors {}
   :event-queue nil
   :handling nil
   :trace-id (atom 0)
   :trace-enabled? false
   :trace-cbs (atom {})
   :trace-traces (atom [])
   })

(defn state?
  [state]
  (every? (partial contains? state) state-keys))

