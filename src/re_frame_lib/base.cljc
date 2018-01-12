(ns re-frame-lib.base)

(def state-keys
  [:app-db :query->reaction :kind->id->handler :event-queue :handling
   
   :trace-id :trace-enabled? :trace-cbs :trace-traces])

(defn state?
  [state]
  (every? (partial contains? state) state-keys))

