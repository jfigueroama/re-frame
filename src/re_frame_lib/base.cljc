(ns re-frame-lib.base)

(def state-keys
  [:app-db :query->reaction :kind->id->handler :event-queue])

(defn state?
  [state]
  (every? (partial contains? state) state-keys))

