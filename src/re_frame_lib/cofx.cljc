(ns re-frame-lib.cofx
  (:require
    [re-frame-lib.base         :refer [state?]]
    [re-frame-lib.interceptor  :refer [->interceptor]]
    [re-frame-lib.registrar
     :refer [get-handler clear-handlers register-handler]]
    [re-frame-lib.loggers      :refer [console]]))


;; -- Registration ------------------------------------------------------------

(def kind :cofx)
(assert (re-frame-lib.registrar/kinds kind))

(defn reg-cofx
  [state id handler]
  {:pre [(state? state)]}
  (register-handler state kind id handler))


;; -- Interceptor -------------------------------------------------------------

(defn inject-cofx
  ([state id]
   {:pre [(state? state)]}
   (->interceptor
     :id      :coeffects
     :before  (fn coeffects-before
                [context]
                (if-let [handler (get-handler state kind id)]
                  (update context :coeffects handler)
                  (console :error "No cofx handler registered for " id)))))
  ([state id value]
   {:pre [(state? state)]}
   (->interceptor
     :id      :coeffects
     :before  (fn coeffects-before
                [context]
                (if-let [handler (get-handler state kind id)]
                  (update context :coeffects handler value)
                  (console :error "No cofx handler registered for " id))))))


;; -- Builtin CoEffects Handlers  ---------------------------------------------

;; Because this interceptor is used so much, we reify it
;; This does not work any more.
(defn inject-db [state] {:pre [(state? state)]} (inject-cofx state :db))

