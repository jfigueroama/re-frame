(ns re-frame-lib.fx
  (:require
    [re-frame-lib.base        :refer [state?]]
    [re-frame-lib.router      :as router]
    [re-frame-lib.interceptor :refer [->interceptor]]
    [re-frame-lib.interop     :refer [set-timeout!]]
    [re-frame-lib.events      :as events]
    [re-frame-lib.registrar   :refer [get-handler clear-handlers register-handler]]
    [re-frame-lib.loggers     :refer [console]]
    [re-frame-lib.trace :as trace :include-macros true]))

;; -- Registration ------------------------------------------------------------

(def kind :fx)
(assert (re-frame-lib.registrar/kinds kind))

(defn reg-fx
  [state id handler]
  {:pre [(state? state)]}
  (register-handler state kind id handler))

;; -- Interceptor -------------------------------------------------------------

(defn do-fx
  "An interceptor whose `:after` actions the contents of `:effects`. As a result,
  this interceptor is Domino 3.

  This interceptor is silently added (by reg-event-db etc) to the front of
  interceptor chains for all events.

  For each key in `:effects` (a map), it calls the registered `effects handler`
  (see `reg-fx` for registration of effect handlers).

  So, if `:effects` was:
      {:dispatch  [:hello 42]
       :db        {...}
       :undo      \"set flag\"}

  it will call the registered effect handlers for each of the map's keys:
  `:dispatch`, `:undo` and `:db`. When calling each handler, provides the map
  value for that key - so in the example above the effect handler for :dispatch
  will be given one arg `[:hello 42]`.

  You cannot rely on the ordering in which effects are executed, other than that
  `:db` is guaranteed to be executed first."
  [state]
  {:pre [(state? state)]}
  (->interceptor
    :id :do-fx
    :after (fn do-fx-after
             [context]
             (trace/with-trace
               {:op-type :event/do-fx}
               (let [effects            (:effects context)
                     effects-without-db (dissoc effects :db)]
                 ;; :db effect is guaranteed to be handled before all other effects.
                 (when-let [new-db (:db effects)]
                   ((get-handler state kind :db false) new-db))
                 (doseq [[effect-key effect-value] effects-without-db]
                   (if-let [effect-fn (get-handler state kind effect-key false)]
                     (effect-fn effect-value)
                     (console :warn "re-frame: no handler registered for effect: " effect-key ". Ignoring."))))))))

(defn dispatch-later
  [state {:keys [ms dispatch] :as effect}]
  {:pre [(state? state)]}
  (if (or (empty? dispatch) (not (number? ms)))
    (console :error "re-frame: ignoring bad :dispatch-later value: " effect)
    (set-timeout! #(router/dispatch state dispatch) ms)))
