(ns re-frame-lib.fx
  (:require
    [re-frame-lib.base        :refer [state?]]
    [re-frame-lib.router      :as router]
    [re-frame-lib.interceptor :refer [->interceptor]]
    [re-frame-lib.interop     :refer [set-timeout!]]
    [re-frame-lib.events      :as events]
    [re-frame-lib.registrar   :refer [get-handler clear-handlers register-handler]]
    [re-frame-lib.loggers     :refer [console]]))

;; TODO Add every effect to the new state

;; -- Registration ------------------------------------------------------------

(def kind :fx)
(assert (re-frame-lib.registrar/kinds kind))

(defn reg-fx
  "Register inside the re-frame `state` the given effect `handler` for the
  given `id`.

  `id` is keyword, often namespaced.
  `handler` is a side-effecting function which takes a single argument and whose return
  value is ignored.

  Example Use
  -----------

  First, registration ... associate `:effect2` with a handler.

  (reg-fx
     state
     :effect2
     (fn [value]
        ... do something side-effect-y))

  Then, later, if an event handler were to return this effects map ...

  {...
   :effect2  [1 2]}

   ... then the `handler` `fn` we registered previously, using `reg-fx`, will be
   called with an argument of `[1 2]`."
  [state id handler]
  {:pre [(state? state)]}
  (register-handler state kind id handler))

;; -- Interceptor -------------------------------------------------------------

(defn do-fx
  "An interceptor whose `:after` actions the contents of `:effects`.
  As a result, this interceptor is Domino 3. Requires the re-frame `state`.

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

  You cannot rely on the ordering in which effects are executed."
  [state]
  {:pre [(state? state)]}
  (->interceptor
    :id :do-fx
    :after (fn do-fx-after
             [context]
             (doseq [[effect-key effect-value] (:effects context)]
               (if-let [effect-fn (get-handler state kind effect-key false)]
                 (effect-fn effect-value)
                 (console :error "re-frame: no handler registered for effect: \"" effect-key "\". Ignoring."))))))


