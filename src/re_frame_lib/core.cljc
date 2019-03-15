(ns re-frame-lib.core
  (:require
    [re-frame-lib.base             :as base]
    [re-frame-lib.events           :as events]
    [re-frame-lib.subs             :as subs]
    [re-frame-lib.interop          :as interop
                                   :refer [empty-queue ratom set-timeout!]]
    [re-frame-lib.fx               :as fx]
    [re-frame-lib.cofx             :as cofx]
    [re-frame-lib.router           :as router]
    [re-frame-lib.loggers          :as loggers]
    [re-frame-lib.registrar        :as registrar]
    [re-frame-lib.interceptor      :as interceptor]
    [re-frame-lib.std-interceptors :as std-interceptors
     :refer [db-handler->interceptor
             fx-handler->interceptor
             ctx-handler->interceptor]]
    [clojure.set               :as set]))


;; -- API ---------------------------------------------------------------------
;;
;; This namespace represents the re-frame API
;;
;; Below, you'll see we've used this technique:
;;   (def  api-name-for-fn    deeper.namespace/where-the-defn-is)
;;
;; So, we promote a `defn` in a deeper namespace "up" to the API
;; via a `def` in this namespace.
;;
;; Turns out, this approach makes it hard:
;;   - to auto-generate API docs
;;   - for IDEs to provide code completion on functions in the API
;;
;; Which is annoying. But there are pros and cons and we haven't
;; yet revisited the decision.  To compensate, we've added more nudity
;; to the docs.
;;

(def state? base/state?)

;; -- dispatch ----------------------------------------------------------------
(def dispatch       router/dispatch)
(def dispatch-sync  router/dispatch-sync)


;; -- subscriptions -----------------------------------------------------------
(def reg-sub        subs/reg-sub)
(def subscribe      subs/subscribe)

(defn- clear-x
  [kind]
  (fn clear-x-kind
    [state & params]
    {:pre [(state? state)]}
    (apply registrar/clear-handlers
           (concat [state kind] params))))

(def clear-sub (clear-x subs/kind))  ;; think unreg-sub
(def clear-subscription-cache! subs/clear-subscription-cache!)

(defn reg-sub-raw
  "This is a low level, advanced function.  You should probably be
  using reg-sub instead.
  Docs in https://github.com/Day8/re-frame/blob/master/docs/SubscriptionFlow.md"
  [state query-id handler-fn]
  {:pre [(state? state)]}
  (registrar/register-handler state subs/kind query-id handler-fn))


;; -- effects -----------------------------------------------------------------
(def reg-fx      fx/reg-fx)
(def clear-fx    (clear-x fx/kind))  ;; think unreg-fx

;; -- coeffects ---------------------------------------------------------------
(def reg-cofx    cofx/reg-cofx)
(def inject-cofx cofx/inject-cofx)
(def clear-cofx (clear-x cofx/kind)) ;; think unreg-cofx


;; -- Events ------------------------------------------------------------------

(defn reg-event-db
  "Register the given event `handler` (function) for the given `id`. Optionally, provide
  an `interceptors` chain.
  `id` is typically a namespaced keyword  (but can be anything)
  `handler` is a function: (db event) -> db
  `interceptors` is a collection of interceptors. Will be flattened and nils removed.
  `handler` is wrapped in its own interceptor and added to the end of the interceptor
   chain, so that, in the end, only a chain is registered.
   Special effects and coeffects interceptors are added to the front of this
   chain."
  ([state id handler]
   {:pre [(state? state)]}
   (reg-event-db state id nil handler))
  ([state id interceptors handler]
   (events/register state
                    id
                    [(cofx/inject-db state)
                     (fx/do-fx state)
                     interceptors
                     (db-handler->interceptor handler)])))


(defn reg-event-fx
  "Register the given event `handler` (function) for the given `id`. Optionally, provide
  an `interceptors` chain.
  `id` is typically a namespaced keyword  (but can be anything)
  `handler` is a function: (coeffects-map event-vector) -> effects-map
  `interceptors` is a collection of interceptors. Will be flattened and nils removed.
  `handler` is wrapped in its own interceptor and added to the end of the interceptor
   chain, so that, in the end, only a chain is registered.
   Special effects and coeffects interceptors are added to the front of the
   interceptor chain.  These interceptors inject the value of app-db into coeffects,
   and, later, action effects."
  ([state id handler]
   {:pre [(state? state)]}
   (reg-event-fx state id nil handler))
  ([state id interceptors handler]
   {:pre [(state? state)]}
   (events/register state
                    id
                    [(cofx/inject-db state)
                     (fx/do-fx state)
                     interceptors
                     (fx-handler->interceptor handler)])))

(defn reg-event-ctx
  "Register the given event `handler` (function) for the given `id`. Optionally, provide
  an `interceptors` chain.
  `id` is typically a namespaced keyword  (but can be anything)
  `handler` is a function: (context-map event-vector) -> context-map

  This form of registration is almost never used. "
  ([state id handler]
   {:pre [(state? state)]}
   (reg-event-ctx id nil handler))
  ([state id interceptors handler]
   {:pre [(state? state)]}
   (events/register state
                    id
                    [(cofx/inject-db state)
                     (fx/do-fx state)
                     interceptors
                     (ctx-handler->interceptor handler)])))

(def clear-event (clear-x events/kind)) ;; think unreg-event-*

;; -- interceptors ------------------------------------------------------------

;; Standard interceptors.
;; Detailed docs on each in std-interceptors.cljs
(def debug       std-interceptors/debug)
(def path        std-interceptors/path)
(def enrich      std-interceptors/enrich)
(def trim-v      std-interceptors/trim-v)
(def after       std-interceptors/after)
(def on-changes  std-interceptors/on-changes)


;; Utility functions for creating your own interceptors
;;
;;  (def my-interceptor
;;     (->interceptor                ;; used to create an interceptor
;;       :id     :my-interceptor     ;; an id - decorative only
;;       :before (fn [context]                         ;; you normally want to change :coeffects
;;                  ... use get-coeffect  and assoc-coeffect
;;                       )
;;       :after  (fn [context]                         ;; you normally want to change :effects
;;                 (let [db (get-effect context :db)]  ;; (get-in context [:effects :db])
;;                   (assoc-effect context :http-ajax {...}])))))
;;
(def ->interceptor   interceptor/->interceptor)
(def get-coeffect    interceptor/get-coeffect)
(def assoc-coeffect  interceptor/assoc-coeffect)
(def get-effect      interceptor/get-effect)
(def assoc-effect    interceptor/assoc-effect)
(def enqueue         interceptor/enqueue)


;; --  logging ----------------------------------------------------------------
;; Internally, re-frame uses the logging functions: warn, log, error, group and groupEnd
;; By default, these functions map directly to the js/console implementations,
;; but you can override with your own fns (set or subset).
;; Example Usage:
;;   (defn my-fn [& args]  (post-it-somewhere (apply str args)))  ;; here is my alternative
;;   (re-frame-lib.core/set-loggers!  {:warn my-fn :log my-fn})       ;; override the defaults with mine
(def set-loggers! loggers/set-loggers!)

;; If you are writing an extension to re-frame, like perhaps
;; an effects handler, you may want to use re-frame logging.
;;
;; usage: (console :error "Oh, dear God, it happened: " a-var " and " another)
;;        (console :warn "Possible breach of containment wall at: " dt)
(def console loggers/console)


;; -- unit testing ------------------------------------------------------------

(defn make-restore-fn
  "Checkpoints the state of re-frame and returns a function which, when
  later called with current-state, will restore re-frame to that
  checkpointed state.

  Checkpoint includes app-db, all registered handlers and all subscriptions.
  "
  [state]
  (let [handlers   @(:kind->id->handler state)
        app-db     @(:app-db state)
        subs-cache @(:query->reaction state)]
    (fn [cur-state]
     ;; call `dispose!` on all current subscriptions which
     ;; didn't originally exist.
      (let [original-subs (set (vals subs-cache))
            current-subs  (set (vals @(:query->reaction cur-state)))]
        (doseq [sub (set/difference current-subs original-subs)]
          (interop/dispose! sub)))

      ;; Reset the atoms
      ;; We don't need to reset subs/query->reaction, as
      ;; disposing of the subs removes them from the cache anyway
      (reset! (:kind->id->handler cur-state)  handlers)
      (reset! (:app-db cur-state) app-db)
      nil)))

(defn purge-event-queue
  "Remove all events queued for processing"
  [state]
  {:pre [(state? state)]}
  (router/purge (:event-queue state)))

;; -- Event Processing Callbacks  ---------------------------------------------

(defn add-post-event-callback
  "Registers a function `f` to be called after each event is processed
  `f` will be called with two arguments:
  - `event`: a vector. The event just processed.
  - `queue`: a PersistentQueue, possibly empty, of events yet to be processed.

  This is useful in advanced cases like:
  - you are implementing a complex bootstrap pipeline
  - you want to create your own handling infrastructure, with perhaps multiple
  handlers for the one event, etc.  Hook in here.
  - libraries providing 'isomorphic javascript' rendering on  Nodejs or Nashorn.

  'id' is typically a keyword. Supplied at \"add time\" so it can subsequently
  be used at \"remove time\" to get rid of the right callback.
  "
  ([state f]
   {:pre [(state? state)]}
   (add-post-event-callback state f f))   ;; use f as its own identifier
  ([state id f]
   {:pre [(state? state)]}
   (let [event-queue (:event-queue state)]
     (router/add-post-event-callback event-queue id f)
     state)))


(defn remove-post-event-callback
  [state id]
  (let [event-queue (:event-queue state)]
    (router/remove-post-event-callback event-queue id)
    state))


;; STATE -------------------------------------------

(defn new-state-wo-event-queue
  []
  {:app-db            (ratom {})
   :query->reaction   (ratom {})
   :kind->id->handler (atom {})
   :event-queue       nil
   :handling          (atom nil)
   :trace-id          (atom 0)
   :trace-enabled?    false
   :trace-cbs         (atom {})
   :trace-traces      (atom [])})

(defn add-state-defaults
  "Adds to the re-frame state some builtin handlers, like coeffects, effects,
  etc."
  [state]
  (-> state
      ;; Adds to coeffects the value in `app-db`, under the key `:db`
      (reg-cofx
        :db
        (fn db-coeffects-handler
          [coeffects]
          (assoc coeffects :db @(:app-db state)))) 

      ;; :dispatch-later
      ;;
      ;; `dispatch` one or more events after given delays. Expects a collection
      ;; of maps with two keys:  :`ms` and `:dispatch`
      ;;
      ;; usage:
      ;;
      ;;    {:dispatch-later [{:ms 200 :dispatch [:event-id "param"]}    ;;  in 200ms do this: (dispatch [:event-id "param"])
      ;;                      {:ms 100 :dispatch [:also :this :in :100ms]}]}
      ;;
      (reg-fx
        :dispatch-later
        (fn [value]
          (doseq [{:keys [ms dispatch] :as effect} value]
            (if (or (empty? dispatch) (not (number? ms)))
              (console :error "re-frame: ignoring bad :dispatch-later value:" effect)
              (set-timeout! #(router/dispatch state dispatch) ms)))))

      ;; :dispatch
      ;;
      ;; `dispatch` one event. Excepts a single vector.
      ;;
      ;; usage:
      ;;   {:dispatch [:event-id "param"] }

      (reg-fx
        :dispatch
        (fn [value]
          (if-not (vector? value)
            (console :error "re-frame: ignoring bad :dispatch value. Expected a vector, but got:" value)
            (router/dispatch state value))))


      ;; :dispatch-n
      ;;
      ;; `dispatch` more than one event. Expects a list or vector of events. Something for which
      ;; sequential? returns true.
      ;;
      ;; usage:
      ;;   {:dispatch-n (list [:do :all] [:three :of] [:these])}
      ;;
      ;; Note: nil events are ignored which means events can be added
      ;; conditionally:
      ;;    {:dispatch-n (list (when (> 3 5) [:conditioned-out])
      ;;                       [:another-one])}
      ;;
      (reg-fx
        :dispatch-n
        (fn [value]
          (if-not (sequential? value)
            (console :error "re-frame: ignoring bad :dispatch-n value. Expected a collection, got got:" value)
            (doseq [event (remove nil? value)] (router/dispatch state event)))))


      ;; :deregister-event-handler
      ;;
      ;; removes a previously registered event handler. Expects either a single id (
      ;; typically a namespaced keyword), or a seq of ids.
      ;;
      ;; usage:
      ;;   {:deregister-event-handler :my-id}
      ;; or:
      ;;   {:deregister-event-handler [:one-id :another-id]}
      ;;
      (reg-fx
        :deregister-event-handler
        (fn [value]
          (let [clear-event (clear-x events/kind)]
            (if (sequential? value)
              (doseq [event value] (clear-event state event))
              (clear-event state value)))))

      ;; :db
      ;;
      ;; reset! app-db with a new value. `value` is expected to be a map.
      ;;
      ;; usage:
      ;;   {:db  {:key1 value1 key2 value2}}
      ;;
      (reg-fx
        :db
        (fn [value]
          (let [app-db (:app-db state)]
            (if-not (identical? @app-db value)
              (reset! app-db value)))))))

(defn empty-db?
  "Checks if the app-db atom is empty. Useful for figwheel."
  [st]
  (empty? @(:app-db st)))

(defn new-state
  "Creates a new re-frame `state`. This is the one to use.
  Once created, you can create subscriptions and events handlers using the ->
  threading macro:
  
  (defonce state
    (-> (new-state)
        subs/reg-some-subs
        events/reg-some-events)))

  or you can reload the handlers
  (if (empty-db? state)
    (swap! astate #(-> % subs/reg-some-subs events/reg-some-events))) "
  []
  (let [state (new-state-wo-event-queue)]
    (add-state-defaults
      (assoc state
             :event-queue
             (router/->EventQueue state :idle empty-queue {})))))
