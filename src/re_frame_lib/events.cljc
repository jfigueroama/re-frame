(ns re-frame-lib.events
  (:require [re-frame-lib.base        :refer [state?]]
            [re-frame-lib.utils       :refer [first-in-vector]]
            [re-frame-lib.interop     :refer [empty-queue debug-enabled?]]
            [re-frame-lib.registrar   :refer [get-handler register-handler]]
            [re-frame-lib.loggers     :refer [console]]
            [re-frame-lib.interceptor :as  interceptor]
            [re-frame-lib.trace       :as trace :include-macros true]))


(def kind :event)
(assert (re-frame-lib.registrar/kinds kind))

(defn- flatten-and-remove-nils
  "`interceptors` might have nested collections, and contain nil elements.
  return a flat collection, with all nils removed.
  This function is 9/10 about giving good error messages."
  [id interceptors]
  (let [make-chain  #(->> % flatten (remove nil?))]
    (if-not debug-enabled?
      (make-chain interceptors)
      (do    ;; do a whole lot of development time checks
        (when-not (coll? interceptors)
          (console :error "re-frame: when registering " id ", expected a collection of interceptors, got: " interceptors))
        (let [chain (make-chain interceptors)]
          (when (empty? chain)
            (console :error "re-frame: when registering " id ", given an empty interceptor chain"))
          (when-let [not-i (first (remove interceptor/interceptor? chain))]
            (if (fn? not-i)
              (console :error "re-frame: when registering " id ", got a function instead of an interceptor. Did you provide old style middleware by mistake? Got: " not-i)
              (console :error "re-frame: when registering " id ", expected interceptors, but got: " not-i)))
          chain)))))


(defn register
  "Associate the given event `id` with the given collection of `interceptors`
  in the re-frame `state`.

   `interceptors` may contain nested collections and there may be nils
   at any level,so process this structure into a simple, nil-less vector
   before registration.

   Typically, an `event handler` will be at the end of the chain (wrapped
   in an interceptor)."
  [state id interceptors]
  (register-handler state kind id (flatten-and-remove-nils id interceptors)))



;; -- handle event --------------------------------------------------------------------------------

(defn handle
  "Given an event vector `event-v` and the re-frame `state`, look up
  the associated interceptor chain, and execute it."
  [state event-v]
  {:pre [(state? state)]}
  (let [event-id   (first-in-vector event-v)
        app-db     (:app-db state)
        *handling* (:handling state)]
    (if-let [interceptors  (get-handler state kind event-id true)]
      (if @*handling*
          (console :error "re-frame: while handling \"" *handling* "\", dispatch-sync was called for \"" event-v "\". You can't call dispatch-sync within an event handler.")
          (do
           (reset! *handling* event-v)
           (try
             (trace/with-trace
                 {:operation event-id
                  :op-type   kind
                  :tags      {:event event-v}}
                 (trace/merge-trace! {:tags {:app-db-before @app-db}})
                 (interceptor/execute event-v interceptors)
                 (trace/merge-trace! {:tags {:app-db-after  @app-db}}))
             (finally (reset! *handling* nil))))))
    state))

