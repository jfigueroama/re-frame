(ns re-frame-lib.registrar
  "In many places, re-frame asks you to associate an `id` (keyword)
  with a `handler` (function).  This namespace contains the
  central registry of such associations."
  (:require  [re-frame-lib.base :refer [state?]]
             [re-frame-lib.interop :refer [debug-enabled?]]
             [re-frame-lib.loggers :refer [console]]))


;; kinds of handlers
(def kinds #{:event :fx :cofx :sub})

;; This atom contains a register of all handlers.
;; Contains a two layer map, keyed first by `kind` (of handler), and then `id` of handler.
;; Leaf nodes are handlers.

(defn get-handler
  ([state kind]
   {:pre [(state? state)]}
   (let [kind->id->handler (:kind->id->handler state)]
     (get @kind->id->handler kind)))

  ([state kind id]
   {:pre [(state? state)]}
   (let [kind->id->handler (:kind->id->handler state)]
     (-> (get @kind->id->handler kind)
         (get id))))

  ([state kind id required?]
   {:pre [(state? state)]}
   (let [kind->id->handler (:kind->id->handler state)
         handler (get-handler state kind id)]
     (when debug-enabled?                          ;; This is in a separate `when` so Closure DCE can run ...
       (when (and required? (nil? handler))        ;; ...otherwise you'd need to type-hint the `and` with a ^boolean for DCE.
         (console :error "re-frame: no " (str kind) " handler registered for: " id)))
     handler)))


(defn register-handler
  [state kind id handler-fn]
  {:pre [(state? state)]}
  (let [kind->id->handler (:kind->id->handler state)]
    (when debug-enabled?                                       ;; This is in a separate when so Closure DCE can run
      (when (get-handler state kind id false)
        (console :warn "re-frame: overwriting" (str kind) "handler for:" id)))   ;; allow it, but warn. Happens on figwheel reloads.
    (swap! kind->id->handler assoc-in [kind id] handler-fn)
    handler-fn))    ;; note: returns the just registered handler


(defn clear-handlers
  ([state]            ;; clear all kinds
   {:pre [(state? state)]}
   (let [kind->id->handler (:kind->id->handler state)]
     (reset! kind->id->handler {})))

  ([state kind]        ;; clear all handlers for this kind
   {:pre [(state? state)]}
   (let [kind->id->handler (:kind->id->handler state)]
     (assert (kinds kind))
     (swap! kind->id->handler dissoc kind)))

  ([state kind id]     ;; clear a single handler for a kind
   {:pre [(state? state)]}
   (let [kind->id->handler (:kind->id->handler state)]
     (assert (kinds kind))
     (if (get-handler state kind id)
       (swap! kind->id->handler update-in [kind] dissoc id)
       (console :warn "re-frame: can't clear" (str kind) "handler for" (str id ". Handler not found."))))))

