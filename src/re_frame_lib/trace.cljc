(ns re-frame-lib.trace
  "Tracing for re-frame-lib.
  Alpha quality, subject to change/break at any time."
  #?(:cljs (:require-macros [net.cgrand.macrovich :as macros]
                            [re-frame-lib.trace :refer [finish-trace with-trace merge-trace!]]))
  (:require [re-frame-lib.base :refer [state?]]
            [re-frame-lib.interop :as interop]
            [re-frame-lib.loggers :refer [console]]
            #?(:clj [net.cgrand.macrovich :as macros])
            #?(:cljs [goog.functions])))

;(def id (atom 0))
(def ^:dynamic *current-trace* nil)

;(defn reset-tracing! []
;  (reset! id 0))

;#?(:cljs (goog-define trace-enabled? false)
;   :clj  (def ^boolean trace-enabled? false))

(defn ^boolean is-trace-enabled?
  "See https://groups.google.com/d/msg/clojurescript/jk43kmYiMhA/IHglVr_TPdgJ for more details"
  [state]
  (:trace-enabled? state))

;(def trace-cbs (atom {}))
;(defonce traces (atom []))

(defn register-trace-cb
  "Registers a tracing callback function which will receive a collection of one or more traces.
  Will replace an existing callback function if it shares the same key."
  [state key f]
  {:pre [(state? state)]}
  (let [trace-enabled? (:trace-enabled? state)
        trace-cbs (:trace-cbs state)]
    (if trace-enabled?
      (swap! trace-cbs assoc key f)
      (console :warn "Tracing is not enabled. Please set {\"re_frame.trace.trace_enabled_QMARK_\" true} in :closure-defines. See: https://github.com/Day8/re-frame-trace#installation."))
    state))

(defn remove-trace-cb [state key]
  {:pre [(state? state)]}
  (let [trace-cbs (:trace-cbs state)]
    (swap! trace-cbs dissoc key)
    state))

(defn next-id [state] {:pre [(state? state)]}
  (let [id (:trace-id state)] (swap! id inc)))

(defn start-trace [state {:keys [operation op-type tags child-of]}]
  {:pre [(state? state)]}
  {:id        (next-id state)
   :operation operation
   :op-type   op-type
   :tags      tags
   :child-of  (or child-of (:id *current-trace*))
   :start     (interop/now)})

(defn debounce [f interval]
  #?(:cljs (goog.functions/debounce f interval)
     :clj  (f)))

(defn run-tracing-callbacks!
  {:pre [(state? state)]}
  (let [trace-cbs (:trace-cbs state)
        traces    (:trace-traces state)]
    (debounce
      (fn []
        (doseq [[k cb] @trace-cbs]
          (try (cb @traces)
               #?(:clj (catch Exception e
                         (console :error "Error thrown from trace cb" k "while storing" @traces e)))
               #?(:cljs (catch :default e
                          (console :error "Error thrown from trace cb" k "while storing" @traces e))))
          (reset! traces [])))
      50)))


(macros/deftime
  (defmacro finish-trace [state trace]
    `(let [traces (:trace-traces ~state)]
       (when (is-trace-enabled? ~state)
         (let [end#      (interop/now)
               duration# (- end# (:start ~trace))]
           (swap! traces conj (assoc ~trace
                                     :duration duration#
                                     :end (interop/now)))
           (run-tracing-callbacks! state)))))

 (defmacro with-trace
     "Create a trace inside the scope of the with-trace macro

          Common keys for trace-opts
          :op-type - what kind of operation is this? e.g. :sub/create, :render.
          :operation - identifier for the operation, for an subscription it would be the subscription keyword
          tags - a map of arbitrary kv pairs"
     [state {:keys [operation op-type tags child-of] :as trace-opts} & body]
     `(if (is-trace-enabled? ~state)
        (binding [*current-trace* (start-trace ~trace-opts)]
          (try ~@body
               (finally (finish-trace state *current-trace*))))
        (do ~@body)))

  (defmacro merge-trace! [state m]
     ;; Overwrite keys in tags, and all top level keys.
     `(when (is-trace-enabled? ~state)
        (let [new-trace# (-> (update *current-trace* :tags merge (:tags ~m))
                             (merge (dissoc ~m :tags)))]
          (set! *current-trace* new-trace#))
        nil)))
