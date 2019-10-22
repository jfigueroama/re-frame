(ns re-frame-lib.async-test-flow
  (:require
    [cljs.test :refer-macros [is async]]
    [nedap.speced.def :as speced]
    [re-frame-lib.async-test-flow.kws :as async-test-flow]
    [re-frame-lib.async-test-flow.kws.test-definition-state :as test-definition-state]
    [re-frame-lib.async-test-flow.impl :as impl :refer [assoc-into-def]]))

(declare flow-step)

(speced/defn spawn
  [^::async-test-flow/general-state general-state
   ^::async-test-flow/test-definitions-with-state test-defs
   ^::async-test-flow/spawn-timeout spawn-timeout]
  (js/setTimeout
    (partial flow-step general-state test-defs)
    spawn-timeout))

(speced/defn flow-step
  "Contains all the logic for the step checking, running, etc."
  [{:keys [state done spawn-timeout current-idx
           on-error on-step on-success] :as general-state}
   ^::async-test-flow/test-definitions-with-state test-defs]
  (let [assoc-status       (assoc-into-def current-idx :status)
        assoc-db           (assoc-into-def current-idx :db)
        assoc-start        (assoc-into-def current-idx :started-at)
        assoc-ex           (assoc-into-def current-idx :ex)
        assoc-error        (fn assoc-status-inner [tdefs ex]
                             (-> tdefs
                                 (assoc-status :ERROR)
                                 (assoc-ex     ex)))
        {:keys [status db ex dispatch
                wait-until wait-until-fallback]
         :as test-def}     (get test-defs current-idx)
        app-db             @(:app-db state)
        timeout?           (impl/gen-timeout-fn general-state)
        now                (js/Date.)
        pspawn             (partial spawn general-state)]
    (case status
      :NOT-STARTED
      (do (try 
            (dispatch state)
            (pspawn (-> test-defs
                        (assoc-status :RUNNING)
                        (assoc-db     app-db)
                        (assoc-start  (js/Date.)))
                   0)
            (catch :default e
              (pspawn (assoc-error test-defs e) 0))))
    
      :RUNNING
      (do
        (if-not (timeout? now test-def)
          (try (let [move-on (if (some? wait-until)
                               (wait-until app-db)
                               (wait-until-fallback db app-db))]
                 (if move-on
                   (pspawn (assoc-status test-defs :EXECUTED) 0)
                   (pspawn test-defs spawn-timeout)))
               (catch :default e
                 (pspawn (assoc-error test-defs e) 0)))
         (pspawn (assoc-status test-defs :TIMEOUT) 0)))

      :TIMEOUT
      (pspawn (assoc-error test-defs
                           (ex-info "Timeout reached"
                                    {:test-definition test-def
                                     :general-state   general-state
                                     :reason          :TIMEOUT}))
              0)
    
      :EXECUTED
      (try (apply (:test test-def)
                  [(with-meta app-db {:state state})])
           (pspawn (assoc-status test-defs :TESTED) 0)
           (catch :default e
             (pspawn (assoc-error test-defs e) 0)))
             
    
      :TESTED
      (pspawn (assoc-status test-defs :DONE) 0)
    
      :ERROR
      (pspawn (assoc-status test-defs :DONE) 0)
    
      :DONE
      (let [max-idx   (dec (count test-defs))]
        (try (when-not ex
              (on-step general-state test-def)) ; could be moved to a on-sucess and/or on-step
             (catch :default e
               (js/console.error e)))
        (if (and (< current-idx max-idx)
                 (nil? ex))
          (spawn (update general-state :current-idx inc)
                 test-defs
                 0)
          (try (if (some? ex)
                 (on-error ex)
                 (on-success general-state test-defs))
               (catch :default e
                 (js/console.error e))
               (finally
                 (done))))))))


(speced/defn run-test-flow
  "Runs the flow assigning an index to every test-definition"
  ([state test-defs]
   (run-test-flow state test-defs {}))
  ([^::async-test-flow/state state
    ^::async-test-flow/test-definitions test-defs
    ^::async-test-flow/main-options options]
   (async done
          (let [general-state            (impl/new-general-state (assoc options
                                                                        :done done
                                                                        :state state))
                test-definitions-w-state (mapv (comp impl/add-test-definition-state
                                                     impl/conform-test-definition)
                                               test-defs)]
            (if-not (zero? (count test-defs))
              (js/setTimeout
                 (partial flow-step general-state test-definitions-w-state))
              (done))))))

