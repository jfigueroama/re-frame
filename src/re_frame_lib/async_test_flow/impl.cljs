(ns re-frame-lib.async-test-flow.impl
  (:require
    [cljs.test :refer-macros [is]]
    [nedap.speced.def :as speced]
    [re-frame-lib.core :refer [dispatch]]
    [re-frame-lib.async-test-flow.kws :as async-test-flow]))

(defn conform-dispatch
  [test-def]
  (cond (fn? test-def)
        test-def
                   
        (vector? test-def)
        (fn dispatch-evt [reframe-state]
          (dispatch reframe-state test-def))
                   
        (map? test-def)
        (conform-dispatch (:dispatch test-def))))

(speced/defn ^::async-test-flow/test-definition conform-test-definition
  [^::async-test-flow/test-definition test-def]
  (let [dispatch-fn   (conform-dispatch test-def)
        timeout       (when (and (map? test-def)
                                 (contains? test-def :timeout))
                        (:timeout test-def))
        wait-until    (if (and (map? test-def)
                               (contains? test-def :wait-until))
                        (:wait-until test-def))
        wait-until-fb (fn wait-until-fallback-fn [old-db new-db] (not= old-db new-db))
        test-fn       (if (and (map? test-def)
                               (contains? test-def :test))
                        (:test test-def)
                        (constantly nil))
        semi-test-def {:dispatch dispatch-fn
                       :test     test-fn
                       :timeout  timeout}]
      (if (some? wait-until)
        (assoc semi-test-def :wait-until wait-until)
        (assoc semi-test-def :wait-until-fallback wait-until-fb))))
      
             
(speced/defn add-test-definition-state
  [^::async-test-flow/test-definition test-def]
  (assoc test-def
         :status     :NOT-STARTED
         :db         {}
         :started-at nil
         :ex         nil))
 
(speced/defn ^::async-test-flow/general-state new-general-state
  [^::async-test-flow/main-options options]
  (let [{:keys [test-timeout
                test-started-at
                spawn-timeout
                on-error
                on-success
                on-step]} options]
    (assoc options
           :test-timeout    (or test-timeout 
                                10000)
           :test-started-at (or test-started-at
                                (js/Date.))
           :spawn-timeout   (or spawn-timeout
                                200)
           :on-error        (or on-error
                                (fn e-handler [e] (is (throw e))))
           :on-step         (or on-step
                                (fn step-handler [general-state {:keys [ex] :as test-def}]
                                  (is (nil? ex))))
           :on-success      (or on-success
                                (fn success-handler [{:keys [] :as  general-state}
                                                     test-defs]
                                  (let [status (-> test-defs last :status)]
                                    (is (= :DONE status)))))
                                  
           :current-idx     0)))

(defn assoc-into-def
  [current-idx k]
  (fn tassoc [m v]
    (assoc-in m [current-idx k] v)))


(speced/defn gen-timeout-fn
  "Generates a timeout function that checks for the test-timeout or the timeout of
  the step if exists."
  [{:keys [test-timeout test-started-at] :as general-state}]
  (let [test-started-at-stamp (.getTime test-started-at)]
    (speced/fn ^boolean? timeout?
      [^inst? now
       {:keys [timeout ^inst? started-at] :as test-def}]
      (let [now-stamp         (.getTime now)
            test-elapsed-time (- now-stamp
                                 test-started-at-stamp)]
        (if (< test-elapsed-time
               test-timeout)
          (if (some? timeout)
            (> (- now-stamp
                  (.getTime started-at))
               timeout)
            false)
          true)))))
