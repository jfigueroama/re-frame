(ns re-frame-lib.async-test-flow-test
  (:require [cljs.test :refer [deftest testing is are async]]
            [re-frame-lib.core :refer [new-state reg-event-db dispatch]]
            [re-frame-lib.async-test-flow.kws :as async-test-flow]
            [re-frame-lib.async-test-flow :as sut]))

(def state-with-event
  (-> (new-state)
      (reg-event-db :init  (fn init-evt [db _] (assoc db :event nil)))
      (reg-event-db :event (fn event [db _] (assoc db :event true)))))

(deftest catch-exception-in-dispath
  (async done
    (sut/run-test-flow
      (new-state)
      [(fn [_] (throw (js/Error. "an error")))]
      {:done     done
       :on-error (fn [e] (is (instance? js/Error e)))})))

(deftest catch-exception-in-test
  (async done
    (sut/run-test-flow
      state-with-event
      [{:dispatch [:event]
        :test     (fn atest [db]
                    (throw :error-in-test))}]
      {:done     done
       :on-error (fn [e] (is (= :error-in-test e)))})))

(deftest catch-exception-in-wait-for
  (async done
    (sut/run-test-flow
      (new-state)
      [{:dispatch       (constantly nil)
        :wait-until     (fn wait-until [db]
                          (throw :error-in-wait-until))}]
      {:done     done
       :on-error (fn [e] (is (= e :error-in-wait-until)))})))


(deftest throws-timeout-after-1-sec
  (async done
    (sut/run-test-flow
      (new-state)
      [{:dispatch (fn [s] nil)}]
      {:done         done
       :on-error     (fn [e] (is (= :TIMEOUT (-> e ex-data :reason))))
       :test-timeout 1000})))

(deftest wait-until-fallback-works
  (async done
    (sut/run-test-flow
      state-with-event
      [[:init]
       {:dispatch (fn [s] (js/setTimeout #(dispatch s [:event])
                                         1000))
        :test     (fn [db] (is (true? (:event db))))}]
      {:done          done
       :spawn-timeout 200
       :test-timeout  2000})))


(deftest run-sucessfully
  (let [state (-> (new-state)
                  (reg-event-db :init  (fn init-evt [db _] (assoc db :value 0)))
                  (reg-event-db :inc   (fn inc-evt [db _] (update db :value inc))))]
    (async done
      (sut/run-test-flow
        state
        [[:init]  ; direct dispatch and implicit testing that we endup with a different db
         {:dispatch [:inc]
          :test     (fn [db] (= 1 (:value db)))}
         {:dispatch (fn [app-state]
                      (dispatch app-state [:inc]))
          :test     (fn [db] (= 2 (:value db)))}]
        {:done         done
         :test-timeout 3000}))))

