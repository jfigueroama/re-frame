(ns re-frame-lib.async-test-flow-test
  (:require [cljs.test :refer [deftest testing is are]]
            [re-frame-lib.core :refer [new-state reg-event-db dispatch]]
            [re-frame-lib.async-test-flow.kws :as async-test-flow]
            [re-frame-lib.async-test-flow :as sut]))

(def state-with-event
  (-> (new-state)
      (reg-event-db :init  (fn init-evt [db _] (assoc db :event nil)))
      (reg-event-db :event (fn event [db _] (assoc db :event true)))))

(deftest catch-exception-in-dispath
  (sut/run-test-flow
    (new-state)
    [(fn [_] (throw (js/Error. "an error")))]
    {:on-error (fn [e] (is (instance? js/Error e)))}))

(deftest catch-exception-in-test
  (sut/run-test-flow
    state-with-event
    [{:dispatch [:event]
      :test     (fn atest [db]
                  (throw :error-in-test))}]
    {:on-error (fn [e] (is (= :error-in-test e)))}))

(deftest catch-exception-in-wait-for
  (sut/run-test-flow
    (new-state)
    [{:dispatch       (constantly nil)
      :wait-until     (fn wait-until [db]
                        (throw :error-in-wait-until))}]
    {:on-error (fn [e] (is (= e :error-in-wait-until)))}))


(deftest throws-timeout-after-1-sec
  (sut/run-test-flow
    (new-state)
    [{:dispatch (fn [s] nil)}]
    {:on-error (fn [e] (is (= :TIMEOUT (-> e ex-data :reason))))
     :test-timeout 1000}))

(deftest wait-until-fallback-works
  (sut/run-test-flow
    state-with-event
    [[:init]
     {:dispatch (fn [s] (js/setTimeout #(dispatch s [:event])
                                       1000))
      :test     (fn [db] (is (true? (:event db))))}]
    {:spawn-timeout 200
     :test-timeout 2000}))


