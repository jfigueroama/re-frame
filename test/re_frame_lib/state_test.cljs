(ns re-frame-lib.state-test
  (:require [cljs.test         :as test
                               :refer-macros [is deftest testing async]]
            [reagent.ratom     :as r :refer-macros [reaction]]
            [re-frame-lib.core     :as rf]))

(deftest test-sub-independence
  (testing "app-db"
    (let [db-sub #(rf/reg-sub
                       %
                       :test-sub
                       (fn [db [_]]
                         db))

          stateA (doto (rf/new-state) db-sub)
          stateB (doto (rf/new-state) db-sub)
          test-subA (rf/subscribe stateA [:test-sub])
          test-subB (rf/subscribe stateB [:test-sub])]
    (is (= @test-subA @test-subB))
    (is (not= test-subA test-subB))
    (reset! (:app-db stateA) {:new-state :A})
    (is (not= @test-subA @test-subB))
    (is (= @test-subA {:new-state :A}))
    (reset! (:app-db stateB) {:new-state :B})
    (is (= @test-subB {:new-state :B}))
    
    )))

(deftest test-sub-event-independence
  (let [db-sub #(rf/reg-sub % :db (fn [db _] db))
        inc-evt
        #(rf/reg-event-db % :inc-counter (fn [db _] (update db :counter inc)))
        stateA (rf/new-state)
        stateB (rf/new-state) ]
    (doseq [s [stateA stateB]]
      (reset! (:app-db s) {:counter 0})
      (doto s db-sub inc-evt))
    (rf/dispatch stateA [:inc-counter])
    (async done
      (js/setTimeout (fn [] (js/console.log "Async testing")
                       (let [sub-db-a (rf/subscribe stateA [:db])
                             sub-db-b (rf/subscribe stateB [:db])]
                         (is (= @sub-db-a {:counter 1}))
                         (is (= @sub-db-b {:counter 0}))
                         (done))) 500))))


