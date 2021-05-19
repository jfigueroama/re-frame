(ns functional.todomvc.example
  (:require [clojure.test :refer [deftest testing async is]]
            [matcher-combinators.test]
            [re-frame-lib.async-test-flow :refer [run-test-flow]]
            [todomvc.core :refer [state-for]]
            [todomvc.db]
            [todomvc.test.helpers :refer [mount-test]]))

(deftest one (testing "simple stuff" (is (= 1 1))))

(deftest adding-item
  (async
    done
    (let [state (state-for "adding-item"
                           {:fake-local-storage (atom {})})]
      (run-test-flow
        state
        [{:dispatch [:initialise-db]
          :test     (fn [db]
                      (mount-test state :adding-item)
                      (is (= db todomvc.db/default-db)))}
         {:dispatch [:add-todo "first"]
          :test     (fn [db]
                      (is (match? {:todos {1 {:id 1 :title "first" :done false}}}
                                  db)))}
         {:dispatch [:add-todo "second"]
          :wait-until (fn [db]
                        (some #(#{"second"} (:title %))
                              (-> db :todos vals)))
          :test     (fn [db]
                        (is (match? #{{:id 1 :title "first" :done false}
                                      {:id 2 :title "second" :done false}}
                                    (-> db :todos vals set))))}]
        {:done         done
         :test-timeout 10000}))))

(deftest removing-item
  (async
    done
    (let [state (state-for "removing-item"
                           {:fake-local-storage (atom {})})]
      (run-test-flow
        state
        [{:dispatch [:initialise-db]
          :test     #(mount-test state :removing-item)}
         [:add-todo "first"]
         {:dispatch [:add-todo "second"]
          :wait-until (fn [db]
                        (= 2 (count (-> db :todos vals))))}
         {:dispatch [:delete-todo 1]
          :wait-until #(= 1 (count (-> % :todos vals)))
          :test (fn [db] (is (match? {:todos {2 {:id 2 :title "second" :done false}}}
                                     db)))}]
        {:done         done

         :test-timeout 10000}))))
