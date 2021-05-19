(ns unit.todomvc.core
  (:require
   [clojure.test :as t :refer [deftest testing is]]
   [todomvc.core :as sut]))

(deftest example-unit-test
  (testing "checking state atoms"
    (is (map? @sut/astateA))
    (is (map? @sut/astateB))))
