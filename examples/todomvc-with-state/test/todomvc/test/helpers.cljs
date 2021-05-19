(ns todomvc.test.helpers
  "The test file must contain an app-tests element to hold the containers"
  (:require
    [todomvc.views]
    [reagent.dom]))

(defn create-legend-text
  [test-id showing-state]
  (str " " test-id " | "
       (if @showing-state "Hide" "Show")))

(defn mount-app
  [state eid]
  (when-let [app (.getElementById js/document eid)]
    (reagent.dom/render [todomvc.views/todo-app state] app)))

(defn mount-toggler
  [state test-id showing-state]
  (fn [evt]
    (if @showing-state
      (reagent.dom/unmount-component-at-node
        (js/document.getElementById test-id))
      (mount-app state test-id))
    (swap! showing-state not)
    (set! (.-textContent (.-target evt))
          (create-legend-text test-id showing-state))))


(defn mount-test
  [state test-key-id]
  (let [test-id      (name test-key-id)
        test-root-id (str "test-root-" test-id)]
    (when-let [root (js/document.getElementById "app-tests")]
      (when-let [test-root (js/document.getElementById test-root-id)]
                (reagent.dom/unmount-component-at-node test-root)
                (.removeChild root test-root))
      (let [showing-state (atom false)
            test-root     (js/document.createElement "DIV")
            legend        (js/document.createElement "LEGEND")
            frame         (js/document.createElement "FIELDSET")
            container     (js/document.createElement "DIV")
            new-line      (js/document.createElement "BR")]
        (set! (.-textContent legend)
              (create-legend-text test-id showing-state))
        (.addEventListener legend "click" (mount-toggler state test-id showing-state))
        (.setAttribute container "id" test-id)
        (doto frame
          (.appendChild legend)
          (.appendChild container))
        (doto test-root
          (.appendChild frame)
          (.appendChild new-line)
          (.setAttribute "id" test-root-id))
        (.appendChild root test-root)))))
