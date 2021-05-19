(ns todomvc.views
  (:require [reagent.core  :as reagent]
            [re-frame-lib.core :refer [subscribe dispatch]]
            [clojure.string :as str]))


(defn todo-input [state {:keys [title on-save on-stop]}]
  (let [val  (reagent/atom title)
        stop #(do (reset! val "")
                  (when on-stop (on-stop)))
        save #(let [v (-> @val str str/trim)]
                (on-save v)
                (stop))]
    (fn [state props]
      [:input (merge (dissoc props :on-save :on-stop :title)
                     {:type        "text"
                      :value       @val
                      :auto-focus  true
                      :on-blur     save
                      :on-change   #(reset! val (-> % .-target .-value))
                      :on-key-down #(case (.-which %)
                                      13 (save)
                                      27 (stop)
                                      nil)})])))


(defn todo-item
  []
  (let [editing (reagent/atom false)]
    (fn [state {:keys [id done title]}]
      [:li {:class (str (when done "completed ")
                        (when @editing "editing"))}
        [:div.view
          [:input.toggle
            {:type "checkbox"
             :checked done
             :on-change #(dispatch state [:toggle-done id])}]
          [:label
            {:on-double-click #(reset! editing true)}
            title]
          [:button.destroy
            {:on-click #(dispatch state [:delete-todo id])}]]
        (when @editing
          [todo-input
            state
            {:class "edit"
             :title title
             :on-save #(if (seq %)
                          (dispatch state [:save id %])
                          (dispatch state [:delete-todo id]))
             :on-stop #(reset! editing false)}])])))


(defn task-list
  [{:keys [ls-key]  :as state}]
  (let [visible-todos @(subscribe state [:visible-todos])
        all-complete? @(subscribe state [:all-complete?])]
      [:section#main
        [:input#toggle-all
          {:type "checkbox"
           :checked all-complete?
           :on-change #(dispatch state [:complete-all-toggle])}]
        [:label
          {:for "toggle-all"}
          "Mark all as complete"]
        [:ul#todo-list
          (for [todo  visible-todos]
            ^{:key (:id todo)} [todo-item state todo])]]))


(defn footer-controls
  [state]
  (let [[active done] @(subscribe state [:footer-counts])
        showing       @(subscribe state [:showing])
        a-fn          (fn [filter-kw txt]
                        [:a {:class (when (= filter-kw showing) "selected")
                             :on-click #(dispatch state [:set-showing filter-kw])} txt])]
    [:footer#footer
     [:span#todo-count
      [:strong active] " " (case active 1 "item" "items") " left"]
     [:ul#filters
      [:li (a-fn :all    "All")]
      [:li (a-fn :active "Active")]
      [:li (a-fn :done   "Completed")]]
     (when (pos? done)
       [:button#clear-completed {:on-click #(dispatch state [:clear-completed])}
        "Clear completed"])]))


(defn task-entry
  [state]
  [:header#header
    [:h1 "todos"]
    [todo-input
      state
      {:id "new-todo"
       :placeholder "What needs to be done?"
       :on-save #(when (seq %)
                    (dispatch state [:add-todo %]))}]])


(defn todo-app
  [state]
  [:<>
   [:section#todoapp
    [task-entry state]
    (when (seq @(subscribe state [:todos]))
      [task-list state])
    [footer-controls state]]
   [:footer#info
    [:p "Double-click to edit a todo"]]])
