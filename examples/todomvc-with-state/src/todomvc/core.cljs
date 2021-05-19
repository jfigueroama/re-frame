(ns todomvc.core
  (:require-macros [secretary.core :refer [defroute]])
  (:require [goog.events :as events]
            [reagent.dom]
            [re-frame-lib.core :as rf :refer [dispatch dispatch-sync new-state]]
            [secretary.core :as secretary]
            [todomvc.db]
            [todomvc.events] ;; These two are only required to make the compiler
            [todomvc.subs]   ;; load them (see docs/App-Structure.md)
            [todomvc.views])
  (:import [goog History]
           [goog.history EventType]))

(defn state-for
  [app-id extras]
  (-> (new-state)
      (assoc :ls-key (str "todos-reframe-" app-id))
      (merge extras)
      (todomvc.db/register)
      (todomvc.subs/register)
      (todomvc.events/register)))

(defonce astateA
  (atom (state-for "appA" {})))

(defonce astateB
         (atom (state-for "appB" {})))

;; -- Debugging aids ----------------------------------------------------------
(enable-console-print!)   ;; so that println writes to `console.log`


;; Put an initial value into app-db.
;; The event handler for `:initialise-db` can be found in `events.cljs`
;; Using the sync version of dispatch means that value is in
;; place before we go onto the next step.

;; -- Routes and History ------------------------------------------------------
;; Although we use the secretary library below, that's mostly a historical
;; accident. You might also consider using:
;;   - https://github.com/DomKM/silk
;;   - https://github.com/juxt/bidi
;; We don't have a strong opinion.
;;
;(defroute "/" [] (dispatch [:set-showing :all]))
;(defroute "/:filter" [filter] (dispatch [:set-showing (keyword filter)]))

;(defonce history
;  (doto (History.)
;    (events/listen EventType.NAVIGATE
;                   (fn [^js/goog.History.Event event] (secretary/dispatch! (.-token event)))
;    (.setEnabled true))


;; -- Entry Point -------------------------------------------------------------

(defn render
  [astatea astateB]
  ;; Render the UI into the HTML's <div id="app" /> element
  ;; The view function `todomvc.views/todo-app` is the
  ;; root view for the entire UI.
  (dispatch-sync @astateA [:initialise-db])
  (dispatch-sync @astateB [:initialise-db])
  (reagent.dom/render [todomvc.views/todo-app @astateA]
                      (.getElementById js/document "appA"))
  (reagent.dom/render [todomvc.views/todo-app @astateB]
                      (.getElementById js/document "appB")))

(defn ^:dev/after-load clear-cache-and-render!
  []
  ;; The `:dev/after-load` metadata causes this function to be called
  ;; after shadow-cljs hot-reloads code. We force a UI update by clearing
  ;; the Reframe subscription cache.
  (when (.getElementById js/document "appA")
    (rf/clear-subscription-cache! @astateA)
    (rf/clear-subscription-cache! @astateB)
    (render astateA astateB)))

(defn ^:export main
  []
  (render astateA astateB))
