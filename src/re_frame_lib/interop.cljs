(ns re-frame.interop
  (:require [goog.async.nextTick]
            [reagent.core]
            [reagent.ratom]))

(def next-tick goog.async.nextTick)

(def empty-queue #queue [])

(def after-render reagent.core/after-render)

;; Make sure the Google Closure compiler sees this as a boolean constant,
;; otherwise Dead Code Elimination won't happen in `:advanced` builds.
;; Type hints have been liberally sprinkled.
;; https://developers.google.com/closure/compiler/docs/js-for-compiler
(def ^boolean debug-enabled? "@define {boolean}" ^boolean js/goog.DEBUG)

(defn ratom [x]
  (reagent.core/atom x))

(defn ratom? [x]
  (satisfies? reagent.ratom/IReactiveAtom x))

(defn deref? [x]
  (satisfies? IDeref x))


(defn make-reaction [f]
  (reagent.ratom/make-reaction f))

(defn add-on-dispose! [a-ratom f]
  (reagent.ratom/add-on-dispose! a-ratom f))

(defn dispose! [a-ratom]
	(reagent.ratom/dispose! a-ratom))

(defn set-timeout! [f ms]
  (js/setTimeout f ms))

(defn now []
  (if (exists? js/performance.now)
    (js/performance.now)
    (js/Date.now)))

(defn reagent-id
  "Produces an id for reactive Reagent values
  e.g. reactions, ratoms, cursors."
  [reactive-val]
  (when (implements? reagent.ratom/IReactiveAtom reactive-val)
    (str (condp instance? reactive-val
           reagent.ratom/RAtom "ra"
           reagent.ratom/RCursor "rc"
           reagent.ratom/Reaction "rx"
           reagent.ratom/Track "tr"
           "other")
         (hash reactive-val))))

; FIX move this to other place
(defn new-state
  []
  {
   :app-db (ratom {})
   :debug-enabled? false
   :kind->id->handler (atom {})
   :event-queue (->EventQueue :idle empty-queue {})  ; FIX cyclic dependency
   :*handling* (atom nil)
   :loggers (atom {:log       (js/console.log.bind   js/console)
                  :warn      (js/console.warn.bind  js/console)
                  :error     (js/console.error.bind js/console)
                  :group     (if (.-group js/console)         ;; console.group does not exist  < IE 11
                               (js/console.group.bind js/console)
                               (js/console.log.bind   js/console))
                  :groupEnd  (if (.-groupEnd js/console)        ;; console.groupEnd does not exist  < IE 11
                               (js/console.groupEnd.bind js/console)
                               #())})})
