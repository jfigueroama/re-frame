(ns re-frame.interop
  (:require [goog.async.nextTick]
            [reagent.core]
            [reagent.ratom]))

(def ^boolean -debug-enabled? "@define {boolean}" ^boolean js/goog.DEBUG)

(defn new-state
  [state]
  (merge state {:executor nil
                :on-dispose-callbacks nil
                :debug-enabled? -debug-enabled?}))

(defn state?
  [state]
  (and (contains? state :executor) (contains? state :on-dispose-callbacks)))


(defn next-tick
  [state f]
  {:pre [(state? state)]}
  (goog.async.nextTick f))

(def empty-queue #queue [])

(defn after-render
  [state f]
  {:pre [(state? state)]}
  (reagent.core/after-render f))

;; Make sure the Google Closure compiler sees this as a boolean constant,
;; otherwise Dead Code Elimination won't happen in `:advanced` builds.
;; Type hints have been liberally sprinkled.
;; https://developers.google.com/closure/compiler/docs/js-for-compiler


(defn ratom [x]
  (reagent.core/atom x))

(defn ratom? [x]
  (satisfies? reagent.ratom/IReactiveAtom x))

(defn deref? [x]
  (satisfies? IDeref x))


(defn make-reaction [f]
  (reagent.ratom/make-reaction f))

(defn add-on-dispose!
  [state a-ratom f]
  {:pre [(state? state)]}
  (reagent.ratom/add-on-dispose! a-ratom f))

(defn dispose!
  [state a-ratom]
  {:pre [(state? state)]}
  (reagent.ratom/dispose! a-ratom))

(defn set-timeout! [state f ms] {:pre [(state? state)]} (js/setTimeout f ms))

(defn now []
  (if (and
       (exists? js/performance)
       (exists? js/performance.now))
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
