(ns re-frame.loggers
  (:require
   [clojure.set :refer [difference]]))

(defn console
  [state level & args]
  (assert (contains?
            @(:loggers state) level)
          (str "re-frame: log called with unknown level: " level))
  (apply (level @(:loggers state)) args))


(defn set-loggers!
  "Change the set (or a subset) of logging functions used by re-frame.
  `new-loggers` should be a map with the same keys as `loggers` (above)"
  [state new-loggers]
  (assert  (empty? (difference
                     (set (keys new-loggers))
                     (-> @(:loggers state) keys set))) "Unknown keys in new-loggers")
  (swap! (:loggers state) merge new-loggers))

(defn get-loggers
  "Get the current logging functions used by re-frame."
  [state]
  @(:loggers state))
