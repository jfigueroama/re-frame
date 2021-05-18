(ns re-frame-lib.settings
  (:require
    [re-frame-lib.base :refer [state?]]
    [re-frame-lib.interop :as interop]
    [re-frame-lib.loggers :refer [console]]))

(def defaults
  {:loaded?             false
   :global-interceptors interop/empty-queue})

(defn loaded?
  [{:keys [store] :as state}]
  {:pre [(state? state)]}
  (:loaded? @store))

(defn -replace-global-interceptor
  [state global-interceptors interceptor]
  (reduce
    (fn [ret existing-interceptor]
      (if (= (:id interceptor)
             (:id existing-interceptor))
        (do
          (when interop/debug-enabled?
            (when (not (loaded? state))
              (console :warn "re-frame: replacing duplicate global interceptor id: " (:id interceptor))))
          (conj ret interceptor))
        (conj ret existing-interceptor)))
    interop/empty-queue
    global-interceptors))

(defn reg-global-interceptor
  [{:keys [store] :as state}
   {:keys [id] :as interceptor}]
  {:pre [(state? state)]}
  (swap! store update :global-interceptors
         (fn [global-interceptors]
           (let [ids (map :id global-interceptors)]
             (if (some #{id} ids)
               ;; If the id already exists we replace it in-place to maintain the ordering of
               ;; global interceptors esp during hot-code reloading in development.
               (-replace-global-interceptor state global-interceptors interceptor)
               (conj global-interceptors interceptor))))))

(defn get-global-interceptors
  [{:keys [store] :as state}]
  {:pre [(state? state)]}
  (:global-interceptors @store))

(defn clear-global-interceptors
  ([{:keys [store] :as state}]
   {:pre [(state? state)]}
   (swap! store assoc :global-interceptors interop/empty-queue))
  ([{:keys [store] :as state} id]
   {:pre [(state? state)]}
   (swap! store update :global-interceptors
          (fn [global-interceptors]
            (into interop/empty-queue (remove #(= id (:id %)) global-interceptors))))))
