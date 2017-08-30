(ns hypercrud.state.reducers
  (:require [hypercrud.browser.core :as browser]
            [hypercrud.client.tx :as tx]
            [hypercrud.state.core :as state]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.core :as util]))

(defn hydrate-id-reducer [loading? action & args]
  (case action
    :hydrate!-start (first args)
    :hydrate!-success nil
    :hydrate!-failure nil

    (or loading? false)))

(defn stage-reducer [stage action & args]
  (let [discard (fn [stage branch]
                  (dissoc stage branch))
        with (fn [stage branch conn-id tx]
               (update-in stage [branch conn-id] tx/into-tx tx))
        clean (fn [stage]
                (->> stage
                     (util/map-values (fn [multi-color-tx]
                                        (->> multi-color-tx
                                             (remove (fn [[conn-id tx]] (nil? tx)))
                                             (into {}))))
                     (remove (fn [[branch multi-color-tx]] (empty? multi-color-tx)))
                     (into {})))]
    (case action
      :transact!-success nil

      :discard (let [[branch] args]
                 (-> stage
                     (discard branch)
                     clean))

      :with (let [[branch conn-id tx] args]
              (-> stage
                  (with branch conn-id tx)
                  clean))

      :merge (let [[branch] args
                   parent-branch (branch/decode-parent-branch branch)]
               (-> (reduce (fn [stage [conn-id tx]]
                             (with stage parent-branch conn-id tx))
                           stage
                           (get stage branch))
                   (discard branch)
                   clean))

      :reset-branch (let [[branch multi-color-tx] args]
                      (-> stage
                          (assoc branch multi-color-tx)
                          clean))

      :reset-stage (first args)

      stage)))

(defn route-reducer [route action & args]
  (case action
    :soft-set-route (first args)
    :hard-set-route (first args)

    ; todo we want to overwrite our current browser location with this new url
    ; currently this new route breaks the back button
    :transact!-success (browser/replace-tempids-in-route (first args) route)

    route))

(defn ptm-reducer [ptm action & args]
  (case action
    :set-ptm (first args)

    (or ptm nil)))

(defn error-reducer [error action & args]
  (case action
    :set-ptm nil
    :set-error (first args)
    :hydrate!-failure (first args)
    error))

(defn pressed-keys-reducer [v action & args]
  (or v #{}))

(def root-reducer-map {:hydrate-id hydrate-id-reducer
                       :route route-reducer
                       :stage stage-reducer
                       :ptm ptm-reducer
                       :error error-reducer
                       :pressed-keys pressed-keys-reducer})

(def root-reducer (state/combine-reducers root-reducer-map))
