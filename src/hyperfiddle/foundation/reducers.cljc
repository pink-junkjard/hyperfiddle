(ns hyperfiddle.foundation.reducers
  (:require [contrib.data :refer [map-values]]
            [contrib.datomic-tx :as tx]
            [contrib.pprint :refer [pprint-str]]
            [hypercrud.browser.routing :as routing]
            [hypercrud.types.Err :refer [->Err]]
            [hypercrud.util.branch :as branch]))


(defn- serializable-error [e]
  ; need errors to be serializable, so crapily pr-str
  (let [?message #?(:clj (.getMessage e) :cljs (ex-message e))]
    (cond
      (string? e) e
      ?message (assoc (->Err (str ?message)) :data (pprint-str (ex-data e)))
      :else (pr-str e))))

(defn stage-reducer [stage action & args]
  (let [discard (fn [stage branch]
                  (dissoc stage branch))
        with (fn [stage branch uri tx]
               (update-in stage [branch uri] tx/into-tx tx))
        clean (fn [stage]
                (->> stage
                     (map-values (fn [multi-color-tx]
                                   (->> multi-color-tx
                                        (remove (fn [[uri tx]] (nil? tx)))
                                        (into {}))))
                     (remove (fn [[branch multi-color-tx]] (empty? multi-color-tx)))
                     (into {})))]
    (-> (case action
          :transact!-success nil

          :discard-partition (let [[branch] args]
                               (discard stage branch))

          :with (let [[branch uri tx] args]
                  (-> stage
                      (with branch uri tx)))

          :merge (let [[branch] args
                       parent-branch (branch/decode-parent-branch branch)]
                   (-> (reduce (fn [stage [uri tx]]
                                 (with stage parent-branch uri tx))
                               stage
                               (get stage branch))
                       (discard branch)))

          :reset-stage (first args)

          stage)
        clean)))

(defn fatal-error-reducer [error action & args]
  (case action
    :set-global-basis nil
    :set-error (serializable-error (first args))
    error))

(defn pressed-keys-reducer [v action & args]
  (or v #{}))

(defn global-basis-reducer [global-basis action & args]
  (case action
    :set-global-basis (first args)
    global-basis))

(defn domain-reducer [domain action & args]
  (case action
    :hyperfiddle.runtime/set-domain (first args)
    domain))

(defn staging-open-reducer [staging-open? action & args]
  (case action
    :toggle-staging (not staging-open?)
    :partition-error (let [[branch error] args]
                       (nil? branch))
    (or staging-open? false)))

(defn display-mode-reducer [display-mode action & args]
  (case action
    :toggle-display-mode (case display-mode
                           :xray :user
                           :user :xray)
    :set-display-mode (first args)
    (or display-mode :user)))

(defn partitions-reducer [partitions action & args]
  (->> (case action
         :transact!-success (assoc-in partitions [nil :hydrate-id] "hack; dont flicker while page rebuilds")

         :add-partition (let [[branch route branch-aux] args]
                          (update partitions branch
                                  (fn [current-branch]
                                    (if (routing/compare-routes route (:route current-branch))
                                      (assoc current-branch :route route)
                                      {:route route
                                       :hyperfiddle.runtime/branch-aux branch-aux}))))

         :discard-partition (let [[branch] args]
                              (dissoc partitions branch))

         :partition-basis (let [[branch local-basis] args]
                            (assoc-in partitions [branch :local-basis] local-basis))

         :partition-route (let [[branch route] args]
                            (if (= route (get-in partitions [branch :route]))
                              partitions
                              (-> partitions
                                  (assoc-in [branch :route] route)
                                  #_(dissoc :error :ptm :tempid-lookups))))

         :hydrate!-start (let [[branch] args]
                           (update partitions branch
                                   (fn [partition]
                                     (assoc partition
                                       :hydrate-id
                                       (hash (select-keys partition [:hyperfiddle.runtime/branch-aux :route :stage :local-basis]))))))

         :hydrate!-success (let [[branch ptm tempid-lookups] args]
                             (update partitions branch
                                     (fn [partition]
                                       (-> partition
                                           (dissoc :error :hydrate-id)
                                           (assoc :ptm ptm
                                                  :tempid-lookups tempid-lookups)))))

         :partition-error (let [[branch error] args]
                            (update partitions branch
                                    (fn [partition]
                                      (-> partition
                                          (dissoc :hydrate-id)
                                          (assoc :error (serializable-error error))))))

         :open-popover (let [[branch popover-id] args]
                         (update-in partitions [branch :popovers] conj popover-id))

         :close-popover (let [[branch popover-id] args]
                          (update-in partitions [branch :popovers] disj popover-id))

         (or partitions {}))
       (map-values (fn [partition]
                     ; apply defaults
                     (->> {:hydrate-id identity
                           :popovers #(or % #{})

                           ; data needed to hydrate a partition
                           :route identity
                           :hyperfiddle.runtime/branch-aux identity
                           :stage identity
                           :local-basis identity

                           ; response data of hydrating a partition
                           :error identity
                           :ptm identity
                           :tempid-lookups identity}
                          (reduce (fn [v [k f]] (update v k f)) partition))))))

(defn auto-transact-reducer [auto-tx action & args]
  (case action
    :set-auto-transact (first args)
    :enable-auto-transact true
    :disable-auto-transact false
    (if (boolean? auto-tx)
      auto-tx
      true)))

(def reducer-map {:hyperfiddle.runtime/fatal-error fatal-error-reducer
                  :hyperfiddle.runtime/domain domain-reducer
                  :hyperfiddle.runtime/global-basis global-basis-reducer
                  :hyperfiddle.runtime/partitions partitions-reducer
                  :hyperfiddle.runtime/auto-transact auto-transact-reducer

                  ; user
                  :display-mode display-mode-reducer
                  :staging-open staging-open-reducer
                  :pressed-keys pressed-keys-reducer

                  ; needs migration
                  :stage stage-reducer
                  })
