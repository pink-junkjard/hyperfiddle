(ns hypercrud.client.reagent
  (:require [cats.monad.exception :as exception]
            [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hypercrud.util.core :as util]
            [reagent.core :as reagent]))


(defn- build-connect-comp [requests]
  (let [tracks (let [force-vec (fn [map-or-vec] (if (map? map-or-vec) (vals map-or-vec) map-or-vec))]
                 (-> requests
                     (update :dbval force-vec)
                     (update :hydrate force-vec)))]
    (reagent/create-class
      {:reagent-render
       (fn [comp & args]
         (let [results (-> requests
                           (update :dbval (fn [map-or-vec]
                                         (let [f #(apply hc/db hc/*peer* %)]
                                           (if (map? map-or-vec)
                                             (util/map-values f map-or-vec)
                                             (map f map-or-vec)))))
                           (update :hydrate (fn [map-or-vec]
                                              (let [f (fn [request]
                                                        (if request
                                                          (hc/hydrate hc/*peer* request)
                                                          (exception/success request)))]
                                                (if (map? map-or-vec)
                                                  (util/map-values f map-or-vec)
                                                  (map f map-or-vec))))))]
           (if (and (empty? (filter exception/failure? (:hydrate results)))
                    (not (empty? (filter #(= % peer/loading-response) (:hydrate results)))))
             [:div "loading" #_[:pre (with-out-str (pprint/pprint requests))]] ; todo spinner
             (apply vector comp results args))))

       :component-did-mount
       (fn [this]
         (add-watch hc/*peer* (goog/getUid this)
                    (fn [k r o n]
                      (when (or
                              ;stage changed?
                              (not (every? (fn [[conn-id branch]]
                                             (= (get-in (:stage o) [conn-id branch])
                                                (get-in (:stage n) [conn-id branch])))
                                           (:dbval tracks)))

                              ;responses changed?
                              (not (every? (fn [request]
                                             (= (get (:ptm o) request)
                                                (get (:ptm n) request)))
                                           (:hydrate tracks))))
                        (reagent/force-update this)))))

       :component-will-unmount
       (fn [this]
         (remove-watch hc/*peer* (goog/getUid this)))})))

(defn connect [requests comp & args]
  (apply vector (build-connect-comp requests) comp args))

;dep:
;  loading?
;  error
;  value
; dep = Option[Exception[Value]]

