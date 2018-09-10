(ns hypercrud.browser.q-util
  (:require
    [cats.core :refer [mlet]]
    [cats.monad.either :as either]
    [contrib.data :refer [parse-query-element]]
    [contrib.datomic-tx :refer [smart-identity]]
    [contrib.try$ :refer [try-either]]
    [hypercrud.client.core :as hc]
    [hypercrud.types.Entity :refer [#?(:cljs Entity)]]
    [hypercrud.types.ThinEntity :refer [#?(:cljs ThinEntity)]])
  #?(:clj
     (:import (hypercrud.types.Entity Entity)
              (hypercrud.types.ThinEntity ThinEntity))))


(defn parse-holes [q]
  {:pre [(vector? q)]
   :post [(vector? %)]}
  (as-> (parse-query-element q :in) elements
                                    (if (empty? elements) ['$] elements)
                                    ;; the string conversion should happen at the other side imo
                                    (mapv str elements)))

(defn- fix-param [param]
  (cond
    (instance? Entity param) (smart-identity param)
    (instance? ThinEntity param) (smart-identity param)
    :else param))

(defn validate-query-params+ [q args ctx]
  (mlet [query-holes (try-either (parse-holes q)) #_"normalizes for :in $"
         :let [db-lookup (->> (get-in ctx [:hypercrud.browser/domain :domain/databases])
                              (map (juxt :domain.database/name #(hc/db (:peer ctx) (get-in % [:domain.database/record :database/uri]) (:branch ctx))))
                              (into {}))
               ; Add in named database params that aren't formula params
               [params' unused] (loop [acc []
                                       args args
                                       [x & xs] query-holes]
                                  (let [is-db (.startsWith x "$")
                                        next-arg (if is-db (get db-lookup x)
                                                           (fix-param (first args)))
                                        args (if is-db args (rest args))
                                        acc (conj acc next-arg)]
                                    (if xs
                                      (recur acc args xs)
                                      [acc args])))]]
    ;(assert (= 0 (count (filter nil? params')))) ; datomic will give a data source error
    ; validation. better to show the query and overlay the params or something?
    (cond #_#_(seq unused) (either/left {:message "unused param" :data {:query q :params params' :unused unused}})
      (not= (count params') (count query-holes)) (either/left {:message "missing params" :data {:query q :params params' :unused unused}})
      :else-valid (either/right params'))))
