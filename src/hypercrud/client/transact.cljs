(ns hypercrud.client.transact
  (:require [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [hypercrud.client.v-not-nil :refer [v-not-nil?]]
            [hypercrud.client.transit :as transit]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.core :as util]
            [kvlt.core :as kvlt]
            [promesa.core :as p]
            [clojure.string :as string]
            [cuerdas.core :as str]
            [cats.monad.either :as either]
            [hypercrud.util.base-64-url-safe :as base-64-url-safe]))


; still weird complected as called from browser and node with different service-uri
(defn transact! [service-uri htx-groups]
  (let [htx-groups (->> (get htx-groups nil)
                        (util/map-values (partial filter v-not-nil?)))]
    (-> (kvlt/request!
          {:url (str (.-uri-str service-uri) "transact")
           :accept :application/transit+json :as :auto
           :method :post :form htx-groups
           :content-type :application/transit+json})
        (p/then (fn [resp]
                  (if (= 200 (:status resp))
                    ; clear master stage
                    ; but that has to be transactional with a redirect???
                    (p/resolved (:body resp))
                    (p/rejected resp)))))))
