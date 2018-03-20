(ns hyperfiddle.io.sync
  #?(:clj
     (:refer-clojure :exclude [sync]))
  (:require #?(:clj [datomic.api :as d])
                    [hyperfiddle.io.http.core :refer [http-request!]]
                    [promesa.core :as p]
                    [taoensso.timbre :as timbre]))


#?(:clj
   (defn sync [dbs]                                         ; sync is the low level datomic call
     (timbre/debug "syncing" (pr-str dbs))
     ; ordered kv seq
     (->> dbs
          (mapcat (juxt identity #(-> % str d/connect d/sync deref d/basis-t)))
          (apply sorted-map))))

(defn sync-rpc! [service-uri dbs]
  (-> (http-request! {:url (str service-uri "sync")
                      :accept :application/transit+json :as :auto
                      :method :post :form dbs
                      :content-type :application/transit+json})
      (p/then (fn [{:keys [body]}]
                (->> body
                     (apply concat)
                     (apply sorted-map))))))
