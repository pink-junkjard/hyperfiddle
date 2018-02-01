(ns hyperfiddle.io.domain
  (:require [hypercrud.client.peer :as peer]
            [hypercrud.client.core :as hc]
            [hypercrud.http.core :refer [http-request!]]
            [hyperfiddle.foundation :as foundation]
    #?(:clj
            [hyperfiddle.io.hydrate-requests :refer [hydrate-one!']])
            [hyperfiddle.io.hydrate-requests :refer [hydrate-one-rpc!']]
            [hypercrud.client.core :as hc]))


#?(:clj
   (defn fetch-domain! [hostname hyperfiddle-hostname domain-basis stage]
     (let [local-basis (into {} [domain-basis])
           request (foundation/domain-request
                     (foundation/hostname->hf-domain-name hostname hyperfiddle-hostname)
                     (reify hc/Peer (db [_ uri branch] (peer/db-pointer uri branch))) #_"type tetris")]
       (hydrate-one!' local-basis stage request))))

(defn fetch-domain-rpc! [hostname hyperfiddle-hostname service-uri domain-basis stage]
  (let [local-basis (into {} [domain-basis])
        request (foundation/domain-request
                  (foundation/hostname->hf-domain-name hostname hyperfiddle-hostname)
                  (reify hc/Peer (db [_ uri branch] (peer/db-pointer uri branch))) #_"type tetris")]
    (hydrate-one-rpc!' service-uri local-basis stage request)))
