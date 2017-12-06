(ns hypercrud.api.impl
  (:require [clojure.set :as set]
            [hypercrud.api.core :as api]
            [promesa.core :as p]))


;(defn global-basis [])
;
;(defn local-basis! [global-basis route state-val])
;
;(defn hydrate-route! [local-basis route state-val])
;
;(defn edge [route])
;
;; has datomic dep
;#_(defn hydrate-requests [local-basis requests staged-branches])
;
;; has datomic dep
;#_(defn transact! [tx-groups])
;
;; has datomic dep
;#_(defn sync [dbs])

(defn global-basis [rt]
  (p/resolved nil))

(defn local-basis [rt global-basis]
  (p/resolved global-basis))

(defn hydrate-loop [rt request-fn local-basis stage id->tempid ptm]
  (let [requests (->> (request-fn id->tempid ptm) (into #{}))
        have-requests (set (keys ptm))
        new-requests (set/difference requests have-requests)
        new-requests-vec (into [] new-requests)]
    ; inspect dbvals used in requests see if stage has changed for them
    (if (set/subset? new-requests have-requests)
      (p/resolved {:id->tempid id->tempid
                   :ptm ptm})
      (p/then (api/hydrate-requests rt local-basis stage new-requests-vec)
              (fn [{:keys [pulled-trees id->tempid]}]
                (let [new-ptm (zipmap new-requests-vec pulled-trees)
                      ptm (merge ptm new-ptm)]              ; todo this is a memory leak
                  (hydrate-loop rt request-fn local-basis stage id->tempid ptm)))))))
