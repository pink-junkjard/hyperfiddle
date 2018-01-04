(ns hypercrud.api.core
  (:refer-clojure :exclude [sync]))


(defprotocol HypercrudDataAPI
  (global-basis [rt])
  (local-basis [rt global-basis encoded-route foo branch])
  (hydrate-route [rt local-basis encoded-route foo branch stage]) ; returns ptm without stage-val hashes
  (hydrate-requests [rt local-basis stage requests])
  (sync [rt dbs])
  (transact! [rt tx-groups]))
