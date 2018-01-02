(ns hypercrud.api.core
  (:refer-clojure :exclude [sync]))


(defprotocol HypercrudDataAPI
  (global-basis [rt])
  (local-basis [rt global-basis encoded-route foo])
  (hydrate-route [rt local-basis encoded-route foo stage])
  (hydrate-requests [rt local-basis stage requests])
  (sync [rt dbs])
  (transact! [rt tx-groups]))
