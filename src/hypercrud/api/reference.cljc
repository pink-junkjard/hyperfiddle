(ns hypercrud.api.reference
  (:require [hypercrud.api.core :refer [#?(:cljs HypercrudDataAPI)]]
            [hypercrud.api.http :as http]
            [hypercrud.api.impl.global-basis :refer [global-basis]]
            [hypercrud.api.impl.local-basis :refer [local-basis]]
            [hypercrud.api.util :as api-util]
            [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer])
  #?(:clj
     (:import (hypercrud.api.core HypercrudDataAPI))))


(deftype BrowserReference [service-uri state-atom request-fn]
  HypercrudDataAPI
  (global-basis [rt]
    (global-basis rt))

  (local-basis [rt global-basis encoded-route foo branch]
    (local-basis rt global-basis encoded-route foo))

  (hydrate-route [rt local-basis encoded-route foo branch stage]
    (let [data-cache (select-keys @state-atom [:id->tempid :ptm])]
      (api-util/hydrate-loop rt (partial request-fn encoded-route foo branch) local-basis stage data-cache)))

  (hydrate-requests [rt local-basis stage requests]
    (http/hydrate-requests! service-uri local-basis stage requests))

  (sync [rt dbs]
    (http/sync! service-uri dbs))

  (transact! [rt tx-groups]
    (http/transact!! service-uri tx-groups))

  hc/Peer
  (hydrate [this request]
    (peer/hydrate state-atom request))

  (db [this uri branch]
    (peer/db state-atom uri branch))

  ; IEquiv?

  #?@(:cljs [IHash
             (-hash [this] (goog/getUid this))]))
