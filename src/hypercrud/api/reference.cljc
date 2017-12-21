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

  (local-basis [rt]
    (local-basis rt (:global-basis @state-atom)))

  (hydrate-route [rt]
    (let [{:keys [id->tempid local-basis ptm stage]} @state-atom]
      (api-util/hydrate-loop rt request-fn local-basis stage id->tempid ptm)))

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
