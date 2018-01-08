(ns hyperfiddle.appval.example-runtime
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hyperfiddle.api :as api]
            [hyperfiddle.appfn.runtime-local :refer [hydrate-requests hydrate-loop]]
            [hyperfiddle.appfn.runtime-rpc :refer [hydrate-requests! sync! transact!!]]
            [hyperfiddle.appval.runtime-local :refer [hydrate-route global-basis local-basis]]
            [hyperfiddle.appval.runtime-rpc :refer [hydrate-route! global-basis! local-basis!]]
            [promesa.core :as p])
  #?(:clj
     (:import (hyperfiddle.api AppFnApi AppValApi))))


(deftype BrowserReferenceRuntime [service-uri state-atom request-fn]
  api/AppValGlobalBasis
  (global-basis [rt] (p/resolved nil))

  api/AppValLocalBasis
  (local-basis [rt global-basis encoded-route foo branch]
    (p/resolved global-basis))

  api/AppValHydrate
  (hydrate-route [rt local-basis encoded-route foo branch stage]
    (let [data-cache (select-keys @state-atom [:id->tempid :ptm])]
      (hydrate-loop rt (partial request-fn encoded-route foo branch) local-basis stage data-cache)))

  api/AppFnHydrate
  (hydrate-requests [rt local-basis stage requests]
    (hydrate-requests! service-uri local-basis stage requests))

  api/AppFnSync
  (sync [rt dbs]
    (sync! service-uri dbs))

  api/AppFnTransact!
  (transact! [rt tx-groups]
    (transact!! service-uri tx-groups))

  hc/Peer
  (hydrate [this request]
    (peer/hydrate state-atom request))

  (db [this uri branch]
    (peer/db-pointer state-atom uri branch))

  ; IEquiv?

  #?@(:cljs [IHash
             (-hash [this] (goog/getUid this))]))
