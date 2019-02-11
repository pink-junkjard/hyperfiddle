(ns hyperfiddle.service.pedestal.hydrate-route
  (:refer-clojure :exclude [sync])
  (:require
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.datomic.hydrate-route :refer [hydrate-route]]
    [hyperfiddle.io.datomic.sync :refer [sync]]
    [hyperfiddle.service.http :as http-service :refer [handle-route]]
    [hyperfiddle.service.pedestal.interceptors :refer [platform->pedestal-req-handler]]
    [promesa.core :as p]))


(deftype IOImpl [domain jwt ?subject]
  io/IO
  (hydrate-route [io local-basis route branch stage]
    (p/do* (hydrate-route domain local-basis route branch stage ?subject)))

  (sync [io dbnames]
    (p/do* (sync domain dbnames))))

(defmethod handle-route :hydrate-route [handler env req]
  (platform->pedestal-req-handler env (partial http-service/hydrate-route-handler ->IOImpl) req))
