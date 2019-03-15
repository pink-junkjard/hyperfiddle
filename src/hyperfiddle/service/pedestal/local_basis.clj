(ns hyperfiddle.service.pedestal.local-basis
  (:require
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.local-basis :as local-basis]
    [hyperfiddle.service.http :as http-service]
    [hyperfiddle.service.pedestal.interceptors :refer [def-data-route platform->pedestal-req-handler]]
    [promesa.core :as p]))


(deftype IO [domain jwt ?subject]
  io/IO
  (local-basis [io global-basis route]
    (p/resolved (local-basis/local-basis io global-basis route))))

(def-data-route :local-basis [handler env req]
  (platform->pedestal-req-handler env (partial http-service/local-basis-handler ->IO) req))
