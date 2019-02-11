(ns hyperfiddle.service.express-js.local-basis
  (:require
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.local-basis :as local-basis]
    [hyperfiddle.service.express-js.middleware :refer [platform->express-req-handler]]
    [hyperfiddle.service.http :as http-service :refer [handle-route]]
    [promesa.core :as p]))


(deftype IO [domain jwt ?subject]
  io/IO
  (local-basis [io global-basis route]
    (p/resolved (local-basis/local-basis io global-basis route))))

(defmethod handle-route :local-basis [handler env req res]
  (platform->express-req-handler env (partial http-service/local-basis-handler ->IO) req res))
