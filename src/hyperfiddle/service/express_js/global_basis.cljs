(ns hyperfiddle.service.express-js.global-basis
  (:require
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.global-basis :refer [global-basis]]
    [hyperfiddle.io.http-client :as http-client]
    [hyperfiddle.service.express-js.middleware :refer [platform->express-req-handler]]
    [hyperfiddle.service.http :as http-service :refer [handle-route]]))


(deftype IOImpl [domain service-uri jwt ?subject]
  io/IO
  (global-basis [io]
    (global-basis io domain))

  (sync [io dbnames]
    (http-client/sync! domain service-uri dbnames jwt)))

(defmethod handle-route :global-basis [handler env req res]
  (platform->express-req-handler env (partial http-service/global-basis-handler ->IOImpl) req res))
