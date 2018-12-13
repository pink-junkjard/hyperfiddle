(ns hyperfiddle.service.express-js.ssr
  (:require
    [goog.object :as object]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.service.http :as http-service :refer [handle-route]]
    [hyperfiddle.service.ssr :as ssr]
    [promesa.core :as p]
    [reagent.dom.server :as reagent-server]
    [taoensso.timbre :as timbre]))


(defmethod handle-route :ssr [handler env req res]
  (let [host-env (object/get req "host-env")
        user-id (object/get req "user-id")
        path (.-path req)
        redirect #(.redirect res %)
        next (fn []
               (let [rt (ssr/build-runtime host-env {::runtime/user-id user-id} (object/get req "jwt"))]
                 (-> (ssr/bootstrap-html-cmp env rt (.-path req))
                     (p/then (fn [{:keys [http-status-code component]}]
                               (doto res
                                 (.status http-status-code)
                                 (.format #js {"text/html" #(.send res (str "<!DOCTYPE html>\n" (reagent-server/render-to-string component)))}))))
                     (p/catch (fn [e]
                                (timbre/error e)
                                (doto res
                                  (.status (or (:hyperfiddle.io/http-status-code (ex-data e)) 500))
                                  (.format #js {"text/html" #(.send res (str "<h2>Fatal error:</h2><h4>" (ex-message e) "</h4>"))})))))))]
    (http-service/ssr-auth-hack host-env user-id path redirect next)))
