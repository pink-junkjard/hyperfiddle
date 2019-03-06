(ns hyperfiddle.service.express-js.ssr-stream
  (:require
    [goog.object :as object]
    [hyperfiddle.service.http :refer [handle-route]]
    [hyperfiddle.service.ssr :as ssr]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(defmethod handle-route :ssr [handler env req res]
  (let [domain (object/get req "domain")
        io (ssr/->IOImpl domain (object/get req "jwt"))
        path (.-path req)
        user-id (object/get req "user-id")]
    (-> (ssr/bootstrap-html-cmp env domain io path user-id)
        (p/then (fn [{:keys [http-status-code component]}]
                  (doto res
                    (.status http-status-code)
                    (.type "html")
                    (.write "<!DOCTYPE html>\n"))
                  (let [stream (ssr/render-to-node-stream component)]
                    (.on stream "error" (fn [e]
                                          (timbre/error e)
                                          (.end res (str "<h2>Fatal rendering error:</h2><h4>" (ex-message e) "</h4>"))))
                    (.pipe stream res))))
        (p/catch (fn [e]
                   (timbre/error e)
                   (doto res
                     (.status (or (:hyperfiddle.io/http-status-code (ex-data e)) 500))
                     (.format #js {"text/html" #(.send res (str "<h2>Fatal error:</h2><h4>" (ex-message e) "</h4>"))})))))))
