(ns hyperfiddle.service.express-js
  (:require
    [hyperfiddle.service.ssr.core :refer [render-to-node-stream ssr]]
    [promesa.core :as p]
    [reagent.dom.server :as reagent-server]
    [taoensso.timbre :as timbre]))


(defn render-to-string [env rt req res]
  (-> (ssr env rt (.-path req))
      (p/then (fn [{:keys [http-status-code component]}]
                (doto res
                  (.status http-status-code)
                  (.format #js {"text/html" #(.send res (str "<!DOCTYPE html>\n" (reagent-server/render-to-string component)))}))))
      (p/catch (fn [e]
                 (timbre/error e)
                 (doto res
                   (.status (or (:hyperfiddle.io/http-status-code (ex-data e)) 500))
                   (.format #js {"text/html" #(.send res (str "<h2>Fatal error:</h2><h4>" (ex-message e) "</h4>"))}))))))

(defn render-to-stream [env rt req res]
  (-> (ssr env rt (.-path req))
      (p/then (fn [{:keys [http-status-code component]}]
                (doto res
                  (.status http-status-code)
                  (.type "html")
                  (.write "<!DOCTYPE html>\n"))
                (let [stream (render-to-node-stream component)]
                  (.on stream "error" (fn [e]
                                        (timbre/error e)
                                        (.end res (str "<h2>Fatal rendering error:</h2><h4>" (ex-message e) "</h4>"))))
                  (.pipe stream res))))
      (p/catch (fn [e]
                 (timbre/error e)
                 (doto res
                   (.status (or (:hyperfiddle.io/http-status-code (ex-data e)) 500))
                   (.format #js {"text/html" #(.send res (str "<h2>Fatal error:</h2><h4>" (ex-message e) "</h4>"))}))))))
