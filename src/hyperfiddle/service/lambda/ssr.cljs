(ns hyperfiddle.service.lambda.ssr
  (:require
    [hyperfiddle.service.ssr.core :refer [ssr]]
    [promesa.core :as p]
    [reagent.dom.server :as reagent-server]
    [taoensso.timbre :as timbre]))


(defn render-to-string [env rt event lambda-context callback]
  (-> (ssr env rt (.-path event))
      (p/then (fn [{:keys [http-status-code component]}]
                (callback nil (clj->js {:statusCode http-status-code
                                        :headers {"Content-Type" "text/html"}
                                        :body (str "<!DOCTYPE html>\n" (reagent-server/render-to-string component))}))))
      (p/catch (fn [e]
                 (timbre/error e)
                 (callback nil (clj->js {:statusCode (or (:hyperfiddle.io/http-status-code (ex-data e)) 500)
                                         :headers {"Content-Type" "text/html"}
                                         :body (str "<h2>Fatal error:</h2><h4>" (ex-message e) "</h4>")}))))))
