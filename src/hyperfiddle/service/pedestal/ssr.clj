(ns hyperfiddle.service.pedestal.ssr
  (:require
    [clojure.core.async :refer [chan put!]]
    [hiccup.core :as hiccup]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.datomic.hydrate-requests :refer [hydrate-requests]]
    [hyperfiddle.io.datomic.hydrate-route :refer [hydrate-route]]
    [hyperfiddle.io.datomic.sync :as sync]
    [hyperfiddle.io.global-basis :as global-basis]
    [hyperfiddle.io.local-basis :as local-basis]
    [hyperfiddle.service.http :refer [handle-route]]
    [hyperfiddle.service.ssr :as ssr]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(deftype IOImpl [domain ?subject]
  io/IO
  (global-basis [io]
    (global-basis/global-basis io domain))

  (local-basis [io global-basis route]
    (p/resolved (local-basis/local-basis io global-basis route)))

  (hydrate-requests [io local-basis staged-branches requests]
    (p/do* (hydrate-requests domain local-basis requests staged-branches nil)))

  (hydrate-route [io local-basis route branch-id stage]
    (hydrate-route domain local-basis route branch-id stage ?subject))

  (sync [io dbnames]
    (p/do* (sync/sync domain dbnames))))

(defmethod handle-route :ssr [handler env context]
  (let [domain (get-in context [:request :domain])
        user-id (get-in context [:request :user-id])
        io (->IOImpl domain user-id)
        route (->> (str (get-in context [:request :path-info]) "?" (get-in context [:request :query-string]))
                   (domain/url-decode domain))
        channel (chan)]
    (-> (ssr/bootstrap-html-cmp env domain io route user-id)
        (p/then (fn [{:keys [http-status-code component]}]
                  {:status http-status-code
                   :headers {"Content-Type" "text/html"}
                   :body (str "<!DOCTYPE html>\n" (hiccup/html (apply (first component) (rest component))))}))
        (p/catch (fn [e]
                   (timbre/error e)
                   {:status (or (:hyperfiddle.io/http-status-code (ex-data e)) 500)
                    :headers {"Content-Type" "text/html"}
                    :body (str "<h2>Fatal error:</h2><h4>" (ex-message e) "</h4>")}))
        (p/then #(put! channel (assoc context :response %))))
    channel))
