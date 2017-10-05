(ns hypercrud.browser.core
  (:require [hypercrud.browser.browser-request :as browser-request]
            [hypercrud.browser.browser-ui :as browser-ui]
            [hypercrud.util.core :as util]))

; todo migrate transact to routing/invert-dbids
(defn replace-tempids-in-route [tempid-lookup params-map]
  (let [replace-tempid #(or (get tempid-lookup %) %)]
    (-> params-map
        (update :link-dbid replace-tempid)
        ; todo doubtful this works on :entity-dbid-s (now :entity)
        (update :query-params #(util/map-values replace-tempid %)))))

(def ui browser-ui/ui)
(def request browser-request/request)

; legacy - this interface is to reuse an evaluated route (performance of eval and effects)
; and also the entrypoint (route is document.location, original anchor is unknowable)
(def ui-from-route browser-ui/ui-from-route)
(def request-from-route browser-request/request-from-route)
