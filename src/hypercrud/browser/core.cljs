(ns hypercrud.browser.core
  (:require [hypercrud.browser.browser-ui :as browser-ui]
            [hypercrud.browser.browser-request :as browser-request]
            [hypercrud.util.core :as util]))

(defn replace-tempids-in-route [tempid-lookup params-map]
  (let [replace-tempid #(or (get tempid-lookup %) %)]
    (-> params-map
        (update :link-dbid replace-tempid)
        ; todo doubtful this works on :entity-dbid-s (now :entity)
        (update :query-params #(util/map-values replace-tempid %)))))

(def safe-ui browser-ui/safe-ui)
(def request browser-request/request)

; legacy - this interface is to reuse an evaluated route (performance of eval and effects)
; and also the entrypoint (route is document.location, original anchor is unknowable)
(def safe-ui' browser-ui/safe-ui')
(def request' browser-request/request')
