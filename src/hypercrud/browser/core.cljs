(ns hypercrud.browser.core
  (:require [hypercrud.browser.base :as base]
            [hypercrud.browser.browser-request :as browser-request]
            [hypercrud.browser.browser-ui :as browser-ui]

    ; deprecated file
            [hypercrud.browser.context-util]))


(def data base/data-from-anchor)
(def ui browser-ui/ui-from-anchor)
(def request browser-request/request-from-anchor)

; this interface is to reuse an evaluated route (performance of eval and effects)
; and also the entrypoint (route is document.location, original anchor is unknowable)
(def data-from-route base/data-from-route)
(def ui-from-route browser-ui/ui-from-route)
(def request-from-route browser-request/request-from-route)
