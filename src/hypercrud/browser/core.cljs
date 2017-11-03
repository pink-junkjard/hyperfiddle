(ns hypercrud.browser.core
  (:require [hypercrud.browser.browser-request :as browser-request]
            [hypercrud.browser.browser-ui :as browser-ui]

    ; deprecated file
            [hypercrud.browser.context-util]))


(def ui browser-ui/ui)
(def request browser-request/request)

; legacy - this interface is to reuse an evaluated route (performance of eval and effects)
; and also the entrypoint (route is document.location, original anchor is unknowable)
(def ui-from-route browser-ui/ui-from-route)
(def request-from-route browser-request/request-from-route)
