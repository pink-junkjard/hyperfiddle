(ns hypercrud.browser.core
  (:require
    [hypercrud.browser.base :as base]
    [hypercrud.browser.browser-request :as browser-request]))


(def data base/data-from-link)
(def request browser-request/request-from-link)

; this interface is to reuse an evaluated route (performance of eval and effects)
; and also the entrypoint (route is document.location, original anchor is unknowable)
(def data-from-route base/data-from-route)
(def request-from-route browser-request/request-from-route)

#?(:cljs
   (defn ^:deprecated ui-from-route [route ctx & [?class]]
     (hyperfiddle.ui/iframe ctx {:route route :class ?class})))
