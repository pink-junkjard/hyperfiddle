(ns hyperfiddle.ide.routing
  (:require
    [hypercrud.browser.base :as base]
    [hyperfiddle.route :as route]))


(defn ide-route->preview-route [ide-route]
  (let [[_ [preview-fiddle & preview-datomic-args] service-args encoded-fragment] ide-route]
    (route/canonicalize
      (base/legacy-lookup-ref->fiddle-ident preview-fiddle)
      (vec preview-datomic-args)
      service-args
      encoded-fragment)))

(defn preview-route->ide-route [preview-route]
  (let [[preview-fiddle preview-datomic-args service-args encoded-fragment] preview-route]
    (route/canonicalize
      :hyperfiddle.ide/edit
      (into [(base/legacy-fiddle-ident->lookup-ref preview-fiddle)] preview-datomic-args)
      service-args
      encoded-fragment)))
