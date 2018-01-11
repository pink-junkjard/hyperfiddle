(ns hyperfiddle.ide
  (:require hyperfiddle.appval.domain.app
            #?(:cljs hyperfiddle.appval.domain.app-ui)))



(def domain {})

(defn entrypoint-hyperfiddle-ide [ctx]
  {:code-database "root"
   :link-id :hyperfiddle/main
   :entity #entity["$" (get-in ctx [:target-route :link-id])]})

#?(:cljs
   (defn view [ctx]
     [hyperfiddle.appval.domain.app-ui/ui (entrypoint-hyperfiddle-ide ctx)]))

(defn data [] nil)
