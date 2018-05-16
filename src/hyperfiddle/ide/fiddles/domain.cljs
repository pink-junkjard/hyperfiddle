(ns hyperfiddle.ide.fiddles.domain
  (:require
    [contrib.data :refer [unwrap]]
    [contrib.string :refer [safe-read-edn-string]]
    [hypercrud.types.URI :refer [is-uri?]]))


(defn ^:export domain-ident-renderer [props ctx]
  (let [ident @(contrib.reactive/cursor (:cell-data ctx) [:domain/ident])
        href (str "http://" ident "." (:hyperfiddle-hostname ctx))]
    [:a {:href href} href]))
