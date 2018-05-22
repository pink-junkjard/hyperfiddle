(ns hyperfiddle.ide.fiddles.domain
  (:require
    [contrib.data :refer [unwrap]]
    [contrib.string :refer [safe-read-edn-string]]
    [hypercrud.types.URI :refer [is-uri?]]))


(defn ^:export domain-ident-renderer [value ctx props]
  (let [href (str "http://" value "." (:hyperfiddle-hostname ctx))]
    [:a {:href href} href]))
