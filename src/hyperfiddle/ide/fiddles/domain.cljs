(ns hyperfiddle.ide.fiddles.domain
  (:require [cats.core :as cats]
            [cats.monad.either :as either]
            [contrib.data :refer [unwrap]]
            [clojure.string :as string]
            [contrib.string :refer [safe-read-edn-string]]
            [hypercrud.types.URI :refer [is-uri?]]))


(defn ^:export domain-ident-renderer [field props ctx]
  (let [ident @(contrib.reactive/cursor (:cell-data ctx) [:domain/ident])
        href (str "http://" ident "." (:hyperfiddle-hostname ctx))]
    [:a {:href href} href]))
