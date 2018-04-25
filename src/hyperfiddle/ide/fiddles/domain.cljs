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

(defn ^:export domain-environment-renderer [field props ctx]
  [:div
   [hypercrud.ui.attribute.code/code field props ctx]
   (->> (unwrap (safe-read-edn-string @(:value ctx))) ; memoized? todo something with this error
        (filter (fn [[k v]] (and (string? k) (string/starts-with? k "$") (is-uri? v))))
        (map (fn [[$db _]]
               (let [props {:route [(keyword "hyperfiddle.schema" $db)]}]
                 ^{:key $db}
                 [(:navigate-cmp ctx) props (str $db " schema")])))
        (doall))])
