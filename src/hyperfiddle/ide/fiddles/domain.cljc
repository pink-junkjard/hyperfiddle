(ns hyperfiddle.ide.fiddles.domain
  (:require [cats.core :as cats]
            [cats.monad.either :as either]
            [clojure.string :as string]
            [contrib.string :refer [safe-read-edn-string]]
            [hypercrud.types.URI #?@(:cljs [:refer [URI]])]
    #?(:cljs [hypercrud.ui.auto-control :as auto-control]))
  #?(:clj
     (:import (java.net URI))))


#?(:cljs
   (defn ^:export domain-ident-renderer [field props ctx]
     (let [ident @(contrib.reactive/cursor (:cell-data ctx) [:domain/ident])
           href (str "http://" ident "." (:hyperfiddle-hostname ctx))]
       [:a {:href href} href])))

#?(:cljs
   (defn ^:export domain-environment-renderer [field props ctx]
     [:div
      [hypercrud.ui.attribute.code/code field props ctx]
      (->> (-> @(:value ctx)
               (safe-read-edn-string)                       ; memoized?
               ; todo something with this error
               (cats/mplus (either/right nil))
               (cats/extract))
           (filter (fn [[k v]] (and (string? k) (string/starts-with? k "$") (instance? URI v))))
           (map (fn [[$db _]]
                  (let [props {:route [(keyword "hyperfiddle.schema" $db)]}]
                    ^{:key $db}
                    [(:navigate-cmp ctx) props (str $db " schema")])))
           (doall))]))
