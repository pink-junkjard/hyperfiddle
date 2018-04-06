(ns hyperfiddle.ide.fiddles.domain
  (:require [cats.core :as cats]
            [cats.monad.either :as either]
            [clojure.string :as string]
            [contrib.string :refer [safe-read-edn-string]]
            [hypercrud.browser.browser-request :as browser-request]
            [hypercrud.browser.context :as context]
            [contrib.macros :refer [str-and-code]]
            [hypercrud.types.URI #?@(:cljs [:refer [URI]])]
    #?(:cljs [hypercrud.ui.auto-control :as auto-control])
            [contrib.reactive :as reactive])
  #?(:clj
     (:import (java.net URI))))


#?(:cljs
   (defn ^:export domain-ident-renderer [field props ctx]
     (let [ident @(contrib.reactive/cursor (:cell-data ctx) [:domain/ident])
           href (str "http://" ident "." (:hyperfiddle-hostname ctx))]
       [:a {:href href} href])))

(defn set-userland-$ [ctx]
  ; The user's links & schema are in the userland database, not root
  (let [domain @(:cell-data ctx)
        $ (:domain/fiddle-repo domain)]
    (-> ctx (update-in [:hypercrud.browser/domain :domain/environment] merge {"$" $}))))

#?(:cljs
   (defn ^:export domain-home-route-renderer [field props ctx]
     [hypercrud.ui.attribute.code/code field props (set-userland-$ ctx)]))

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

(defn bindings [ctx] ctx)

(defn request [ctx]
  (let [{[available-pages] true links false} (->> @(:hypercrud.browser/links ctx)
                                                  (group-by #(= (:link/rel %) :available-pages)))]
    (concat
      (browser-request/fiddle-dependent-requests (assoc ctx :hypercrud.browser/links (reactive/atom links)))
      ; todo support custom request fns at the field level, then this code is 95% deleted
      (let [domain @(:hypercrud.browser/result ctx)
            ctx (as-> ctx ctx
                      (context/relation ctx (reactive/atom [domain]))
                      (context/find-element ctx 0)
                      (context/cell-data ctx)
                      (context/field ctx (context/-field-getter-dumb ctx :dbhole/uri))
                      (context/value ctx (reactive/atom (get domain :domain/home-route))))]
        (browser-request/recurse-request available-pages (set-userland-$ ctx))))))
