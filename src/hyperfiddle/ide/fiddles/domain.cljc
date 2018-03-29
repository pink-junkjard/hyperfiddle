(ns hyperfiddle.ide.fiddles.domain
  (:require [cats.core :as cats]
            [cats.monad.either :as either]
            [clojure.string :as string]
            [hypercrud.browser.browser-request :as browser-request]
            [hypercrud.browser.context :as context]
            [hypercrud.compile.macros :refer [str-and-code]]
            [hypercrud.types.URI #?@(:cljs [:refer [URI]])]
    #?(:cljs [hypercrud.ui.auto-control :as auto-control])
            [hypercrud.util.reactive :as reactive]
            [hypercrud.util.string :as hc-string])
  #?(:clj
     (:import (java.net URI))))


(defn set-userland-$ [ctx]
  ; The user's links & schema are in the userland database, not root
  (let [domain @(:cell-data ctx)
        $ (:domain/fiddle-repo domain)]
    (-> ctx (update-in [:hypercrud.browser/domain :domain/environment] merge {"$" $}))))

(defn bindings [ctx]
  #?(:clj  ctx
     :cljs (-> ctx
               (assoc-in [:fields :domain/ident :renderer]
                         (hypercrud.compile.macros/str-and-code'
                           (fn [maybe-field props ctx]
                             [:div.value
                              [hypercrud.ui.auto-control/auto-control nil nil nil ctx]
                              (let [href (str "http://" @(hypercrud.util.reactive/cursor (:cell-data ctx) [:domain/ident]) "." (:hyperfiddle-hostname ctx))]
                                [:a {:href href} href])])
                           "todo we dont need a str repr hyperfiddle/hyperfiddle#60"))

               (assoc-in [:fields :domain/home-route :renderer]
                         (str-and-code
                           (fn [field props ctx]
                             (let [ctx (set-userland-$ ctx)
                                   control (auto-control/auto-control' (update-in ctx [:fields :domain/home-route] dissoc :renderer) #_ "guard inifinite recursion")]
                               [control field props ctx]))))

               (assoc-in [:fields :domain/environment :renderer]
                         (str-and-code
                           (fn [field props ctx]
                             [:div
                              [(auto-control/attribute-control ctx) field props ctx]
                              (->> (-> @(:value ctx)
                                       (hc-string/safe-read-edn-string)
                                       ; todo something with this error
                                       (cats/mplus (either/right nil))
                                       (cats/extract))
                                   (filter (fn [[k v]] (and (string? k) (string/starts-with? k "$") (instance? URI v))))
                                   (map (fn [[$db _]]
                                          (let [props {:route [(keyword "hyperfiddle.schema" $db)]}]
                                            ^{:key $db}
                                            [(:navigate-cmp ctx) props (str $db " schema")])))
                                   (doall))]))))))

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
