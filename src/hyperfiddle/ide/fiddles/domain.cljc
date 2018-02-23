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
  (-> ctx (update-in [:hypercrud.browser/domain :domain/environment] merge {"$" @(:value ctx)})))

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
                                   (map (fn [[$name _]]
                                          (let [props {:route {:fiddle-id {:ident :schema/all-attributes
                                                                           :dbhole/name (symbol $name)}}}]
                                            ^{:key $name}
                                            [(:navigate-cmp ctx) props (str $name " schema")])))
                                   (doall))]))))))

(defn request [domain ordered-fes links ctx]
  (let [{[available-pages] true links false} (group-by #(= (:link/rel %) :available-pages) links)]
    (concat
      (browser-request/fiddle-dependent-requests domain ordered-fes links ctx)
      ; todo support custom request fns at the field level, then this code is 95% deleted
      (let [fe (first ordered-fes)]
        (let [ctx (-> ctx
                      (context/relation (reactive/atom [domain]))
                      (context/find-element fe 0)
                      (context/cell-data)
                      (context/attribute :domain/home-route)
                      (context/value (reactive/atom (get domain :domain/home-route))))]
          (browser-request/recurse-request available-pages (set-userland-$ ctx)))))))
