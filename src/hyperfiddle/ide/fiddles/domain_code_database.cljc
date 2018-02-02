(ns hyperfiddle.ide.fiddles.domain-code-database
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


(defn dbhole-ctx [ctx]
  (let [initial-repository (->> (get-in ctx [:hypercrud.browser/domain :domain/code-databases])
                                (filter #(= (:dbhole/name %) "root"))
                                first
                                (into {}))
        repository (update initial-repository :repository/environment merge {"$" @(:value ctx)})]
    (-> ctx
        (update-in [:hypercrud.browser/domain :domain/code-databases]
                   (fn [repos]
                     (map #(if (= (:dbhole/name %) "root") repository %) repos)))
        (assoc :hypercrud.browser/repository repository))))

(defn bindings [ctx]
  #?(:clj  ctx
     :cljs (-> ctx
               (assoc-in [:fields :dbhole/uri :renderer]
                         (str-and-code
                           (fn [field props ctx]
                             (let [ctx (dbhole-ctx ctx)
                                   control (auto-control/auto-control' (update-in ctx [:fields :dbhole/uri] dissoc :renderer))]
                               [control field props ctx]))))
               (assoc-in [:fields :repository/environment :renderer]
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
                                          (let [props {:route {:code-database @(reactive/cursor (:cell-data ctx) [:dbhole/name])
                                                               :link-id {:ident :schema/all-attributes
                                                                         :dbhole/name (symbol $name)}}}]
                                            ^{:key $name}
                                            [(:navigate-cmp ctx) props (str $name " schema")])))
                                   (doall))]))))))

(defn request [repositories ordered-fes links ctx]
  (let [{[available-pages] true links false} (group-by #(= (:link/rel %) :available-pages) links)]
    (concat
      (browser-request/fiddle-dependent-requests repositories ordered-fes links ctx)
      ; todo support custom request fns at the field level, then this code is 95% deleted
      (let [fe (first ordered-fes)]
        (->> repositories
             (mapcat (fn [repo]
                       (let [ctx (-> ctx
                                     (context/relation (reactive/atom [repo]))
                                     (context/find-element fe 0)
                                     (context/cell-data)
                                     (context/attribute :dbhole/uri)
                                     (context/value (reactive/atom (get repo :dbhole/uri))))]
                         (browser-request/recurse-request available-pages (dbhole-ctx ctx))))))))))
