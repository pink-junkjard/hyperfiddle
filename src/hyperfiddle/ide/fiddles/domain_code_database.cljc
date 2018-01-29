(ns hyperfiddle.ide.fiddles.domain-code-database
  (:require [cats.core :as cats]
            [cats.monad.either :as either]
            [clojure.string :as string]
            [hypercrud.compile.macros :refer [str-and-code]]
            [hypercrud.types.URI #?@(:cljs [:refer [URI]])]
    #?(:cljs [hypercrud.ui.auto-control :refer [attribute-control]])
            [hypercrud.util.reactive :as reactive]
            [hypercrud.util.string :as hc-string])
  #?(:clj
     (:import (java.net URI))))


(defn bindings [ctx]
  #?(:clj  ctx
     :cljs (assoc-in
             ctx [:fields :repository/environment :renderer]
             (str-and-code
               (fn [field props ctx]
                 [:div
                  [(attribute-control ctx) field props ctx]
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
                       (doall))])))))
