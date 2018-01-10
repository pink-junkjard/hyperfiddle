(ns hyperfiddle.ide.fiddles.domain-code-database
  (:require [cats.core :as cats]
            [cats.monad.either :as either]
            [clojure.string :as string]
            [hypercrud.browser.browser-request :as browser-request]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.link :as link]
            [hypercrud.compile.macros :refer [str-and-code]]
            [hypercrud.types.URI #?@(:cljs [:refer [URI]])]
    #?(:cljs [hypercrud.ui.auto-control :refer [attribute-control]])
            [hypercrud.util.reactive :as reactive]
            [hypercrud.util.string :as hc-string])
  #?(:clj
     (:import (java.net URI))))


(let [f (memoize (fn [name] (reactive/atom {:db/id name})))]
  (defn prep-ctxs [ctx]
    (->> (-> @(:value ctx)
             (hc-string/safe-read-edn-string)
             ; todo something with this error
             (cats/mplus (either/right nil))
             (cats/extract))
         (filter (fn [[k v]] (and (string? k) (string/starts-with? k "$") (instance? URI v))))
         (map (fn [[name uri]]
                (assoc ctx
                  :cell-data (f name)                       ; add a fake entity so modals/branches are unique
                  :name name
                  :uri uri))))))

(defn bindings [ctx]
  #?(:clj  ctx
     :cljs (assoc-in
             ctx [:fields :repository/environment :renderer]
             (str-and-code
               (fn [field links props ctx]
                 (let [foo (group-by #(= (:link/rel %) :attributes-for-database) links)
                       {[attrs-for-db] true links false} foo]
                   [:div
                    [(attribute-control ctx) field links props ctx]
                    (->> (prep-ctxs ctx)
                         (map (fn [ctx]
                                (let [props (link/build-link-props attrs-for-db ctx)]
                                  ^{:key (:uri ctx)}
                                  [(:navigate-cmp ctx) props (str (:name ctx) " schema")])))
                         (doall))]))))))

(defn request [repositories ordered-fes links ctx]
  (let [foo (group-by #(= (:link/rel %) :attributes-for-database) links)
        {[attrs-for-db] true links false} foo]
    (concat
      (browser-request/fiddle-dependent-requests repositories ordered-fes links ctx)
      ; todo support custom request fns at the field level, then this code is 95% deleted
      (let [fe (first ordered-fes)]
        (->> repositories
             (mapcat (fn [repo]
                       (let [ctx (-> ctx
                                     (context/find-element fe 0)
                                     (context/cell-data)
                                     (context/attribute :repository/environment)
                                     (context/value (get repo :repository/environment)))]
                         (->> (prep-ctxs ctx)
                              (mapcat #(browser-request/recurse-request attrs-for-db %))
                              (remove nil?))))))))))
