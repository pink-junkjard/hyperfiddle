(ns hyperfiddle.project
  (:require
    [cats.core :as cats]
    #?(:cljs [contrib.eval-cljs :as eval-cljs])
    [contrib.reactive :as r]
    [contrib.try$ :refer [try-either]]
    [hypercrud.client.core :as hc]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.runtime :as runtime]
    [hypercrud.types.DbRef :refer [->DbRef]]
    [hypercrud.types.EntityRequest :refer [->EntityRequest]]
    [hypercrud.types.QueryRequest :refer [->QueryRequest]]))


(defn attrs-request [ctx]
  (->QueryRequest '[:find [(pull ?attr [:attribute/ident :attribute/renderer]) ...]
                    :where [?attr :attribute/ident]]
                  [(->DbRef 'hyperfiddle.domain/fiddle-database (:branch ctx))]))

(let [f (fn [attrs]
          (->> attrs
               (map (juxt :attribute/ident :attribute/renderer))
               (into {})))]
  (defn hydrate-attrs [ctx]
    (r/fmap->> (hc/hydrate (:peer ctx) (:branch ctx) (attrs-request ctx))
               (cats/fmap f))))

(defn project-request [ctx]
  (->EntityRequest
    [:domain/ident (domain/ident (runtime/domain (:peer ctx)))]
    (->DbRef 'hyperfiddle.domain/fiddle-database (:branch ctx))
    [:db/id
     :project/code
     :project/css]))

#?(:cljs
   (defn eval-domain-code!+ [code-str]
     (try-either (some->> code-str (eval-cljs/eval-statement-str! 'user.domain)))))
