(ns hyperfiddle.project
  (:require
    #?(:cljs [contrib.eval-cljs :as eval-cljs])
    [contrib.try$ :refer [try-either]]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.core :as io]
    [hypercrud.types.DbRef :refer [->DbRef]]
    [hypercrud.types.EntityRequest :refer [->EntityRequest]]
    [hypercrud.types.QueryRequest :refer [->QueryRequest]]
    [promesa.core :as p]))


(defn attrs-request [domain branch]
  (->QueryRequest '[:find ?i ?r :where
                    [?attr :attribute/ident ?i]
                    [?attr :attribute/renderer ?r]]
                  [(->DbRef (domain/fiddle-dbname domain) branch)]
                  {:limit nil}))

(defn hydrate-attr-renderers [io domain local-basis branch staged-branches]
  (-> (io/hydrate-one! io local-basis staged-branches (attrs-request domain branch))
      (p/then #(into {} %))))

(defn project-request [domain branch]
  (->EntityRequest
    :hyperfiddle/project
    (->DbRef (domain/fiddle-dbname domain) branch)
    [:project/code
     :project/css]))

(defn hydrate-project-record [io domain local-basis branch staged-branches]
  (io/hydrate-one! io local-basis staged-branches (project-request domain branch)))

#?(:cljs
   (defn eval-domain-code!+ [code-str]
     (try-either (some->> code-str (eval-cljs/eval-statement-str! 'user.domain)))))
