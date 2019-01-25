(ns hyperfiddle.project
  (:require
    #?(:cljs [contrib.eval-cljs :as eval-cljs])
    [contrib.try$ :refer [try-either]]
    [hyperfiddle.io.core :as io]
    [hypercrud.types.DbRef :refer [->DbRef]]
    [hypercrud.types.EntityRequest :refer [->EntityRequest]]
    [hypercrud.types.QueryRequest :refer [->QueryRequest]]
    [promesa.core :as p]))


(defn attrs-request [branch]
  (->QueryRequest '[:find ?i ?r :where
                    [?attr :attribute/ident ?i]
                    [?attr :attribute/renderer ?r]]
                  [(->DbRef 'hyperfiddle.domain/fiddle-database branch)]))

(defn hydrate-attr-renderers [io local-basis branch staged-branches]
  (-> (io/hydrate-one! io local-basis staged-branches (attrs-request branch))
      (p/then #(into {} %))))

(defn project-request [branch]
  (->EntityRequest
    :hyperfiddle/project
    (->DbRef 'hyperfiddle.domain/fiddle-database branch)
    [:project/code
     :project/css]))

(defn hydrate-project-record [io local-basis branch staged-branches]
  (io/hydrate-one! io local-basis staged-branches (project-request branch)))

#?(:cljs
   (defn eval-domain-code!+ [code-str]
     (try-either (some->> code-str (eval-cljs/eval-statement-str! 'user.domain)))))
