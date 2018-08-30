(ns hyperfiddle.ui.docstring
  (:require
    [contrib.string :refer [blank->nil]]
    [contrib.pprint :refer [pprint-str]]
    [contrib.reactive :as r]
    [cuerdas.core :as str]
    [hypercrud.browser.context :as context]))


(defn fqn->name [s]
  (-> s
      (str/split "\n" 2)
      (first)

      ; both cljs and js work with cljs eval
      (as-> s (if (str/includes? s "/")
                (str/split s "/")
                (str/split s ".")))
      last))

(defn attribute-schema-human [attr]
  ((juxt
     #_:db/ident
     #(some-> % :db/valueType :db/ident name)
     #(some-> % :db/cardinality :db/ident name)
     #(some-> % :db/isComponent (if :component) name)
     #(some-> % :db/unique :db/ident name)
     #(some-> % :attribute/renderer fqn->name))
    attr))

(defn semantic-docstring [ctx]
  (let [path (:hypercrud.browser/path ctx)
        field @(:hypercrud.browser/field ctx)               ; for debug
        attr (context/hydrate-attribute ctx (last (:hypercrud.browser/path ctx)))
        dbdoc (some-> @(r/cursor attr [:db/doc]) blank->nil)
        typedoc (some->> @(r/fmap attribute-schema-human attr)
                         (interpose " ") (apply str))
        help-md (blank->nil
                  (str                                      ;"`" (pprint-str field) "`\n\n" ; debug
                       (if typedoc (str "`" (pr-str path) " " typedoc "`\n\n")) ; markdown needs double line-break
                       dbdoc
                       ))]
    help-md))
