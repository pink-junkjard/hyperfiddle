(ns hyperfiddle.ui.docstring
  (:require
    [contrib.string :refer [blank->nil]]
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
     #(some-> % :db/unique :db/ident name))
    attr))

(defn semantic-docstring [ctx & [doc-override]]
  (let [path (:hypercrud.browser/path ctx)
        attr (context/hydrate-attribute! ctx (last (:hypercrud.browser/path ctx)))
        typedoc (some->> (attribute-schema-human attr)
                         (interpose " ") (apply str))
        help-md (blank->nil
                  (str (if typedoc (str "`" (pr-str path) " " typedoc "`\n\n")) ; markdown needs double line-break
                       ;"`" (pprint-str @(:hypercrud.browser/field ctx)) "`\n\n" ; debug
                       (or doc-override (blank->nil (:db/doc attr)))
                       ))]
    help-md))
