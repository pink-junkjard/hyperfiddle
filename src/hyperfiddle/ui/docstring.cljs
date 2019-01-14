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
     #(some-> % :db/unique :db/ident name))
    attr))

(defn semantic-docstring [ctx & [doc-override]]
  (let [[e a v] @(:hypercrud.browser/eav ctx)
        attr (context/hydrate-attribute ctx a)
        typedoc (some->> @(r/fmap attribute-schema-human attr)
                         (interpose " ") (apply str))
        help-md (blank->nil
                  ; Use path over a because it could have flattened the nesting and attr is ambiguous
                  (str (if typedoc (str "`" (pr-str (:hypercrud.browser/pull-path ctx)) " " typedoc "`\n\n")) ; markdown needs double line-break
                       (or doc-override (some-> @(r/cursor attr [:db/doc]) blank->nil))
                       ))]
    help-md))
