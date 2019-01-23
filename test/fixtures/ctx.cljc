(ns fixtures.ctx
  (:require
    [contrib.reactive :as r]
    [contrib.reader]
    [hyperfiddle.runtime :as runtime]))

(def query-coll
  '[:find
    [(pull ?e [:reg/email
               :reg/age
               *
               {:reg/gender [:db/ident]
                :reg/shirt-size [:db/ident
                                 *]}
               :db/id])
     ...]
    :where
    [?e :reg/email]
    [?e :reg/age ?age] [(> ?age 49)]])

(def result-coll
  [{:db/id 17592186046765,
    :hyperfiddle/owners [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"],
    :reg/email "elizabeth@example.com",
    :reg/age 65,
    :reg/gender {:db/ident :gender/female},
    :reg/shirt-size
    {:db/id 17592186046209,
     :db/ident :shirt-size/womens-medium,
     :hyperfiddle/owners [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"],
     :reg/gender {:db/id 17592186046204}}}
   {:db/id 17592186046317,
    :hyperfiddle/owners [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"],
    :reg/email "alice@example.com",
    :reg/name "Alice",
    :reg/age 50,
    :reg/gender {:db/ident :gender/female},
    :reg/shirt-size
    {:db/id 17592186046209,
     :db/ident :shirt-size/womens-medium,
     :hyperfiddle/owners [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"],
     :reg/gender {:db/id 17592186046204}}}])

(def schema
  {:db/excise {:db/id 15, :db/ident :db/excise, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}},
   :hypercrud/props {:db/id 88, :db/ident :hypercrud/props, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Map of extra user props for the renderer, e.g. `:disabled` `:label-fn` `:tooltip`"},
   :db/fn {:db/id 52, :db/ident :db/fn, :db/valueType {:db/ident :db.type/fn}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "A function-valued attribute for direct use by transactions and queries."},
   :db/index {:db/id 44, :db/ident :db/index, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Property of an attribute. If true, create an AVET index for the attribute. Defaults to false."},
   :db/unique {:db/id 42, :db/ident :db/unique, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Property of an attribute. If value is :db.unique/value, then attribute value is unique to each entity. Attempts to insert a duplicate value for a temporary entity id will fail. If value is :db.unique/identity, then attribute value is unique, and upsert is enabled. Attempting to insert a duplicate value for a temporary entity id will cause all attributes associated with that temporary id to be merged with the entity already in the database. Defaults to nil."},
   :hyperblog.nav/children {:db/id 124, :db/ident :hyperblog.nav/children, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "Each nav item has optional children", :attribute/renderer "hyperfiddle.ui.controls/edn-many"},
   :reg/gender {:db/id 111, :db/ident :reg/gender, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}},
   :db.excise/beforeT {:db/id 17, :db/ident :db.excise/beforeT, :db/valueType {:db/ident :db.type/long}, :db/cardinality {:db/ident :db.cardinality/one}},
   :task/completed {:db/id 109, :db/ident :task/completed, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}},
   :fiddle/pull-database {:db/id 121, :db/ident :fiddle/pull-database, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Argument to `datomic.api/pull`"},
   :fiddle/cljs-ns {:db/id 122, :db/ident :fiddle/cljs-ns, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Undocumented, pending cleanup Q3'18"},
   :fiddle/css {:db/id 105, :db/ident :fiddle/css, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "CSS for this fiddle"},
   :fiddle/pull {:db/id 104, :db/ident :fiddle/pull, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Datomic pull expression. Drives form fields and order. If you omit `:db/id` the form will be read-only."},
   :db.sys/reId {:db/id 9, :db/ident :db.sys/reId, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "System-assigned attribute for an id e in the log that has been changed to id v in the index"},
   :db/valueType {:db/id 40, :db/ident :db/valueType, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Property of an attribute that specifies the attribute's value type. Built-in value types include, :db.type/keyword, :db.type/string, :db.type/ref, :db.type/instant, :db.type/long, :db.type/bigdec, :db.type/boolean, :db.type/float, :db.type/uuid, :db.type/double, :db.type/bigint,  :db.type/uri."},
   :db/txInstant {:db/id 50, :db/ident :db/txInstant, :db/valueType {:db/ident :db.type/instant}, :db/cardinality {:db/ident :db.cardinality/one}, :db/index true, :db/doc "Attribute whose value is a :db.type/instant. A :db/txInstant is recorded automatically with every transaction."},
   :link/formula {:db/id 102, :db/ident :link/formula, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "CLJC expression which specifies target fiddle's query input arguments as a value or list of values. Hyperfiddle provides a default formula where sensible."},
   :fiddle/ident {:db/id 106, :db/ident :fiddle/ident, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "Identifies this fiddle and used in fiddle URLs."},
   :attribute/ident {:db/id 76, :db/ident :attribute/ident, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "Named schema attribute"},
   :fiddle/renderer {:db/id 93, :db/ident :fiddle/renderer, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Reagent hiccup expression for the view. Clear this to restore default."},
   :link/class-string {:db/id 126, :db/ident :link/class-string, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/many}},
   :attribute/renderer {:db/id 77, :db/ident :attribute/renderer, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Fully qualified var like `hyperfiddle.ui.controls/code`. This is not the actual Datomic schema entity because \"source code\" concerns like renderers are not schema."},
   :link/ident {:db/id 91, :db/ident :link/ident, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/value}, :db/doc "archived"},
   :db/noHistory {:db/id 45, :db/ident :db/noHistory, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Property of an attribute. If true, past values of the attribute are not retained after indexing. Defaults to false."},
   :task/title {:db/id 108, :db/ident :task/title, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}},
   :hyperblog.post/related {:db/id 125, :db/ident :hyperblog.post/related, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "This post links to these posts, e.g. a table of contents, or \"if you liked this you might also like\"", :attribute/renderer "hyperfiddle.ui.controls/edn-many"},
   :db/isComponent {:db/id 43, :db/ident :db/isComponent, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Property of attribute whose vtype is :db.type/ref. If true, then the attribute is a component of the entity referencing it. When you query for an entire entity, components are fetched automatically. Defaults to nil."},
   :hyperblog.post/sort-index1 {:db/id 117, :db/ident :hyperblog.post/sort-index1, :db/valueType {:db/ident :db.type/long}, :db/cardinality {:db/ident :db.cardinality/one}}, :db/lang {:db/id 46, :db/ident :db/lang, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Attribute of a data function. Value is a keyword naming the implementation language of the function. Legal values are :db.lang/java and :db.lang/clojure"},
   :hyperblog.post/title {:db/id 116, :db/ident :hyperblog.post/title, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}, :db/fulltext {:db/id 51, :db/ident :db/fulltext, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Property of an attribute. If true, create a fulltext search index for the attribute. Defaults to false."},
   :reg/shirt-size {:db/id 112, :db/ident :reg/shirt-size, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}, :reg/age {:db/id 114, :db/ident :reg/age, :db/valueType {:db/ident :db.type/long}, :db/cardinality {:db/ident :db.cardinality/one}},
   :hyperblog.post/hidden {:db/id 119, :db/ident :hyperblog.post/hidden, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Omit from user-facing view but show in admin dashboards"},
   :link/tx-fn {:db/id 74, :db/ident :link/tx-fn, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "`:button` click handler, a function that can modify the form value before staging as a Datomic transaction. managed+create links have a default `:tx-fn` implementing aparent-child reference."},
   :db/cardinality {:db/id 41, :db/ident :db/cardinality, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Property of an attribute. Two possible values: :db.cardinality/one for single-valued attributes, and :db.cardinality/many for many-valued attributes. Defaults to :db.cardinality/one."},
   :reg/birthdate {:db/id 115, :db/ident :reg/birthdate, :db/valueType {:db/ident :db.type/instant}, :db/cardinality {:db/ident :db.cardinality/one}}, :db/doc {:db/id 62, :db/ident :db/doc, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/fulltext true, :db/doc "Documentation string for an entity."},
   :link/fiddle {:db/id 69, :db/ident :link/fiddle, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Link to other fiddles"},
   :link/rel {:db/id 68, :db/ident :link/rel, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Names the semantic relationship between this fiddle and the target fiddle e.g. `:options`. Widget renderers may use this hint to render views intelligently."},
   :link/path {:db/id 103, :db/ident :link/path, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Associate this link in a specific position of the form, e.g. `\"0 :reg/gender\"` where 0 is the find-element index."}, :link/class {:db/id 127, :db/ident :link/class, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/many}},
   :db.excise/before {:db/id 18, :db/ident :db.excise/before, :db/valueType {:db/ident :db.type/instant}, :db/cardinality {:db/ident :db.cardinality/one}},
   :db/ident {:db/id 10, :db/ident :db/ident, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "Attribute used to uniquely name an entity."},
   :db/code {:db/id 47, :db/ident :db/code, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/fulltext true, :db/doc "String-valued attribute of a data function that contains the function's source code."},
   :fiddle/type {:db/id 99, :db/ident :fiddle/type, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Which Datomic API to use for data fetching, blank means nothing"},
   :fiddle/links {:db/id 89, :db/ident :fiddle/links, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/isComponent true, :db/doc "Links to other fiddles. This is a more advanced feature."},
   :db.install/valueType {:db/id 12, :db/ident :db.install/valueType, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "System attribute with type :db.type/ref. Asserting this attribute on :db.part/db with value v will install v as a value type."},
   :db.alter/attribute {:db/id 19, :db/ident :db.alter/attribute, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "System attribute with type :db.type/ref. Asserting this attribute on :db.part/db with value v will alter the definition of existing attribute v."},
   :db.install/function {:db/id 14, :db/ident :db.install/function, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "System attribute with type :db.type/ref. Asserting this attribute on :db.part/db with value v will install v as a data function."},
   :db.install/partition {:db/id 11, :db/ident :db.install/partition, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "System attribute with type :db.type/ref. Asserting this attribute on :db.part/db with value v will install v as a partition."},
   :reg/name {:db/id 113, :db/ident :reg/name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}},
   :fiddle/markdown {:db/id 107, :db/ident :fiddle/markdown, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Convenient place to store markdown, `:fiddle/renderer` must render this."},
   :fiddle/hydrate-result-as-fiddle {:db/id 123, :db/ident :fiddle/hydrate-result-as-fiddle, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Experimental"},
   :db.install/attribute {:db/id 13, :db/ident :db.install/attribute, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "System attribute with type :db.type/ref. Asserting this attribute on :db.part/db with value v will install v as an attribute."},
   :db.excise/attrs {:db/id 16, :db/ident :db.excise/attrs, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}},
   :reg/email {:db/id 110, :db/ident :reg/email, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}},
   :hyperblog.post/published {:db/id 118, :db/ident :hyperblog.post/published, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}},
   :fressian/tag {:db/id 39, :db/ident :fressian/tag, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/index true, :db/doc "Keyword-valued attribute of a value type that specifies the underlying fressian type used for serialization."},
   :fiddle/query {:db/id 98, :db/ident :fiddle/query, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Datomic query. Database inputs are resolved by name through the `:domain/environment`. Pull `:db/id` for editable forms. Currently no support yet for rules, d/history or d/log."},
   :db.sys/partiallyIndexed {:db/id 8, :db/ident :db.sys/partiallyIndexed, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "System-assigned attribute set to true for transactions not fully incorporated into the index"},
   :hyperblog.post/draft-placeholder {:db/id 120, :db/ident :hyperblog.post/draft-placeholder, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Show in rendered view, but disabled (no link)"}})

(def ctx
  {:hyperfiddle.ui/debug-tooltips true,
   :hypercrud.browser/path [],
   :peer (let [state (r/atom {::runtime/partitions {nil {:schemas {"$" schema}}}})]
           (reify runtime/State
             (runtime/state [_] state)
             (runtime/state [_ path] (r/cursor state path)))),
   :hypercrud.browser/route (r/track identity [:demo/hyper-control nil]),
   :hypercrud.ui/display-mode (r/track identity :hypercrud.browser.browser-ui/xray),
   :hypercrud.browser/fiddle
   (r/track identity {:fiddle/cljs-ns "(contrib.cljs-platform/merge-user!\n  {\"navi\"\n   (fn [ref props ctx]\n     (let [call [:span \"Hey, listen!\"]\n           fairy [:img {:src \"https://i.imgur.com/fkbfjYU.png\"}]]\n       [:div.navi.p.alert.alert-info props call fairy\n        [hyperfiddle.ui/markdown @ref {:hyperfiddle.ui/unp true}]]))})",
                      :fiddle/css ".display-mode-user .-demo-hyper-control div.field.attribute { display: flex; }\n.display-mode-user .-demo-hyper-control div.field.attribute > :first-child { \n  flex: 0 1 8em !important; display: inline; padding-right: 1em; text-align: right; }\n.display-mode-user .-demo-hyper-control div.field.attribute > :nth-child(2) { flex: 1 1; }",
                      :fiddle/ident :demo/hyper-control,
                      :fiddle/renderer "(let [{:keys [:hypercrud.browser/fiddle]} ctx]\n  [:div props\n   [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]])",
                      :db/id 17592186047704,
                      :fiddle/type :query,
                      :fiddle/links ({:db/id 17592186047708,
                                      :link/class [:genders],
                                      :link/fiddle
                                      {:db/id 17592186046527,
                                       :fiddle/ident :demo/gender-options,
                                       :fiddle/query "[:find \n #_(pull ?e [:db/ident])\n #_?e\n ;?e (count ?e)\n [(pull ?e [:db/ident]) ...] \n #_[?e ...]\n\n :where\n [?e :db/ident ?v]\n [(namespace ?v) ?ns]\n [(ground \"gender\") ?ns]]",
                                       :fiddle/type :query, :fiddle/links ()},
                                      :link/rel :hf/iframe,
                                      :link/formula "(constantly nil)"}
                                      {:db/id 17592186047709,
                                       :link/class [:shirt-sizes], :link/fiddle
                                       {:db/id 17592186046391,
                                        :fiddle/ident :demo/shirt-sizes,
                                        :fiddle/query "[:find \n [(pull ?e [:db/ident {:reg/gender [:db/ident]}]) ...]\n :in $ ?gender\n :where \n [?e :db/ident] \n [?e :reg/gender ?gender]]",
                                        :fiddle/type :query, :fiddle/links ()},
                                       :link/path ":reg/gender",
                                       :link/rel :hf/iframe,
                                       :link/formula "identity"}),
                      :fiddle/markdown "### Example registration form\n\n!field[](:reg/email)\n!field[](:reg/name)\n!field[](:reg/age)\n!field[](:reg/birthdate)\n!field[](:reg/gender){options=genders}\n\nWould you like a tee-shirt with that?\n\n!field[](:reg/shirt-size){options=shirt-sizes}\n\n!block[Change `:gender` to `:female` ☝️ and the shirt-size options will update.]{.alert .alert-info}",
                      :fiddle/query "[:find  \n (pull ?e [:db/id \n           :reg/email \n           :reg/name \n           :reg/age\n           :reg/birthdate \n           {:reg/gender [:db/ident]}\n           {:reg/shirt-size [:db/ident]}])\n .\n :where \n [?e :reg/email \"dustin@example.com\"]]"}),
   :hypercrud.browser/field
   (r/track identity '{
                       ;:hypercrud.browser.field/query {:qfind #datascript.parser.FindScalar{:element #datascript.parser.Pull{:source #datascript.parser.SrcVar{:symbol $},
                       ;                                                                                                          :variable #datascript.parser.Variable{:symbol ?e},
                       ;                                                                                                          :pattern #datascript.parser.Constant{:value [:db/id :reg/email :reg/name :reg/age :reg/birthdate
                       ;                                                                                                                                                       {:reg/gender [:db/ident]} {:reg/shirt-size [:db/ident]}]}}},
                       ;                                    :qwith nil,
                       ;                                    :qin [#datascript.parser.BindScalar{:variable #datascript.parser.SrcVar{:symbol $}}],
                       ;                                    :qwhere [#datascript.parser.Pattern{:source #datascript.parser.DefaultSrc{},
                       ;                                                                        :pattern [#datascript.parser.Variable{:symbol ?e}
                       ;                                                                                  #datascript.parser.Constant{:value :reg/email}
                       ;                                                                                  #datascript.parser.Constant{:value "dustin@example.com"}]}]},
                       :hypercrud.browser.field/cardinality :db.cardinality/one,
                       :hypercrud.browser.field/children
                       [{:hypercrud.browser.field/cardinality :db.cardinality/one,
                         :hypercrud.browser.field/children nil, :hypercrud.browser.field/data-has-id? false, :hypercrud.browser.field/label "id",
                         :hypercrud.browser.field/get-value :db/id, :hypercrud.browser.field/path-segment :db/id, :hypercrud.browser.field/source-symbol $}
                        {:hypercrud.browser.field/cardinality :db.cardinality/one,
                         :hypercrud.browser.field/children nil, :hypercrud.browser.field/data-has-id? false, :hypercrud.browser.field/label "email", :hypercrud.browser.field/get-value :reg/email, :hypercrud.browser.field/path-segment :reg/email, :hypercrud.browser.field/source-symbol $}
                        {:hypercrud.browser.field/cardinality :db.cardinality/one, :hypercrud.browser.field/children nil, :hypercrud.browser.field/data-has-id? false, :hypercrud.browser.field/label "name", :hypercrud.browser.field/get-value :reg/name, :hypercrud.browser.field/path-segment :reg/name, :hypercrud.browser.field/source-symbol $}
                        {:hypercrud.browser.field/cardinality :db.cardinality/one,
                         :hypercrud.browser.field/children nil, :hypercrud.browser.field/data-has-id? false, :hypercrud.browser.field/label "age", :hypercrud.browser.field/get-value :reg/age, :hypercrud.browser.field/path-segment :reg/age, :hypercrud.browser.field/source-symbol $}
                        {:hypercrud.browser.field/cardinality :db.cardinality/one,
                         :hypercrud.browser.field/children nil, :hypercrud.browser.field/data-has-id? false, :hypercrud.browser.field/label "birthdate", :hypercrud.browser.field/get-value :reg/birthdate, :hypercrud.browser.field/path-segment :reg/birthdate, :hypercrud.browser.field/source-symbol $}
                        {:hypercrud.browser.field/cardinality :db.cardinality/one, :hypercrud.browser.field/label "gender", :hypercrud.browser.field/get-value :reg/gender, :hypercrud.browser.field/path-segment :reg/gender, :hypercrud.browser.field/source-symbol $,
                         :hypercrud.browser.field/children [{:hypercrud.browser.field/cardinality :db.cardinality/one, :hypercrud.browser.field/children nil, :hypercrud.browser.field/data-has-id? false, :hypercrud.browser.field/label "ident", :hypercrud.browser.field/get-value :db/ident, :hypercrud.browser.field/path-segment :db/ident, :hypercrud.browser.field/source-symbol $}], :hypercrud.browser.field/data-has-id? true}
                        {:hypercrud.browser.field/cardinality :db.cardinality/one, :hypercrud.browser.field/label "shirt-size", :hypercrud.browser.field/get-value :reg/shirt-size, :hypercrud.browser.field/path-segment :reg/shirt-size, :hypercrud.browser.field/source-symbol $, :hypercrud.browser.field/children
                         [{:hypercrud.browser.field/cardinality :db.cardinality/one, :hypercrud.browser.field/children nil, :hypercrud.browser.field/data-has-id? false, :hypercrud.browser.field/label "ident", :hypercrud.browser.field/get-value :db/ident, :hypercrud.browser.field/path-segment :db/ident, :hypercrud.browser.field/source-symbol $}],
                         :hypercrud.browser.field/data-has-id? true}], :hypercrud.browser.field/data-has-id? true, :hypercrud.browser.field/get-value identity, :hypercrud.browser.field/label ?e, :hypercrud.browser.field/path-segment nil, :hypercrud.browser.field/source-symbol $}),
   :hypercrud.browser/data (r/track identity {:db/id 17592186046396,
                                              :reg/email "dustin@example.com",
                                              :reg/name "Dustin Getz",
                                              :reg/age 32,
                                              :reg/birthdate #inst "1985-09-21T00:05:00.000-00:00",
                                              :reg/gender {:db/ident :gender/male},
                                              :reg/shirt-size {:db/ident :shirt-size/mens-medium}})})
