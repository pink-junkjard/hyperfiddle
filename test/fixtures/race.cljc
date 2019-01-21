(ns fixtures.race
  (:require
    [contrib.reader]))

(def fiddle {:fiddle/ident :tutorial.race/submission
             :fiddle/type :query
             :fiddle/query
             (pr-str
               '[:find
                 [(pull ?e [:dustingetz.reg/email
                            :dustingetz.reg/name
                            ; :dustingetz.reg/age
                            ; :dustingetz.reg/birthdate
                            {:dustingetz.reg/gender [:db/ident]}
                            {:dustingetz.reg/shirt-size [:db/ident]}
                            :db/id])
                  ...]
                 :where
                 [?e :dustingetz.reg/email]])
             })

(def result
  '[{:db/id 17592186046196,
     :dustingetz.reg/email "dustin@example.com",
     :dustingetz.reg/name "Dustin Getz",
     :dustingetz.reg/gender {:db/ident :dustingetz.gender/male},
     :dustingetz.reg/shirt-size {:db/ident :dustingetz.shirt-size/mens-large}}
    {:db/id 17592186046763,
     :dustingetz.reg/email "bob@example.com",
     :dustingetz.reg/gender {:db/ident :dustingetz.gender/male},
     :dustingetz.reg/shirt-size {:db/ident :dustingetz.shirt-size/mens-large}}
    #_{:db/id 17592186046764, :dustingetz.reg/email "charlie@example.com", :dustingetz.reg/gender {:db/ident :dustingetz.gender/male}}
    #_{:db/id 17592186046317,
     :dustingetz.reg/email "alice@example.com",
     :dustingetz.reg/name "Alice",
     :dustingetz.reg/gender {:db/ident :dustingetz.gender/female},
     :dustingetz.reg/shirt-size {:db/ident :dustingetz.shirt-size/womens-medium}}
    #_{:db/id 17592186046765,
     :dustingetz.reg/email "elizabeth@example.com",
     :dustingetz.reg/gender {:db/ident :dustingetz.gender/female},
     :dustingetz.reg/shirt-size {:db/ident :dustingetz.shirt-size/womens-medium}}])

(def schema
  (contrib.datomic/indexed-schema
    '({:db/ident :attribute/ident, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "FK to schema, they can't be directly on $ schema because attribute renderers are a \"source code\" concern. TODO: move these off domain and into the fiddle repo."}
       {:db/ident :attribute/renderer, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Default attribute renderer, a CLJS var like `hyperfiddle.ui.controls/code`."}
       {:db/ident :community/category, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/many}, :db/fulltext true, :db/doc "All community categories"}
       {:db/ident :community/name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/fulltext true, :db/doc "A community's name"}
       {:db/ident :community/neighborhood, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "A community's neighborhood"}
       {:db/ident :community/orgtype, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "A community orgtype enum value"}
       {:db/ident :community/type, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "Community type enum values"}
       {:db/ident :community/url, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "A community's url"}
       {:db/ident :didier/team-name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :district/name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "A unique district name (upsertable)"}
       {:db/ident :district/region, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "A district region enum value"}
       {:db/ident :dustingetz/foo, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz/foo1, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz/next, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz/prev, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.gist/src-clojure, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.nav/children, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "Each nav item has optional children"}
       {:db/ident :dustingetz.post/draft-placeholder, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Show in rendered view, but disabled (no link)"}
       {:db/ident :dustingetz.post/hidden, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Omit from user-facing view but show in admin dashboards"}
       {:db/ident :dustingetz.post/published, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.post/published-date, :db/valueType {:db/ident :db.type/instant}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.post/related, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "This post links to these posts, e.g. a table of contents, or \"if you liked this you might also like\""}
       {:db/ident :dustingetz.post/slug, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "Human readable unique identifier for this post "}
       {:db/ident :dustingetz.post/sort-index, :db/valueType {:db/ident :db.type/long}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.post/title, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.reg/age, :db/valueType {:db/ident :db.type/long}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.reg/birthdate, :db/valueType {:db/ident :db.type/instant}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.reg/email, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}}
       {:db/ident :dustingetz.reg/gender, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.reg/name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.reg/shirt-size, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.slack-converter/raw-source, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.storm/channel, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.storm/ident, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}}
       {:db/ident :dustingetz.streaker/comment, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.streaker/date, :db/valueType {:db/ident :db.type/instant}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.streaker/diet, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.streaker/eating, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.streaker/exercise, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/many}}
       {:db/ident :dustingetz.streaker/food-log, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.streaker/gym, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Generally PIT"}
       {:db/ident :dustingetz.streaker/intention, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.streaker/miles-ran, :db/valueType {:db/ident :db.type/long}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Miles run, should be float."}
       {:db/ident :dustingetz.streaker/mood, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.streaker/pt, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "personal trainer"}
       {:db/ident :dustingetz.streaker/teatotal, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.streaker/weight, :db/valueType {:db/ident :db.type/long}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Should be decimal, but js/jvm coercion issues"}
       {:db/ident :dustingetz.streaker/weight-6am, :db/valueType {:db/ident :db.type/long}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.streaker/yoga, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.streaker.lifting/bench-1rm, :db/valueType {:db/ident :db.type/long}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "One rep max"}
       {:db/ident :dustingetz.streaker.lifting/squat-1rm, :db/valueType {:db/ident :db.type/long}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "One rep max"}
       {:db/ident :dustingetz.task/completed, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.task/parent, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :dustingetz.task/title, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :fiddle/cljs-ns, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "ClojureScript `user` namespace, available in :fiddle/renderer."}
       {:db/ident :fiddle/css, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Fiddle CSS. \n\nWarning: CSS is not scoped, please write targetted CSS"}
       {:db/ident :fiddle/hydrate-result-as-fiddle, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Experimental. When set, data-sync will interpret this fiddle's result as a fiddle - like a higher order fiddle - this is a recursion mechanic."}
       {:db/ident :fiddle/ident, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "Fiddle identifier used in URLs. Warning: changing this breaks fiddle URLs."}
       {:db/ident :fiddle/links, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/isComponent true, :db/doc "Links to other fiddles that are available from this fiddle"}
       {:db/ident :fiddle/markdown, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Markdown expression for fiddle view, optional"}
       {:db/ident :fiddle/pull, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Datomic pull expression for the entity addressed by the URL"}
       {:db/ident :fiddle/pull-database, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Argument to `datomic.api/pull`, defaults to $"}
       {:db/ident :fiddle/query, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Datomic query datalog. \n\nWarning: no support yet for rules, d/history, d/log or other datomic API access."}
       {:db/ident :fiddle/renderer, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Reagent expression for the fiddle view"}
       {:db/ident :fiddle/type, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Which Datomic query API"}
       {:db/ident :fiddle/uuid, :db/valueType {:db/ident :db.type/uuid}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "For naming anonymous fiddles"}
       {:db/ident :helloworld/email, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "hello world email "}
       {:db/ident :hyperfiddle/archived, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :hyperfiddle/deprecated, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :hyperfiddle/owners, :db/valueType {:db/ident :db.type/uuid}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "Entity owners uuids, used by ACLs"}
       {:db/ident :hyperfiddle/starred, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :link/class, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "semantic selector, like html css classes"}
       {:db/ident :link/fiddle, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "link target"}
       {:db/ident :link/formula, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "deprecated; function hook to influence target query inputs wiring, this is fully managed now"}
       {:db/ident :link/path, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "todo rename; specifies the attribute for which this link is valid"}
       {:db/ident :link/rel, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "archived"}
       {:db/ident :link/tx-fn, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "names a hyperfiddle.api/tx-fn multimethod which builds a transaction"}
       {:db/ident :neighborhood/district, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "A neighborhood's district"}
       {:db/ident :neighborhood/name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "A unique neighborhood name (upsertable)"}
       {:db/ident :person/name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}}
       {:db/ident :person/parents, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}}
       {:db/ident :putterson.todone/done, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :putterson.todone/title, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}})))

(def schemas {"$" schema})
