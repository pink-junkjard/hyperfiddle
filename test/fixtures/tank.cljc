(ns fixtures.tank
  (:require
    [contrib.reader]))


; These can be copy pasted from hyperfiddle gui

(def fiddles
   {:tutorial.race/submission
    [{:fiddle/ident :tutorial.race/submission
      :fiddle/type :query
      :fiddle/query
      (pr-str
         '[:find
           [(pull ?e [:dustingetz.reg/email
                      :dustingetz.reg/name
                      {:dustingetz.reg/gender [:db/ident]}
                      {:dustingetz.reg/shirt-size [:db/ident]}
                      :db/id])
            ...]
           :where
           [?e :dustingetz.reg/email]])}
     [{:db/id 17592186046196,
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
         :dustingetz.reg/shirt-size {:db/ident :dustingetz.shirt-size/womens-medium}}]]

    :hfnet.tank/index
    [{:db/id 17592186045830,
      :fiddle/css
      "table { width: 100%; }\n.p { max-width: 35em; }\n\nfigcaption.figure-caption { /* FIXME WHEN LANDS */\n  text-align: left;\n  font-size: 1.5em;\n}\n\nfigure img {\n  border: 1px solid lightgray;\n}\n\n.highlight { \n  background-color: #ffa; \n}",
      :fiddle/ident :hfnet.tank/index,
      :fiddle/markdown
      "!block{style=\"margin-bottom: 2em;\"}\n\n# Hyperfiddle Tank\n\nThis is a pastebin like environment for learning Hyperfiddle. \n\n1. Please namespace :idents with your username\n1. Schema changes are allowed\n1. To make a fiddle interactive, change !span[tank]{.i}.hyperfiddle.net to !span[demo]{.i}.hyperfiddle.net \n\n!block{style=\"margin-bottom: 2em;\"}\n\n### Showcase\n* [:clojurians](//demo.hyperfiddle.net/:clojurians/)\n* [:seattle/neighborhoods](//demo.hyperfiddle.net/:seattle!neighborhoods/)\n* [gender-shirtsize](//demo.hyperfiddle.net/:dustingetz!gender-shirtsize/)\n* [:tutorial.blog](//demo.hyperfiddle.net/:tutorial.blog/)\n* [markdown-pipeline](//demo.hyperfiddle.net/:dustingetz!markdown-pipeline/#:hf.src!markdown)\n\n!block{style=\"margin-bottom: 2em;\"}\n",
      :fiddle/query
      "[:find \n (pull ?e [:db/id :fiddle/ident])\n (max ?tx)\n ?entrypoint\n :where\n [?e :fiddle/ident]  \n [(hyperfiddle.query/entrypoint-fiddle? $ ?e) ?entrypoint]\n (not [?e :dustingetz.post/hidden true])\n [?e _ _ ?tx]]",
      :fiddle/renderer
      "(let [show-all (reagent.core/atom false)\n      needle (reagent.core/atom \"\")\n      regex? (reagent.core/atom false)]\n  (fn [val ctx props]\n    (let [[err re] (try [nil (re-pattern @needle)]\n                     (catch :default e\n                       [e #\"\"]))]\n      [:main.container props\n       [hyperfiddle.ui/markdown (-> ctx :hypercrud.browser/fiddle deref :fiddle/markdown) ctx]\n       [:h3 \"Recently modified fiddles\"]\n       #_[contrib.ui/easy-checkbox-boolean \"Show all fiddles? \" show-all]\n       [:div.form-group {:class (when (and @regex? err) \"has-error\")}\n        #_[:label.col-sm-2.control-label {:for \"hyperfiddle/entry-point-needle\"} \"Filter\"]\n        [:input.col-sm-10.form-control\n         {:id \"hyperfiddle/entry-point-needle\"\n          :style {:display \"inline-block\"} ; bootstrap styles are half imported, wonky hacks\n          :type \"text\"\n          :on-change #(reset! needle (.. % -target -value))\n          :auto-complete \"off\"\n          #_#_:auto-focus true  ; page sroll pos gets lost, otherwise this is great\n          :placeholder \"filter\"}]\n        #_[:span.col-sm-3\n           [contrib.ui/easy-checkbox-boolean \"Regex? \" regex?]]]\n       (when (and @regex? err) [:span (ex-message err)])\n\n       [:hr]\n\n       [:div.scroll-wrapper\n        [:table.table-hover\n         [:tbody\n          (->> @(:hypercrud.browser/data ctx)\n               #_((if @show-all identity (partial filter #(-> % :fiddle/ident namespace nil?))))\n               (filter (fn [[fiddle tx]]\n                         (let [s (str (:fiddle/ident fiddle))]\n                           (if @regex?\n                             (re-seq re s)\n                             (cuerdas.core/includes? s @needle)))))\n               (sort (fn [[a b] [a' b']]\n                       (if (= b b') \n                         (compare (clojure.string/replace (str (:fiddle/ident a)) #\"/\" \".\")\n                                  (clojure.string/replace (str (:fiddle/ident a')) #\"/\" \".\"))\n                         (> b b'))\n                       ))\n               (map (fn [[fiddle tx is-entrypoint]]\n                      [:tr {:key (str (hash (:fiddle/ident fiddle)))}\n                       [:td\n                        (if is-entrypoint\n                          [hyperfiddle.ui/anchor ctx {:route [(:fiddle/ident fiddle)]} (str (:fiddle/ident fiddle))]\n                          (str (:fiddle/ident fiddle)))\n                        ]]))\n               (doall))]]]])))",
      :fiddle/type :query,
      :hyperfiddle/owners
      [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"]}
     [[{:db/id 17592186045419, :fiddle/ident :karl.hardenstine/fiddles} 13194139534712 true]
      [{:db/id 17592186045426, :fiddle/ident :khardenstine/fiddles} 13194139534711 true]
      [{:db/id 17592186045455, :fiddle/ident :dustingetz/quick-element} 13194139534359 true]
      [{:db/id 17592186045473, :fiddle/ident :dustingetz/html5-video} 13194139535967 true]
      [{:db/id 17592186045481, :fiddle/ident :dustingetz/reagent-react-fragments} 13194139536287 true]
      [{:db/id 17592186045487, :fiddle/ident :khardenstine/neighborhoods} 13194139534711 true]
      [{:db/id 17592186045489, :fiddle/ident :tank.util/empty} 13194139535218 true]
      [{:db/id 17592186045500, :fiddle/ident :dustingetz/resultset-sorting} 13194139534412 true]
      [{:db/id 17592186045743, :fiddle/ident :seattle/neighborhoods} 13194139536407 true]
      [{:db/id 17592186045750, :fiddle/ident :seattle/districts} 13194139534662 true]
      [{:db/id 17592186045754, :fiddle/ident :seattle/neighborhood-new} 13194139534991 false]
      [{:db/id 17592186045758, :fiddle/ident :seattle/neighborhood-edit} 13194139534709 false]
      [{:db/id 17592186045762, :fiddle/ident :seattle/district} 13194139534667 false]
      [{:db/id 17592186045818, :fiddle/ident :dustingetz/reagent-svg-demo} 13194139534717 true]
      [{:db/id 17592186045823, :fiddle/ident :dustingetz/clojure-tree-seq} 13194139534735 true]
      [{:db/id 17592186045830, :fiddle/ident :hfnet.tank/index} 13194139536342 true]
      [{:db/id 17592186045848, :fiddle/ident :didier/helloworld} 13194139535447 false]
      [{:db/id 17592186046189, :fiddle/ident :didier/comments} 13194139535208 true]
      [{:db/id 17592186046195, :fiddle/ident :dustingetz/gender-shirtsize} 13194139536348 true]
      [{:db/id 17592186046198, :fiddle/ident :tutorial.race/submission} 13194139536346 false]
      [{:db/id 17592186046200, :fiddle/ident :tutorial.race/genders} 13194139535963 true]
      [{:db/id 17592186046202, :fiddle/ident :tutorial.race/shirt-sizes} 13194139535964 false]
      [{:db/id 17592186046215, :fiddle/ident :tank.util/schema} 13194139535458 true]
      [{:db/id 17592186046221, :fiddle/ident :tutorial.blog} 13194139536349 true]
      [{:db/id 17592186046223, :fiddle/ident :tutorial.blog/index-new-post} 13194139536264 false]
      [{:db/id 17592186046230, :fiddle/ident :tutorial.blog/post-edit} 13194139535127 false]
      [{:db/id 17592186046233, :fiddle/ident :tutorial.blog/post-view} 13194139535965 false]
      [{:db/id 17592186046327, :fiddle/ident :dustingetz/slack-storm} 13194139536337 true]
      [{:db/id 17592186046579, :fiddle/ident :dustingetz/image-gallery-css-only} 13194139535486 true]
      [{:db/id 17592186046596, :fiddle/ident :tacosundae/ctx} 13194139535493 true]
      [{:db/id 17592186046599, :fiddle/ident :dustingetz.forum/index} 13194139535585 true]
      [{:db/id 17592186046603, :fiddle/ident :dustingetz.boards/nav} 13194139535496 true]
      [{:db/id 17592186046606, :fiddle/ident :clojurians} 13194139536353 true]
      [{:db/id 17592186046609, :fiddle/ident :clojurians/channel} 13194139536357 false]
      [{:db/id 17592186046611, :fiddle/ident :clojurians/channel-days} 13194139536313 false]
      [{:db/id 17592186046617, :fiddle/ident :clojurians/channel-messages} 13194139535886 false]
      [{:db/id 17592186046633, :fiddle/ident :clojurians/user-messages} 13194139536293 false]
      [{:db/id 17592186046640, :fiddle/ident :clojurians/user-profile} 13194139536314 false]
      [{:db/id 17592186046649, :fiddle/ident :clojurians/users} 13194139536352 true]
      [{:db/id 17592186046661, :fiddle/ident :clojurians/max-channel-message-count} 13194139535559 true]
      [{:db/id 17592186046679, :fiddle/ident :mbrainz} 13194139535580 true]
      [{:db/id 17592186046693, :fiddle/ident :dustingetz/www.hyperfiddle-consulting.com} 13194139536409 true]
      [{:db/id 17592186046699, :fiddle/ident :dustingetz/social-icons-svg-bootstrap} 13194139535599 true]
      [{:db/id 17592186046706, :fiddle/ident :dustingetz/html5-video-m3u8} 13194139535617 true]
      [{:db/id 17592186046717, :fiddle/ident :dustingetz/merge-with-into} 13194139535616 true]
      [{:db/id 17592186046724, :fiddle/ident :cookbook/data-api} 13194139535619 true]
      [{:db/id 17592186046728, :fiddle/ident :cookbook/form-fields} 13194139535623 true]
      [{:db/id 17592186046729, :fiddle/ident :cookbook/form-recursion} 13194139535623 true]
      [{:db/id 17592186046730, :fiddle/ident :cookbook/form-value-renderers} 13194139535623 true]
      [{:db/id 17592186046731, :fiddle/ident :cookbook/form-value-renderers-markdown} 13194139535623 true]
      [{:db/id 17592186046733, :fiddle/ident :cookbook/nested-table} 13194139535892 true]
      [{:db/id 17592186046734, :fiddle/ident :cookbook/markdown-lists} 13194139535623 true]
      [{:db/id 17592186046736, :fiddle/ident :cookbook/table-with-boostrap-renderer} 13194139535623 true]
      [{:db/id 17592186046739, :fiddle/ident :cookbook/fiddle-renderer} 13194139535634 true]
      [{:db/id 17592186046741, :fiddle/ident :cookbook/markdown-table} 13194139535639 true]
      [{:db/id 17592186046747, :fiddle/ident :cookbook/markdown-form-body-renderers} 13194139535639 true]
      [{:db/id 17592186046749, :fiddle/ident :cookbook/markdown-list-value-renderers} 13194139535926 true]
      [{:db/id 17592186046750, :fiddle/ident :cookbook/markdown-basics} 13194139535654 true]
      [{:db/id 17592186046752, :fiddle/ident :cookbook/markdown-figure} 13194139535647 true]
      [{:db/id 17592186046753, :fiddle/ident :cookbook/markdown-functions} 13194139535647 true]
      [{:db/id 17592186046754, :fiddle/ident :cookbook/reagent-bootstrap-table} 13194139535882 true]
      [{:db/id 17592186046755, :fiddle/ident :cookbook/codemirror-integrations} 13194139535647 true]
      [{:db/id 17592186046757, :fiddle/ident :cookbook/markdown-table-of-contents} 13194139535652 true]
      [{:db/id 17592186046767, :fiddle/ident :karatatar/demo} 13194139535664 true]
      [{:db/id 17592186046770, :fiddle/ident :karatatar/note-title} 13194139535665 true]
      [{:db/id 17592186046774, :fiddle/ident :dustingetz/java-magazine-clojure-growth-2018} 13194139535689 true]
      [{:db/id 17592186046797, :fiddle/ident :dustingetz/niko-gobel-differential-dataflow} 13194139536399 true]
      [{:db/id 17592186046811, :fiddle/ident :dustingetz/youtube-embed} 13194139535748 true]
      [{:db/id 17592186046826, :fiddle/ident :dustingetz/slack-datomic-cloud-testing} 13194139535721 true]
      [{:db/id 17592186046828, :fiddle/ident :alexandrkozyrev/seattle} 13194139536255 true]
      [{:db/id 17592186046834, :fiddle/ident :alexandrkozyrev/neighborhood} 13194139535729 true]
      [{:db/id 17592186046836, :fiddle/ident :alexandrkozyrev.seattle/neighborhood} 13194139535733 false]
      [{:db/id 17592186046839, :fiddle/ident :alexandrkozyrev.seattle/region} 13194139535736 false]
      [{:db/id 17592186046842, :fiddle/ident :alexandrkozyrev.seattle/districts} 13194139535744 true]
      [{:db/id 17592186046873, :fiddle/ident :dustingetz/foreign-http-request} 13194139535783 true]
      [{:db/id 17592186046889, :fiddle/ident :dustingetz/counter} 13194139536563 true]
      [{:db/id 17592186046992, :fiddle/ident :dustingetz/notepad} 13194139536277 true]
      [{:db/id 17592186046998, :fiddle/ident :paulrd:seattle} 13194139535893 true]
      [{:db/id 17592186047002, :fiddle/ident :dustingetz.storm/view} 13194139536557 false]
      [{:db/id 17592186047020, :fiddle/ident :dustingetz/markdown-extensions} 13194139536055 true]
      [{:db/id 17592186047073, :fiddle/ident :dustingetz/vega} 13194139535979 true]
      [{:db/id 17592186047089, :fiddle/ident :dustingetz.storm/new-storm} 13194139536555 false]
      [{:db/id 17592186047152, :fiddle/ident :dustingetz/datalog-grounded-value} 13194139536047 true]
      [{:db/id 17592186047158, :fiddle/ident :dustingetz/markdown-pipeline} 13194139536096 true]
      [{:db/id 17592186047169, :fiddle/ident :dustingetz/markdown-codemirror} 13194139536092 true]
      [{:db/id 17592186047204, :fiddle/ident :dustingetz/streaker} 13194139536549 true]
      [{:db/id 17592186047207, :fiddle/ident :dustingetz.streaker/entry} 13194139536452 false]
      [{:db/id 17592186047224, :fiddle/ident :dustingetz.streaker/moods} 13194139536128 true]
      [{:db/id 17592186047290, :fiddle/ident :dustingetz/dagre-d3} 13194139536201 true]
      [{:db/id 17592186047293, :fiddle/ident :dustingetz/jointjs} 13194139536202 true]
      [{:db/id 17592186047308, :fiddle/ident :dustingetz/loader} 13194139536300 true]
      [{:db/id 17592186047314, :fiddle/ident :viebel/defrecord} 13194139536209 true]
      [{:db/id 17592186047315, :fiddle/ident :viebel/clojure-defrecord} 13194139536209 true]
      [{:db/id 17592186047327, :fiddle/ident :didier/clojure-conj} 13194139536232 true]
      [{:db/id 17592186047329, :fiddle/ident :didier/new-team} 13194139536232 false]
      [{:db/id 17592186047332, :fiddle/ident :dustingetz/conj-talk} 13194139536249 true]
      [{:db/id 17592186047348, :fiddle/ident :dustingetz/seattle1} 13194139536243 true]
      [{:db/id 17592186047369, :fiddle/ident :dustingetz.tutorial/blog} 13194139536338 true]
      [{:db/id 17592186047371, :fiddle/ident :dustingetz.tutorial/view-post} 13194139536264 false]
      [{:db/id 17592186047373, :fiddle/ident :dustingetz.tutorial.blog/new-post} 13194139536264 false]
      [{:db/id 17592186047392, :fiddle/ident :dustingetz/naked-codemirror} 13194139536289 true]
      [{:db/id 17592186047395, :fiddle/ident :dustingetz/cljs-eval} 13194139536299 true]
      [{:db/id 17592186047426, :fiddle/ident :dustingetz/clojure-autoformat} 13194139536326 false]
      [{:db/id 17592186047455, :fiddle/ident :d4hines/helloworld} 13194139536463 true]
      [{:db/id 17592186047467, :fiddle/ident :astrocaribe.tutorial/blog} 13194139536369 true]
      [{:db/id 17592186047502, :fiddle/ident :dustingetz/rubber-ducky} 13194139536397 true]
      [{:db/id 17592186047515, :fiddle/ident :dustingetz/sql-join} 13194139536424 true]
      [{:db/id 17592186047518, :fiddle/ident :dustingetz/person-new} 13194139536412 false]
      [{:db/id 17592186047530, :fiddle/ident :dustingetz/parents} 13194139536424 false]
      [{:db/id 17592186047541, :fiddle/ident :dustingetz.datomic-vs-sql/recursion} 13194139536438 true]
      [{:db/id 17592186047544, :fiddle/ident :dustingetz.datomic-vs-sql/cartesian} 13194139536441 true]
      [{:db/id 17592186047548, :fiddle/ident :dustingetz/fiddle-set-arg} 13194139536443 false]
      [{:db/id 17592186047565, :fiddle/ident :d4hines/helloworld.view-post} 13194139536489 false]
      [{:db/id 17592186047570, :fiddle/ident :putterson/todone} 13194139536479 true]
      [{:db/id 17592186047573, :fiddle/ident :putterson/view-todone} 13194139536487 false]
      [{:db/id 17592186047585, :fiddle/ident :putterson/new-todone} 13194139536480 false]
      [{:db/id 17592186047602, :fiddle/ident :bdevel/blog} 13194139536502 true]
      [{:db/id 17592186047604, :fiddle/ident :bdevel/post-show} 13194139536499 false]
      [{:db/id 17592186047615, :fiddle/ident :dustingetz/datomic-pull-tx} 13194139536512 true]
      [{:db/id 17592186047618, :fiddle/ident :fsbl0/app} 13194139536533 true]
      [{:db/id 17592186047663, :fiddle/ident :dustingetz/datomic-find-specs} 13194139536562 true]]]
    }
   )


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
