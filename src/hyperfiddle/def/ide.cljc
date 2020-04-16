(ns hyperfiddle.def.ide
  (:require
    [clojure.set :as set]
    [contrib.etc :refer [with-ns]]
    [hyperfiddle.etc.def :as hf-def]
    [hyperfiddle.api :as hf]))

(declare ctx props args via fiddle-args)

(hf-def/fiddle :hyperfiddle.ide/project
  :pull "$" [:db/id
             :project/code
             :project/css]
  :renderer
  (let [{:keys [:hypercrud.browser/fiddle]} ctx]
    [:div props
     [hyperfiddle.ui/field [:project/code] ctx hyperfiddle.ui/hyper-control]
     [hyperfiddle.ui/field [:project/css] ctx hyperfiddle.ui/hyper-control]]))

(hf-def/fiddle :hyperfiddle/ide
  :pull [:db/id
         :fiddle/css
         :fiddle/ident
         {:fiddle/links [:db/id
                         :link/class
                         {:link/fiddle [:db/id
                                        :fiddle/ident       ; routing
                                        :fiddle/query       ; validation
                                        :fiddle/type        ; validation
                                        ]}
                         :link/formula
                         :link/path
                         :link/rel
                         :link/tx-fn]}
         :fiddle/markdown
         :fiddle/pull
         :fiddle/pull-database
         :fiddle/query
         :fiddle/cljs-ns
         :fiddle/renderer
         :fiddle/type
         :fiddle/hydrate-result-as-fiddle
         *                                                  ; For hyperblog, so we can access :hyperblog.post/title etc from the fiddle renderer
         ]

  :link [":fiddle/ident" [:hf/remove]]
  :link [":fiddle/links" [:hf/remove]]
  :link [":fiddle/links" [:hf/new] :hyperfiddle.ide/new-link]
  :link [":hyperfiddle/ide" [:hf/iframe] :hyperfiddle.ide/fiddle-options]
  :link [":link/fiddle" [:hf/new] :hyperfiddle.ide/new-fiddle
         :tx-fn ":hyperfiddle.ide.fiddles.fiddle-src/new-fiddle"]

  :renderer hyperfiddle.ide.fiddles.fiddle-src/fiddle-src-renderer

  :css "
    table.hyperfiddle.-fiddle-links { table-layout: fixed; }
    table.-fiddle-links th.-hypercrud-browser-path--fiddle-links { width: 60px; }

    table.-fiddle-links td.-hypercrud-browser-path--fiddle-links--link-fiddle { display: flex; }
    table.hyperfiddle.-fiddle-links td.field.-link-fiddle > select { flex: 0 1 80% !important; } /* line up :new */

    .fiddle-editor > .nav > li > a {
      padding: 3px 6px;
    }
  ")

(hf-def/fiddle :hyperfiddle.ide/new-link
  :pull [:link/path :db/id]

  :renderer
  (let [{:keys [:hypercrud.browser/fiddle]} ctx]
    [:div.container-fluid props
     [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]])

  :markdown "
    #### new-link

    !field(:link/path){placeholder=\":district/region\"}
  "

  :css "
    .-hyperfiddle-ide-new-link .field { display: flex; }
    .-hyperfiddle-ide-new-link div.field > :first-child {
      flex: 0 1 7em !important; display: inline; padding-right: 1em; text-align: right; }
    .-hyperfiddle-ide-new-link div.field > :nth-child(2) { flex: 1 1; }
  ")

(hf-def/fiddle :hyperfiddle.ide/fiddle-options
  :query '[:find (pull ?link [:db/id :fiddle/ident])
           :where
           (or [?link :fiddle/ident]
             [?link :fiddle/type])]

  :link [":hyperfiddle.ide/fiddle-options" [:hf/remove]])

(hf-def/fiddle :hyperfiddle.ide/new-fiddle

  :pull [:db/id :fiddle/ident]

  :renderer [:div.container-fluid props
             [:h4 "New fiddle"]
             [hyperfiddle.ui/field [:fiddle/ident] ctx hyperfiddle.ui/hyper-control
              {:placeholder ":username/hello-world"}]]

  :css "
    .-hyperfiddle-ide-new-fiddle > .field { display: flex; }
    .-hyperfiddle-ide-new-fiddle div.field > :first-child {
      flex: 0 1 7em !important; display: inline; padding-right: 1em; text-align: right; }
    .-hyperfiddle-ide-new-fiddle div.field > :nth-child(2) { flex: 1 1; }
  ")

(hf-def/fiddle :hyperfiddle.ide/home
  :link [":hyperfiddle.ide/home" [:hf/iframe] :hyperfiddle/topnav]
  :link [":hyperfiddle.ide/home" [:hf/iframe] :hyperfiddle.ide/fiddle-index]

  :markdown "# :hyperfiddle.ide/home"

  :renderer
  [:<>
   [hyperfiddle.ui/link :hyperfiddle/topnav ctx]
   [:main.container-fluid props
    [hyperfiddle.ui/markdown
     (-> ctx :hypercrud.browser/fiddle deref :fiddle/markdown) ctx]
    [hyperfiddle.ui/link :hyperfiddle.ide/fiddle-index ctx]
    #_[user.domain/filter-controls user/needle]
    #_(let [ctx (assoc ctx :hyperfiddle.ide.home/needle user/needle)]
        [:div.row
         [:div.col-sm-6
          [hyperfiddle.ui/link :dustingetz/fiddle-index-recent ctx]]
         [:div.col-sm-6
          [hyperfiddle.ui/link :dustingetz/fiddle-index-owned ctx]]])]
   #_[hyperfiddle.ide/ide-stage ctx]]

  :code
  (def needle (reagent.core/atom ""))

  (defn filter-controls [needle is-regex]
    [:div.form-group {:class (when (and @is-regex err) "has-error")}
     [:input.form-control
      {:style         {:display "inline-block"}            ; bootstrap styles are half imported, wonky hacks
       :type          "text"
       :on-change     #(reset! needle (.. % -target -value))
       :auto-complete "off"
       #_#_:auto-focus true                                ; page sroll pos gets lost, otherwise this is great
       :placeholder   "filter"}]
     #_[:span.col-sm-3 [contrib.ui/easy-checkbox-boolean "regex " is-regex]]
     #_(when (and @is-regex err) [:span (ex-message err)])])

  :css "
    main {
      flex: 1 1;
      overflow: auto;
    }
  ")

(hf-def/fiddle :hyperfiddle/topnav
  :link [":hyperfiddle/topnav" :hyperfiddle.ide/account :tx-fn ":zero"]
  :link [":hyperfiddle/topnav" [:hf/iframe] :hyperfiddle.ide/topnav-new]
  :link [":hyperfiddle/topnav" [:hf/iframe] :hyperfiddle.ide/env]
  :link [":hyperfiddle/topnav" :hyperfiddle.ide/fiddle-index]

  :renderer
  (hyperfiddle.ide.fiddles.topnav/renderer val ctx props)

  :css "
    .hyperfiddle-user.-hyperfiddle-topnav.user { padding: 0; }
    .-hyperfiddle-topnav .-account { display: inline-block; }

    .-hyperfiddle-topnav button,
    .-hyperfiddle-topnav a { padding: 0 2px; }

    .-hyperfiddle-topnav .-hyperfiddle-ide-entry-point-fiddles {
      max-height: 50vh;
      overflow-y: auto;
    }
  ")

(hf-def/fiddle :hyperfiddle.ide/fiddle-index
  :query '[:find
           (pull ?e [:hyperfiddle/starred
                     :fiddle/ident])
           (max ?tx)
           ?entrypoint
           :where
           [(ground true) ?entrypoint]
           [?e :fiddle/ident ?ident]
           [?e _ _ ?tx]]

  :markdown "### Recently modified fiddles"

  :renderer
  [:<>
   [:div.form-group
    [hyperfiddle.ui/needle-input '[[(str ?ident) ?si]
                                   [(.contains ?si %)]] ctx
     {:style         {:display "inline-block"}              ; bootstrap styles are half imported, wonky hacks
      :class         "form-control"
      :type          "text"
      :on-change     #(reset! needle (.. % -target -value))
      :auto-complete "off"
      #_#_:auto-focus true                                  ; page sroll pos gets lost, otherwise this is great
      :placeholder   "filter"}]]
   [:div.row
    [:div.col-sm-12
     (let [{:keys [:hypercrud.browser/fiddle]} ctx]
       [:div props
        [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]
        [js/user.domain.fiddle-list ctx]])]]]

  :code
  (def fiddle-index-needle (reagent.core/atom "")))

(hf-def/fiddle :hyperfiddle.ide/env
  :link [":hyperfiddle.ide/env" [:hf/iframe] :hyperfiddle/topnav]
  :link [":hyperfiddle.ide/env" [:hf/iframe] :hyperfiddle.ide/domain
         :formula "(constantly #entity[\"$domains\" [:domain/ident (:app-domain-ident (hyperfiddle.runtime/domain (:runtime ctx)))]])"]
  :link [":hyperfiddle.ide/env" [:hf/iframe] :hyperfiddle.ide/project
         :formula "(constantly #entity [\"$\" :hyperfiddle/project])"]
  :link [":hyperfiddle.ide/env" [:hf/iframe] :hyperfiddle.ide/attribute-renderers]

  :renderer
  [:<>
   [hyperfiddle.ui/ui-from-link
    (hyperfiddle.data/select ctx :hyperfiddle/topnav) ctx {}]

   [:div.container-fluid.hyperfiddle-ide-env-content
    (if (= "nodejs" *target*)
      (hyperfiddle.ui.loading/page (hyperfiddle.runtime/domain (:runtime ctx)))
      ; this content chunk takes too long to ssr
      [:<>
       [:h3 "Environment"]
       (if (-> (hyperfiddle.runtime/domain (:runtime ctx))
               (hyperfiddle.domain/database "$domains"))
         ; $domains exists only with directory service
         [hyperfiddle.ui/link :hyperfiddle.ide/domain ctx {}]
         (-> (hyperfiddle.runtime/domain (:runtime ctx))
             :hyperfiddle.ide.domain/user-domain+
             (cats.monad.either/branch
               (fn [e] [:pre (js/pprint-str e)])
               (fn [domain]
                 (->> (for [[label f] [#_:domain/ident
                                       #_:hyperfiddle/owners
                                       ["databases" hyperfiddle.domain/databases]
                                       ["fiddle-dbname" hyperfiddle.domain/fiddle-dbname]
                                       #_:domain/aliases
                                       #_:domain/disable-javascript
                                       #_:domain/home-route
                                       ["environment" hyperfiddle.domain/environment]]]
                        [:<> [:dt label] [:dd (contrib.pprint/pprint-str (f domain))]])
                      (into [:dl]))))))
       [:h5 "Project"]
       [hyperfiddle.ui/link :hyperfiddle.ide/project ctx {}]
       (when (exists? js/show_renderers)
         [:<>
          [:h5 "Attribute renderers"]
          [hyperfiddle.ui/link :hyperfiddle.ide/attribute-renderers ctx {}]])
       ])
    ]

   [hyperfiddle.ide/ide-stage ctx]]

  :css "
    .hyperfiddle-ide-env-content {
      flex: 1 1;
      overflow: auto;
    }
  ")

(hf-def/fiddle :hyperfiddle.ide/topnav-new
  :pull [:fiddle/ident]
  :link [":fiddle/ident" :hyperfiddle.ide/new-fiddle [:hf/new]
         :tx-fn :hyperfiddle.ide.fiddles.topnav/new-fiddle-tx])

(hf-def/fiddle :database/options-list

  :query '[:find (pull $domains ?e [:db/id :database/uri]) ?name
           :in $domains
           :where
           [$domains ?e :database/uri ?uri]
           [(str ?uri) ?suri]
           [(.substring ?suri 28) ?name]])

(hf-def/fiddle :databases/edit
  :pull
  "$domains"
  [:db/id
   :database/uri
   :hyperfiddle/owners
   {:database/write-security [:db/ident]}
   :database.custom-security/client
   :database.custom-security/server
   {:domain.database/_record [:domain.database/name {:domain/_databases [:domain/ident]}]}
   {:domain/_fiddle-database [:domain/ident]}
   :db/doc]

  :link [":database/write-security" [:hf/iframe] :databases/write-security-options]
  :link [":databases/edit" [:hf/remove]]

  :renderer
  (let [custom-sec? (= :hyperfiddle.security/custom @(contrib.reactive/cursor (:hypercrud.browser/data ctx) [:database/write-security :db/ident]))]
    [:div props
     #_[hyperfiddle.ui/markdown @(contrib.reactive/fmap :fiddle/markdown (:hypercrud.browser/fiddle ctx)) ctx]
     [:h2 (-> @(contrib.reactive/fmap (contrib.reactive/comp str :database/uri) (:hypercrud.browser/data ctx))
              (subs (count "datomic:free://datomic:4334/")))]
     [hyperfiddle.ui/field [:database/uri] ctx hyperfiddle.ui/hyper-control]
     [hyperfiddle.ui/field [:hyperfiddle/owners] ctx hyperfiddle.ui/hyper-control]
     [hyperfiddle.ui/field [:database/write-security] ctx hyperfiddle.ui/hyper-control
      {:options :databases/write-security-options}]
     (when custom-sec?
       [hyperfiddle.ui/field [:database.custom-security/client] ctx hyperfiddle.ui/hyper-control])
     (when custom-sec?
       [hyperfiddle.ui/field [:database.custom-security/server] ctx hyperfiddle.ui/hyper-control])
     (hyperfiddle.ui/field [:db/doc] ctx hyperfiddle.ui/hyper-control)
     #_(hyperfiddle.ui/field [] ctx (fn [val ctx props]
                                      (hyperfiddle.ui/link :hf/remove ctx)))
     [:h3 "Domains"]
     [hyperfiddle.ui/field [:domain.database/_record] ctx
      (fn [val ctx props]
        (->> val
             (map (comp :domain/ident :domain/_databases))
             sort
             (map (fn [v]
                    [:li [hyperfiddle.ui/anchor ctx {:route [:domains/edit [v]]} v]]))
             (into [:ul])))]
     [hyperfiddle.ui/field [:domain/_fiddle-database] ctx
      (fn [val ctx props]
        (->> val
             (map :domain/ident)
             sort
             (map (fn [v]
                    [:li [hyperfiddle.ui/anchor ctx {:route [:domains/edit [v]]} v]]))
             (into [:ul])))]]))

(hf-def/fiddle :databases/write-security-options
  :query '[:find (pull $domains ?e [:db/ident])
           :in $domains
           :where
           [$domains ?e :db/ident ?ident]
           [(namespace ?ident) ?ns]
           [(= ?ns "hyperfiddle.security")]])

(hf-def/fiddle :domain.databases/add
  :pull ["$domains"
         :db/id
         :domain.database/name
         {:domain.database/record
          [:database/uri]}]

  :link [":domain.databases/add" [:databases :hf/iframe] :database/options-list]

  :markdown "#### add database to domain"

  :renderer
  (let [{:keys [:hypercrud.browser/fiddle]} ctx]
    [:div props
     [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]
     #_[hyperfiddle.ui/field [:domain.database/name] ctx hyperfiddle.ui/hyper-control]
     [hyperfiddle.ui/field [:domain.database/record] ctx hyperfiddle.ui/hyper-control {:options      "databases"
                                                                                       :option-label (comp :database/uri first)}]]))

(hf-def/fiddle :hyperfiddle.ide/account
  :query
  '[:in $users
    :find
    (pull $users ?user
      [:db/id
       :user/name
       :user/email
       :user/last-seen
       :user/sub
       :user/picture
       :user/user-id
       #_:hyperfiddle.ide/parinfer
       *])
    .
    :where
    [(ground hyperfiddle.io.bindings/*subject*) ?user-id]
    [$users ?user :user/user-id ?user-id]]

  :renderer
  (let [user @(:hypercrud.browser/result ctx)]
    [:div.container-fluid props
     [:div.p
      [hyperfiddle.ui/img (:user/picture user) ctx {:class "avatar"}]]

     [:h3 "Hello, " (:user/name user) "!"]

     [:div.p
      [:form {:action "/logout" :method "post"}
       (str "Last login was " (or (some-> (:user/last-seen user) .toLocaleDateString) "–") ".")

       [:button.btn.btn-link {:type  "submit"
                              :style {:display        "inline"
                                      :padding        "0"
                                      :margin-left    ".25em"
                                      :font-size      "inherit"
                                      :vertical-align "inherit"}}
        "logout"]]
      ]

     [:div.p [:div {:style {:margin-bottom "1em"}}]]

     #_[:div.p "Feature flags"]
     #_(->> (-> ctx :hypercrud.browser/field deref :hypercrud.browser.field/children)
            (map (fn [{a :hypercrud.browser.field/path-segment}]
                   (when (and (keyword? a) (= (namespace a) "hyperfiddle.ide"))
                     ^{:key (str [a])}
                     [hyperfiddle.ui/field [a] ctx])))
            doall)
     #_[hyperfiddle.ui/field ['*] ctx]
     #_[hyperfiddle.ui/link :hyperfiddle.ide.user/domains ctx]

     #_[:div (pr-str (:user/user-id user))]
     [hyperfiddle.ui/field [:user/user-id] ctx]])

  :code
  (defmethod hyperfiddle.api/render #{:user/user-id
                                      :hyperfiddle.ide/account}
    [ctx props]
    [:div.hyperfiddle-input-group
     [:div.input
      (pr-str (hypercrud.browser.context/data ctx))]])

  :css "
    img.avatar { border: 1px solid lightgray; border-radius: 50%; width: 80px; }
    img.avatar { float: left; margin-top: 1rem; margin-right: 1rem; }
    .-hyperfiddle-ide-user-settings h3 { margin-top: 1rem; }
   ")

(hf-def/fiddle :hyperfiddle.ide.user/domains
  :query '[:find [(pull $domains ?domain [:domain/ident :db/id]) ...]
           :in $domains
           :where
           [(ground hyperfiddle.io.bindings/*subject*) ?user-id]
           [$domains ?domain :domain/ident]
           [$domains ?domain :hyperfiddle/owners ?user-id]]

  :markdown "Domains"

  :renderer
  (let [{:keys [:hypercrud.browser/fiddle]} ctx]
    [:div props
     [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]
     [:ul
      (->> (hypercrud.browser.context/data ctx)
           (sort-by :domain/ident)
           (map (fn [domain]
                  [:li {:key (hash (:db/id domain))}
                   [:a
                    {:href (str "http://" (:domain/ident domain) "." (:ide-domain (hyperfiddle.runtime/domain (:runtime ctx))) "/")}
                    (:domain/ident domain)]]))
           (doall))]]))

(hf-def/fiddle :hyperfiddle.ide/attribute-edit
  :pull [:db/id :attribute/ident :attribute/renderer]

  :renderer
  (let [{:keys [:hypercrud.browser/fiddle]} ctx]
    [:div props
     [hyperfiddle.ui/field [:attribute/ident] ctx hyperfiddle.ui/hyper-control]]))

(hf-def/fiddle :hyperfiddle.ide/attribute-renderers
  :query '[:find
           (pull ?e [:attribute/ident
                     :attribute/renderer])
           :where [?e :attribute/ident]]

  :link [":attribute/ident" [:hf/remove]]
  :link [":hyperfiddle.ide/domain-attribute-renderers" [:hf/new] :hyperfiddle.ide/attribute-edit]

  :renderer
  (if (= "nodejs" *target*)
    (hyperfiddle.ui.loading/page (hyperfiddle.runtime/domain (:runtime ctx)))
    ; this table is awful on ssr
    [:div props
     [hyperfiddle.ui/result val ctx {}]]
    ))

(hf-def/fiddle :hyperfiddle.ide/domain
  :pull "$domains"
  [#_:db/id
   :domain/ident
   :hyperfiddle/owners
   {:domain/databases
    [:db/id
     :domain.database/name
     {:domain.database/record
      [#_:db/id
       :database/uri]}]}
   {:domain/fiddle-database
    [#_:db/id
     :database/uri]}
   :domain/aliases
   :domain/disable-javascript
   :domain/home-route
   :domain/environment]

  :link ["$domains :domain/databases" [:hf/remove]]
  :link ["$domains :domain/databases" [:hf/new] :domain.databases/add]
  :link [":hyperfiddle.ide/domain" [:hf/remove]]
  :link [":hyperfiddle.ide/domain" [:hf/iframe] :database/options-list]

  :code
  (defmethod hyperfiddle.api/render #{:domain/ident
                                      :hyperfiddle.ide/domain}
    [ctx props]
    [:div
     [:a (assoc props :href "/")
      (hypercrud.browser.context/v ctx)]])

  ; Information leak
  #_(defmethod hyperfiddle.api/render #{:domain/fiddle-database
                                        :hyperfiddle.ide/domain}
      [ctx props]
      [hyperfiddle.ui.select$/select nil ctx
       {:options      :database/options-list
        :option-label (comp str :database/uri)}])

  :renderer
  (if (= "nodejs" *target*)
    (hyperfiddle.ui.loading/page (hyperfiddle.runtime/domain (:runtime ctx)))
    ; slow to ssr
    [:div props
     [hyperfiddle.ui/result val ctx props]])

  :css "
    div.hyperfiddle.ui div.hyperfiddle.field.-domain-router,
    div.hyperfiddle.ui div.hyperfiddle.field.-domain-environment,
    div.hyperfiddle.ui div.hyperfiddle.field.-domain-code,
    div.hyperfiddle.ui div.hyperfiddle.field.-domain-css
    { display: block; }

    .hyperfiddle-ide table.hyperfiddle { table-layout: fixed; }
    .hyperfiddle-ide th.-db-id { width: 60px; }
    .hyperfiddle-ide th.-domain-database-name { width: 200px; }
    .hyperfiddle-ide td.-db-id button { padding: 0px; }
    .hyperfiddle-ide td.-database-uri > div { padding: 4px 5px; }
  ")

(hf-def/fiddle :hyperfiddle.ide/edit
  :link [":hyperfiddle.ide/edit" [:hf/iframe] :hyperfiddle/topnav]
  :link [":hyperfiddle.ide/edit" [:hf/iframe] :hyperfiddle/ide
         :formula "(constantly (-> @(:hypercrud.browser/route ctx) :hyperfiddle.route/datomic-args first))"]

  :renderer hyperfiddle.ide.edit/view

  :css "
    .-hyperfiddle-ide-edit {
      flex: 1 1;
      overflow-y: auto;
    }

    @media (min-width: 576px) {
      .-hyperfiddle-ide-edit {
        display: flex;
      }
      .-hyperfiddle-ide-edit > div {
        flex: 1 1 50%;
      }
    }

    .fiddle-editor-col {
      display: flex;
      flex-direction: column;
      /*min-height: calc(100vh - 22px);*/
    }

    .fiddle-editor-header {
      padding: .1em .5em;
      background-color: #d7ebff;
    }

    .fiddle-editor-header > label {
      float: right;
    }

    .fiddle-editor {
      flex: 1 1;
    }

    .fiddle-editor > .CodeMirror {
        height: 100%;
    }
  ")

(hf-def/fiddle :hyperfiddle.ide/entry-point-fiddles
  :query '[:find
           (pull ?e [:hyperfiddle/starred
                     :fiddle/ident
                     :db/id])
           (max ?tx)
           :where
           [?e :fiddle/ident ?ident]
           (or-join [?e]
             ; todo *subject* unsupported on peer
             #_(and
                 [(ground hyperfiddle.io.bindings/*subject*) ?user-id]
                 [?e :hyperfiddle/owners ?user-id])
             [(missing? $ ?e :hyperfiddle/owners)])
           [?e _ _ ?tx]]

  :renderer
  (if (= "nodejs" *target*)
    (hyperfiddle.ui.loading/loading-page)
    [:div.container-fluid props
     (when (empty? (:hyperfiddle.route/where @(:hypercrud.browser/route ctx)))
       [:<>
        [:p "My starred fiddles"]
        (let [[as _] ((juxt filter remove) (comp :hyperfiddle/starred first) val)]
          [user/links-list ctx as])
        [:p "My recent fiddles"]
        [user/links-list ctx (->> val (sort-by second) reverse (take 5))]])
     [:p "All my fiddles"]
     [:div.form-group
      [hyperfiddle.ui/needle-input '[[(str ?ident) ?si] [(.contains ?si %)]] ctx
       {:id "hyperfiddle/entry-point-needle"
        :style {:display "inline-block"}                       ; bootstrap styles are half imported, wonky hacks
        :class "col-sm-10 form-control"
        :auto-complete "off"
        #_#_:auto-focus true                                   ; page sroll pos gets lost, otherwise this is great
        :placeholder ":fiddle/ident filter"}]]
     [user/links-list ctx (user/sort-by-fiddle-ident val)]]
    )

  :code
  [(defn sort-by-fiddle-ident [xxs]
     (->> xxs
          (sort (fn [[a] [b]]
                  (compare (clojure.string/replace (str (:fiddle/ident a)) #"/" ".")
                    (clojure.string/replace (str (:fiddle/ident b)) #"/" "."))))))

   (defn links-list [ctx xs]
     [:table.table-hover
      [:tbody
       (->> xs
            (map (fn [[e :as row]]
                   (let [k (hypercrud.browser.context/row-key ctx row)
                         ctx (hypercrud.browser.context/row ctx k)]
                     [:tr {:key k}
                      [:td [hyperfiddle.ui/value [0 :hyperfiddle/starred] ctx]]
                      (let [route {:hyperfiddle.route/fiddle       :hyperfiddle.ide/edit
                                   :hyperfiddle.route/datomic-args [(:fiddle/ident e)]}]
                        [:td [hyperfiddle.ui/anchor ctx {:route route} (str (:fiddle/ident e))]])])))
            (doall))]])]

  :css "
    .-hyperfiddle-ide-entry-point-fiddles table {
      width: 100%;
    }
    .-hyperfiddle-ide-entry-point-fiddles tr > td:first-child {
      width: 1.5em;
    }
  ")

(hf-def/fiddle :hyperfiddle.ide/please-login
  :renderer
  (let [tunneled-route (first (:hyperfiddle.route/datomic-args @(:hypercrud.browser/route ctx)))
        state (hyperfiddle.domain/url-encode (hyperfiddle.runtime/domain (:runtime ctx)) tunneled-route)
        href (hyperfiddle.ide/stateless-login-url ctx state)]
    [:div
     [:br]
     [:center [:h3 "Please " [:a {:href href} "login"]]]]))

(hf-def/fiddle :hyperfiddle.ide/schema
  :link [":hyperfiddle.ide/schema" [:hf/iframe] :hyperfiddle.ide/schema-editor
         :formula "(constantly (->> @(:hypercrud.browser/route ctx) second first (get (:hyperfiddle.ide.domain/user-dbname->ide (hyperfiddle.runtime/domain (:runtime ctx)))) hypercrud.types.DbName/->DbName))"]
  :link [":hyperfiddle.ide/schema" [:hf/iframe] :hyperfiddle/topnav]

  :renderer [[:<>
              [hyperfiddle.ui/ui-from-link
               (hyperfiddle.data/select ctx :hyperfiddle/topnav) ctx {}]
              [hyperfiddle.ui/ui-from-link
               (hyperfiddle.data/select ctx :hyperfiddle.ide/schema-editor) ctx {}]]]

  :css "
    .-hyperfiddle-ide-schema-editor {
      flex: 1 1;
      overflow: auto;
    }
  ")

(hf-def/fiddle :hyperfiddle.ide/schema-editor
  :query '[:find [(pull $_ ?attr
                    [:db/id
                     :db/ident
                     {:db/valueType [:db/ident]}
                     {:db/cardinality [:db/ident]}
                     {:db/unique [:db/ident]}
                     :db/isComponent :db/fulltext :db/doc]) ...]
           :in $_
           :where [$_ :db.part/db :db.install/attribute ?attr]]

  :link [":db/ident" [:hf/edit] :hyperfiddle.ide/schema.attribute
         :formula "(fn [e] [(first (second @(:hypercrud.browser/route ctx))) e])"]
  :link [":db/ident" [:hf/new] :hyperfiddle.ide/schema-editor.attribute
         :formula "(constantly [(first (second @(:hypercrud.browser/route ctx))) (hyperfiddle.api/tempid! ctx)])"]

  :renderer
  (let [hide-datomic (contrib.reactive/atom true)
        hide-archived (contrib.reactive/atom true)
        is-edn (contrib.reactive/atom false)
        needle (contrib.reactive/atom nil)
        db-attr? #(<= (:db/id %) 62)
        archived? #(cuerdas.core/starts-with? (namespace (:db/ident %)) "zzz") ; "zzz/" and "zzz.", we are inconsistent
        do-filter-reactive (fn [xs]                           ; perf sensitive
                             (as-> xs xs
                               (if @hide-datomic (remove db-attr? xs) xs)
                               (if @hide-archived (remove archived? xs) xs)
                               (if (contrib.string/blank->nil @needle)
                                 (filter #(cuerdas.core/includes? (-> % :db/ident str) @needle) xs) xs)))]
    (fn [val ctx props]
      [:div.container-fluid props
       [:h3 (str "Datomic schema for " (-> (hyperfiddle.runtime/domain (:runtime ctx))
                                           :hyperfiddle.ide.domain/user-dbname->ide
                                           clojure.set/map-invert
                                           (get (-> @(:hypercrud.browser/route ctx) second first :dbname))))]
       [:div [:label [:input {:type "checkbox" :checked @hide-datomic :on-change #(swap! hide-datomic not)}] " hide Datomic system attributes"]]
       [:div [:label [:input {:type "checkbox" :checked @hide-archived :on-change #(swap! hide-archived not)}] " hide Hyperfiddle archived attributes"]]
       [:div [:label [:input {:type "checkbox" :checked @is-edn :on-change #(swap! is-edn not)}] " EDN view"]]
       [contrib.ui/text {:value @needle
                         :on-change #(do (reset! needle %))}
        {:placeholder ":task/title"}]
       (let [ctx (-> ctx
                     (update :hypercrud.browser/result (partial contrib.reactive/fmap do-filter-reactive))
                     (assoc :hyperfiddle.ui/layout :hyperfiddle.ui.layout/table))]
         (if @is-edn
           [contrib.ui/code {:value (-> (hypercrud.browser.context/data ctx)
                                        (->> (sort-by :db/ident)
                                             (map #(dissoc % :db/id)))
                                        (contrib.pprint/pprint-str 1000))
                             :read-only true}]
           [hyperfiddle.ui/table
            (fn [ctx]
              [(hyperfiddle.ui/field [:db/ident] ctx)
               (hyperfiddle.ui/field [:db/valueType] ctx #(hyperfiddle.ui.controls/string ((comp (fnil name :–) :db/ident) %) %2 %3) {:disabled true})
               (hyperfiddle.ui/field [:db/cardinality] ctx #(hyperfiddle.ui.controls/string ((comp (fnil name :–) :db/ident) %) %2 %3) {:disabled true})
               (hyperfiddle.ui/field [:db/unique] ctx #(hyperfiddle.ui.controls/string ((comp (fnil name :–) :db/ident) %) %2 %3) {:disabled true})
               (hyperfiddle.ui/field [:db/isComponent] ctx)
               (hyperfiddle.ui/field [:db/fulltext] ctx nil {:disabled true})
               (hyperfiddle.ui/field [:db/doc] ctx)])
            ctx
            {:hyperfiddle.ui.sort/initial-sort [[:db/ident] :asc]}]))]))
  )

(hf-def/fiddle :hyperfiddle.ide/schema-editor.attribute
  :pull "$_"
  [:db/id
   :db/ident
   {:db/valueType [:db/ident]}
   {:db/cardinality [:db/ident]}
   :db/doc
   {:db/unique [:db/ident]}
   :db/isComponent
   :db/fulltext]

  :renderer hyperfiddle.ide.fiddles.schema-attribute/renderer

  :link [":hyperfiddle.ide/schema-editor.attribute" [:cardinality-options :hf/iframe]
         :hyperfiddle.ide/schema.options.cardinality]

  :link [":hyperfiddle.ide/schema-editor.attribute" [:valueType-options :hf/iframe]
         :hyperfiddle.ide/schema.options.valuetype]

  :link [":hyperfiddle.ide/schema-editor.attribute" [:valueType-options :hf/iframe]
         :hyperfiddle.ide/schema.options.unique])

(hf-def/fiddle :hyperfiddle.ide/schema.attribute
  :link [":hyperfiddle.ide/schema.attribute" [:hf/iframe] :hyperfiddle/topnav]
  :link [":hyperfiddle.ide/schema.attribute" [:hf/iframe] :hyperfiddle.ide/schema-editor.attribute
         :formula "(constantly (second @(:hypercrud.browser/route ctx)))"]

  :renderer [[:<>
              [hyperfiddle.ui/ui-from-link
               (hyperfiddle.data/select ctx :hyperfiddle/topnav) ctx {}]
              [hyperfiddle.ui/ui-from-link
               (hyperfiddle.data/select ctx :hyperfiddle.ide/schema-editor.attribute) ctx {}]]]

  :css "
    .-hyperfiddle-ide-schema-editor-attribute {
      flex: 1 1;
      overflow: auto;
    }
  ")

(hf-def/fiddle :hyperfiddle.ide/schema.options.cardinality
  :query '[:find [(pull $_ ?e [:db/ident]) ...]
           :in $_
           :where
           [$_ ?e :db/ident ?ident]
           [(namespace ?ident) ?ns]
           [(= ?ns "db.cardinality")]])

(hf-def/fiddle :hyperfiddle.ide/schema.options.unique
  :query '[:find [(pull $_ ?e [:db/ident]) ...]
           :in $_
           :where
           [$_ ?e :db/ident ?ident]
           [(namespace ?ident) ?ns]
           [(= ?ns "db.unique")]])

(hf-def/fiddle :hyperfiddle.ide/schema.options.valuetype
  :query '[:find [(pull $_ ?valueType [:db/ident]) ...]
           :in $_
           :where
           [$_ ?db-part :db.install/valueType ?valueType]
           [$_ ?db-part :db/ident :db.part/db]])

(hf-def/fiddle :hyperfiddle.ide.user/databases
  :query '[:find [(pull $domains ?db [:database/uri :db/id]) ...]
           :in $domains
           :where
           [(ground hyperfiddle.io.bindings/*subject*) ?user-id]
           [$domains ?db :database/uri]
           [$domains ?db :hyperfiddle/owners ?user-id]]

  :markdown "Databases"

  :renderer
  (let [{:keys [:hypercrud.browser/fiddle]} ctx
        dbs (hypercrud.browser.context/data ctx)]
    (when-not (empty? dbs)
      [:div props
       [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]
       [:ul
        (->> (sort-by :database/uri dbs)
             (map (fn [db]
                    [:li {:key (hash (:db/id db))} (str (:database/uri db))]))
             (doall))]])))

(hf-def/fiddle :hyperfiddle.ide.schema/options.cardinality [:ide-dbname]
  :with {:$db (symbol (fiddle-args :ide-dbname))}
  :query
  '[:in ~(fiddle-args :$db) :find (list 'pull ~(fiddle-args :$db) '?e [:db/ident]) :where
   [~(fiddle-args :$db) ?e :db/ident ?ident]
   [(namespace ?ident) ?ns]
   [(= ?ns "db.cardinality")]])

(hf-def/fiddle :hyperfiddle.ide.schema/options.unique [:ide-dbname]
  :with {:$db (symbol (fiddle-args :ide-dbname))}
  :query '[:in ~(fiddle-args $db) :find (list 'pull ~(fiddle-args $db) '?e [:db/ident]) :where
           [~(fiddle-args $db) ?e :db/ident ?ident]
           [(namespace ?ident) ?ns]
           [(= ?ns "db.unique")]])

(hf-def/fiddle :hyperfiddle.ide.schema/options.valueType [:ide-dbname]
  :with {:$db (symbol (fiddle-args :ide-dbname))}
  :query
  '[:in ~(fiddle-args :$db) :find (pull ~(fiddle-args :$db) ?valueType [:db/ident]) :where
    [~(fiddle-args :$db) ?db-part :db.install/valueType '?valueType]
    [~(fiddle-args :$db) ?db-part :db/ident :db.part/db]])

(hf-def/fiddle :hyperfiddle.ide.schema/editor.attribute [:ide-dbname]
  :with {:ident (with-ns 'hyperfiddle.ide.schema (str "editor.attribute" (fiddle-args :ide-dbname)))}
  :pull ~(fiddle-args :ide-dbname)
        [:db/id                                             ; for smart-identity tempid stability
         :db/ident
         {:db/valueType [:db/ident]}
         {:db/cardinality [:db/ident]}
         :db/doc
         {:db/unique [:db/ident]}
         :db/isComponent
         :db/fulltext]

  :renderer hyperfiddle.ide.fiddles.schema-attribute/renderer

  :link [~(fiddle-args :ident) [:hf/iframe :cardinality-options] ~(via :hyperfiddle.ide.schema/options.cardinality :ide-dbname)]
  :link [~(fiddle-args :ident) [:hf/iframe :unique-options] ~(via :hyperfiddle.ide.schema/options.unique :ide-dbname)]
  :link [~(fiddle-args :ident) [:hf/iframe :valueType-options] ~(via :hyperfiddle.ide.schema/options.valueType :ide-dbname)])

(hf-def/fiddle :hyperfiddle.ide.schema/attribute [:user-dbname :user-dbname->ide]
  :with {:ident      (keyword "hyperfiddle.ide.schema" (str "attribute" (fiddle-args :user-dbname)))
         :ide-dbname ((fiddle-args :user-dbname->ide) (fiddle-args :user-dbname))}

  :link [~(fiddle-args :ident) [:hf/iframe] :hyperfiddle/topnav]

  :link [~(fiddle-args :ident) [:hf/iframe] ~(via :hyperfiddle.ide.schema/editor.attribute ide-dbname)
            :formula (constantly (first (:hyperfiddle.route/datomic-args @(:hypercrud.browser/route ctx))))]

  :renderer
  [:<>
   [hyperfiddle.ui/ui-from-link
    (hyperfiddle.data/select ctx :hyperfiddle/topnav) ctx {}]
   [hyperfiddle.ui/ui-from-link
    (hyperfiddle.data/select ctx :hyperfiddle.ide.schema/editor.attribute ide-dbname) ctx {}]]

  :fiddle/css "
     .-hyperfiddle-ide-schema-editor-attribute {
       flex: 1 1;
       overflow: auto;
     }
   ")

(hf-def/fiddle :hyperfiddle.ide.schema/editor [:ide-dbname :user-dbname->ide]
  :with {:$db (symbol (fiddle-args :ide-dbname))}

  :query
  '[:in ~(fiddle-args :$db) :find (pull ~(fiddle-args :$db) ?attr
                    [:db/id
                     :db/ident
                     {:db/valueType [:db/ident]}
                     {:db/cardinality [:db/ident]}
                     {:db/unique [:db/ident]}
                     :db/isComponent :db/fulltext :db/doc])
   :where [~(fiddle-args :$db) :db.part/db :db.install/attribute '?attr]
   ; binding for ui
   [~(fiddle-args :$db) ?attr :db/ident ?ident]]

  :renderer hyperfiddle.ide.fiddles.schema-editor/renderer

  :link [~(str (fiddle-args :ide-dbname) " :db/ident") ~(via :hyperfiddle.ide.schema/attribute
                                           ((set/map-invert (fiddle-args :user-dbname->ide)) (fiddle-args :ide-dbname)) user-dbname->ide)]

  :link [~(str (fiddle-args :ide-dbname) " :db/ident") [:hf/new] ~(via :hyperfiddle.ide.schema/editor.attribute ide-dbname)]

  :link {:db/id :hyperfiddle.ide.schema.link/edit-attribute
         :link/path
         :link/fiddle
         :link/formula "identity"}

  :link {:db/id :hyperfiddle.ide.schema.link/new-attribute
         :link/path ~(str ide-dbname " :db/ident")
         :link/class #{:hf/new}
         :link/fiddle ~(via :hyperfiddle.ide.schema/editor.attribute ide-dbname)})

(hf-def/fiddle :hyperfiddle.ide.schema/_ [:user-dbname :user-dbname->ide]
  :with
  {:ident ~(keyword "hyperfiddle.ide.schema" user-dbname)
   :ide-dbname (user-dbname->ide user-dbname)}

  :link {~ident [[:hf/iframe ~(via :hyperfiddle.ide.schema/editor ide-dbname user-dbname->ide)]
                 ]}

  :link [~ident [:hf/iframe] ~(via :hyperfiddle.ide.schema/editor ide-dbname user-dbname->ide)]
  :link [~ident [:hf/iframe] :hyperfiddle/topnav]

  :renderer
  [:<>
   [hyperfiddle.ui/ui-from-link
    (hyperfiddle.data/select ctx :hyperfiddle/topnav) ctx {}]
   [hyperfiddle.ui/ui-from-link
    (hyperfiddle.data/select ctx :hyperfiddle.ide.schema/editor
      ide-dbname) ctx {}]]

  :css "
     .-hyperfiddle-ide-schema-editor {
       flex: 1 1;
       overflow: auto;
     }
  ")
