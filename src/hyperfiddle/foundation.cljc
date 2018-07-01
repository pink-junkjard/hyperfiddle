(ns hyperfiddle.foundation
  (:refer-clojure :exclude [read-string])
  (:require [cats.monad.either :as either :refer [branch left right]]
            [clojure.string :as string]
    #?(:cljs [contrib.css :refer [css]])
            [contrib.base-64-url-safe :as base64-url-safe]
            [contrib.data :refer [update-existing]]
            [contrib.eval :as eval]
            [contrib.reactive :as r]
            [contrib.reader :refer [read-string read-edn-string]]
    #?(:cljs [contrib.reagent :refer [fragment]])
            [contrib.pprint :refer [pprint-str pprint-datoms-str]]
    #?(:cljs [contrib.ui :refer [code]])
    #?(:cljs [contrib.ui.tooltip :refer [tooltip]])
            [hypercrud.browser.routing :as routing]
            [hypercrud.browser.router :as router]
            [hypercrud.client.core :as hc]
            [hypercrud.types.Entity :refer [shadow-entity]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hypercrud.types.Err :as Err]
            [contrib.uri :refer [is-uri?]]
    #?(:cljs [hypercrud.ui.stale :as stale])
            [hyperfiddle.actions :as actions]
            [hyperfiddle.runtime :as runtime]
    ;#?(:cljs [hyperfiddle.ui :refer [markdown]])
            [promesa.core :as p]
    #?(:cljs [re-com.tabs :refer [horizontal-tabs]])))


(def domain-uri #uri "datomic:free://datomic:4334/domains")
(def source-domain-ident "hyperfiddle")                     ; todo this needs to be configurable
(def auth0-redirect-path "/auth0")                          ; ide

#?(:cljs
   (defn stateless-login-url [ctx]
     (let [{:keys [domain client-id]} (get-in ctx [:hypercrud.browser/domain :domain/environment :auth0 (get-in ctx [:host-env :ide/root])])]
       (str domain "/login?"
            "client=" client-id
            "&scope=" "openid email profile"
            "&state=" (base64-url-safe/encode (runtime/encode-route (:peer ctx) (:target-route ctx)))
            "&redirect_uri=" (str "http://" (get-in ctx [:host-env :hostname]) auth0-redirect-path)))))

(defn domain-request [domain-eid peer]
  (->EntityRequest domain-eid nil
                   (hc/db peer domain-uri nil)
                   [:db/id
                    :hyperfiddle/owners
                    :domain/aliases
                    :domain/disable-javascript
                    :domain/environment
                    :domain/fiddle-repo
                    :domain/ident

                    ; These are insecure fields, they should be redundant here, but there is order of operation issues
                    :domain/code
                    :domain/css
                    :domain/router
                    :domain/home-route
                    ]))

(defn domain-request-insecure [domain-eid peer branch]
  (->EntityRequest domain-eid nil (hc/db peer domain-uri branch)
                   [:db/id
                    :domain/code
                    :domain/css
                    :domain/router
                    :domain/home-route]))

(defn databases-request [rt branch domain]
  ; todo env databases should just be modeled as refs and pulled in domain-request
  (->> (:domain/environment domain)
       (filter (fn [[k v]] (and (string? k) (string/starts-with? k "$") (is-uri? v))))
       (map second)
       (into #{(:domain/fiddle-repo domain) domain-uri})
       (vector)
       (into [(hc/db rt domain-uri branch)])
       (->QueryRequest
         '[:find ?uri (pull ?db [{:database/write-security [:db/ident]} :hyperfiddle/owners])
           :in $ [?uri ...]
           :where [?db :database/uri ?uri]])))

#?(:cljs
   (defn error-cmp [e]
     [:div
      [:h1 "Fatal error"]
      (if (Err/Err? e)
        [:div
         [:h3 (:msg e)]
         (when-let [data (:data e)]
           [:pre data])]
        [:div
         [:fieldset [:legend "(pr-str e)"]
          [:pre (pr-str e)]]
         [:fieldset [:legend "(ex-data e)"]                 ; network error
          [:pre (:data e)]]                                 ; includes :body key
         [:fieldset [:legend "(.-stack e)"]                 ; network error
          [:pre (.-stack e)]]])]))

(defn process-domain [domain]
  (-> (into {} domain) (update-existing :domain/environment read-string) #_"todo this can throw"))

(defn context [ctx source-domain user-domain-insecure]
  ; Secure first, which is backwards, see `domain-request` comment
  (let [domain (into @(runtime/state (:peer ctx) [::runtime/domain]) user-domain-insecure)]
    (assoc ctx
      :hypercrud.browser/domain domain
      :hypercrud.browser/invert-route (r/partial routing/invert-route domain)
      :hypercrud.browser/source-domain (process-domain source-domain))))

(defn local-basis [page-or-leaf global-basis route ctx f]
  (concat
    (:domain global-basis)
    (f global-basis route ctx)))

(defn api [page-or-leaf route ctx f]
  (let [source-domain-req (domain-request [:domain/ident source-domain-ident] (:peer ctx))
        user-domain-insecure-req (-> [:domain/ident @(runtime/state (:peer ctx) [::runtime/domain :domain/ident])]
                                     (domain-request-insecure (:peer ctx) (:branch ctx)))]
    (into [source-domain-req user-domain-insecure-req]
          (let [source-domain (hc/hydrate-api (:peer ctx) (:branch ctx) source-domain-req)
                user-domain-insecure (hc/hydrate-api (:peer ctx) (:branch ctx) user-domain-insecure-req)]
            (when (and source-domain user-domain-insecure)
              (let [ctx (context ctx source-domain user-domain-insecure)]
                (f route ctx)))))))

#?(:cljs
   (defn ^:export staging [ctx & [child]]
     (let [source-uri @(runtime/state (:peer ctx) [::runtime/domain :domain/fiddle-repo])
           selected-uri (r/atom source-uri)
           tabs-definition (->> @(runtime/state (:peer ctx) [::runtime/domain :domain/environment])
                                (filter (fn [[k v]] (and (string? k) (string/starts-with? k "$") (is-uri? v))))
                                (reduce (fn [acc [dbname uri]]
                                          (update acc uri (fnil conj '()) dbname))
                                        {source-uri '("src")
                                         ; domains-uri shouldn't need to be accessed
                                         domain-uri '("domain")})
                                (mapv (fn [[uri labels]] {:id uri :label (string/join " " labels)}))
                                (sort-by :label))
           change-tab #(reset! selected-uri %)]
       (fn [ctx & [child]]
         (let [stage (runtime/state (:peer ctx) [:stage (:branch ctx) @selected-uri])]
           (fragment
             :topnav
             [horizontal-tabs
              :model selected-uri
              :tabs tabs-definition
              :on-change change-tab]
             [code
              (pprint-datoms-str @stage)
              #(runtime/dispatch! (:peer ctx) (actions/reset-stage-uri (:peer ctx) (:branch ctx) @selected-uri (read-edn-string %)))]
             (when child [child selected-uri stage ctx])
             #_[markdown "Hyperfiddle always generates valid transactions, if it doesn't, please file a bug.

*WARNING:* Datomic schema alterations cannot be used in the same transaction, for now you'll
need to transact the schema before using, see [#6](https://github.com/hyperfiddle/hyperfiddle/issues/6)."]))))))

#?(:cljs
   (defn leaf-view [route ctx f]
     ; A malformed stage can break bootstrap hydrates, but the root-page is bust, so ignore here
     ; Fix this by branching userland so bootstrap is sheltered from staging area? (There are chickens and eggs)
     (f route ctx)))

#?(:cljs
   (defn domain-init! [domain f]
     (let [result (if-let [cljs (:domain/code domain)]
                    (eval/safe-eval-string cljs)
                    (right nil))]
       (fn reagent-render [domain f]
         (branch
           result
           (fn error [e]
             (js/console.warn ":domain/code eval: " e)
             (f))
           (fn []
             (f)))))))

#?(:cljs
   (defn page-view [route ctx f]
     ; Necessary wrapper div, this is returned from react-render
     [:div {:class (apply css @(runtime/state (:peer ctx) [:pressed-keys]))}
      [:style {:dangerouslySetInnerHTML {:__html (:domain/css (:hypercrud.browser/domain ctx))}}]
      (f route ctx)                                         ; nil, seq or reagent component
      (when @(runtime/state (:peer ctx) [:staging-open])
        (let [stage-val @(runtime/state (:peer ctx) [:stage])
              edn (pprint-str stage-val 70)]
          ; todo this can throw
          [code edn #(runtime/dispatch! (:peer ctx) (actions/reset-stage (:peer ctx) (read-edn-string %)))]))]))

#?(:cljs
   (defn page-or-leaf1 [page-or-leaf route ctx f]
     (case page-or-leaf
       ; The foundation comes with special root markup which means the foundation/view knows about page/user (not ide)
       ; Can't ide/user (not page) be part of the userland route?
       :page (page-view route ctx f)
       :leaf (leaf-view route ctx f))))

#?(:cljs
   (defn view [page-or-leaf route ctx f]
     (let [source-domain @(hc/hydrate (:peer ctx) (:branch ctx) (domain-request [:domain/ident source-domain-ident] (:peer ctx)))
           user-domain-insecure (-> [:domain/ident @(runtime/state (:peer ctx) [::runtime/domain :domain/ident])]
                                    (domain-request-insecure (:peer ctx) (:branch ctx))
                                    (->> (hc/hydrate (:peer ctx) (:branch ctx)))
                                    deref)]
       (if-let [e (or @(runtime/state (:peer ctx) [::runtime/fatal-error])
                      (when (either/left? source-domain) @source-domain))]
         [:div                                              ; necessary wrapper div, it is the react root
          [error-cmp e]
          [staging (:peer ctx)]]
         (let [ctx (context ctx @source-domain @user-domain-insecure)
               domain (:hypercrud.browser/domain ctx)]
           ; f is nil, seq or reagent component
           ^{:key domain}
           [domain-init! domain (r/partial page-or-leaf1 page-or-leaf route ctx f)])))))

(defn confirm [message]
  #?(:clj  (throw (ex-info "confirm unsupported by platform" nil))
     :cljs (js/confirm message)))

(defn navigable? [route state]
  (and (not= route (get-in state [::runtime/partitions nil :route]))
       (or (empty? (dissoc (:stage state) nil))
           (confirm "Unstaged work will be lost on navigate, are you sure?"))))

(def LEVEL-NONE 0)
(def LEVEL-GLOBAL-BASIS 1)
(def LEVEL-DOMAIN 2)
(def LEVEL-ROUTE 3)
(def LEVEL-LOCAL-BASIS 4)
(def LEVEL-HYDRATE-PAGE 5)

; this needs to be a bit smarter; this should be invoked by everyone (all service endpoints, ssr, browser)
; e.g. for service/hydrate-route, we have route, and local-basis, just need to fetch domain & hydrate
; it makes no sense for clients to forward domains along requests (same as global-basis),
; so we need to inject into the domain level and then continue on at the appropriate level.
; could also handle dirty staging areas for browser
(defn bootstrap-data [rt init-level load-level encoded-route initial-global-basis & [dirty-stage?]] ;branch and aux as parameter?
  (if (>= init-level load-level)
    (p/resolved nil)
    (-> (condp = (inc init-level)
          LEVEL-GLOBAL-BASIS (actions/refresh-global-basis rt (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
          LEVEL-DOMAIN (actions/refresh-domain rt (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
          LEVEL-ROUTE (let [branch-aux {:hyperfiddle.ide/foo "page"}] ;ide
                        (try (let [route (runtime/decode-route rt encoded-route)]
                               (when-let [e (router/invalid-route? route)] (throw e))
                               (runtime/dispatch! rt [:add-partition nil route branch-aux]))
                             (p/resolved nil)
                             (catch #?(:cljs :default :clj Exception) e
                               (runtime/dispatch! rt [:set-error e])
                               (p/rejected e))))
          LEVEL-LOCAL-BASIS (actions/refresh-partition-basis rt nil (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
          LEVEL-HYDRATE-PAGE (if (or (not= initial-global-basis @(runtime/state rt [::runtime/global-basis])) dirty-stage?)
                               (actions/hydrate-partition rt nil nil (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
                               (p/resolved nil)))
        (p/then #(bootstrap-data rt (inc init-level) load-level encoded-route initial-global-basis dirty-stage?)))))
