(ns hyperfiddle.foundation
  (:refer-clojure :exclude [read-string])
  (:require
    [cats.monad.either :as either :refer [branch left right]]
    [clojure.string :as string]
    #?(:cljs [contrib.css :refer [css]])
    [contrib.base-64-url-safe :as base64-url-safe]
    [contrib.data :refer [update-existing]]
    [contrib.eval :as eval]
    [contrib.reactive :as r]
    [contrib.reader :refer [read-string read-edn-string!]]
    #?(:cljs [contrib.reagent :refer [fragment]])
    [contrib.pprint :refer [pprint-datoms-str]]
    [contrib.string :refer [or-str]]
    #?(:cljs [contrib.ui :refer [code debounced markdown validated-cmp]])
    [hypercrud.browser.context :as context]
    [hypercrud.browser.routing :as routing]
    [hypercrud.browser.router :as router]
    [hypercrud.client.core :as hc]
    [hypercrud.types.EntityRequest :refer [->EntityRequest]]
    [hypercrud.types.Err :as Err]
    #?(:cljs [hypercrud.ui.stale :as stale])
    [hyperfiddle.actions :as actions]
    [hyperfiddle.domain]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.security.domains]
    [promesa.core :as p]
    #?(:cljs [re-com.tabs :refer [horizontal-tabs]])))


(def domain-uri #uri "datomic:free://datomic:4334/domains")
(def source-domain-ident "hyperfiddle")                     ; todo this needs to be configurable
(def auth0-redirect-path "/auth0")                          ; ide

#?(:cljs
   (defn stateless-login-url [ctx]
     (let [{:keys [hostname :ide/root]} (runtime/host-env (:peer ctx))
           {:keys [domain client-id]} (get-in ctx [:hypercrud.browser/domain :domain/environment :auth0 root])]
       (str domain "/login?"
            "client=" client-id
            "&scope=" "openid email profile"
            "&state=" (base64-url-safe/encode (runtime/encode-route (:peer ctx) (context/target-route ctx)))
            "&redirect_uri=" (str "http://" hostname auth0-redirect-path)))))

(defn domain-request [domain-eid peer]
  (->EntityRequest domain-eid
                   (hc/db peer domain-uri nil)
                   [:db/id
                    :hyperfiddle/owners
                    :domain/aliases
                    {:domain/databases [:domain.database/name
                                        {:domain.database/record [:database/uri
                                                                  :database.custom-security/client
                                                                  {:database/write-security [:db/ident]}
                                                                  :hyperfiddle/owners]}]
                     :domain/fiddle-database [:database/uri
                                              :database.custom-security/client
                                              {:database/write-security [:db/ident]}
                                              :hyperfiddle/owners]}
                    :domain/disable-javascript
                    :domain/environment
                    :domain/ident
                    :domain/router
                    :domain/home-route

                    ; These are insecure fields, they should be redundant here, but there is order of operation issues
                    :domain/code
                    :domain/css
                    ]))

(defn domain-request-insecure [domain-eid peer branch]
  (->EntityRequest domain-eid
                   (hc/db peer domain-uri branch)
                   [:db/id
                    :domain/code
                    :domain/css]))

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
          [:pre (str (:data e))]]                           ; includes :body key
         [:fieldset [:legend "(.-stack e)"]                 ; network error
          [:pre (.-stack e)]]])]))

(defn shadow-domain [domain]
  ; also called from the view, which wants database types, so separate from process-domain
  (update domain :domain/home-route or-str "[:hyperfiddle.ide/entry-point-fiddles]"))

(defn process-domain [domain]                               ; this should be memoized at the call site?
  (-> domain
      (update-existing :domain/environment read-string) #_"todo this can throw"
      (shadow-domain)))

(defn context [ctx source-domain user-domain-insecure]
  ; Secure first, which is backwards, see `domain-request` comment
  (let [domain (into @(runtime/state (:peer ctx) [::runtime/domain]) user-domain-insecure)]
    (assoc ctx
      :hypercrud.browser/domain domain                      ; already processed
      :hypercrud.browser/invert-route (r/partial routing/invert-route domain)
      :hypercrud.browser/source-domain (process-domain source-domain))))

(defn local-basis [page-or-leaf global-basis route ctx f]
  (concat
    (:domain global-basis)
    (f global-basis route ctx)))

(defn api [page-or-leaf ctx f]
  (let [source-domain-req (domain-request [:domain/ident source-domain-ident] (:peer ctx))
        user-domain-insecure-req (-> [:domain/ident @(runtime/state (:peer ctx) [::runtime/domain :domain/ident])]
                                     (domain-request-insecure (:peer ctx) (:branch ctx)))]
    (into [source-domain-req user-domain-insecure-req]
          (let [source-domain (hc/hydrate-api (:peer ctx) (:branch ctx) source-domain-req)
                user-domain-insecure (hc/hydrate-api (:peer ctx) (:branch ctx) user-domain-insecure-req)]
            (when (and source-domain user-domain-insecure)
              (let [ctx (context ctx source-domain user-domain-insecure)]
                (f ctx)))))))

#?(:cljs
   (let [parse-string (fn [s]
                        (let [v (read-edn-string! s)]
                          (assert (and (or (nil? v) (vector? v) (seq? v))
                                       (every? (fn [v] (or (map? v) (vector? v) (seq? v))) v)))
                          v))
         to-string pprint-datoms-str
         on-change (fn [peer branch uri-ref o n]
                     (runtime/dispatch! peer (actions/reset-stage-uri peer branch @uri-ref n)))
         code-wrapper (fn [props]
                        [:div.staging-cm-wrapper (select-keys props [:class])
                         [code props]])]
     (defn staging-control [ctx uri-ref]
       (let [props {:value @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :stage @uri-ref])
                    :readOnly @(runtime/state (:peer ctx) [::runtime/auto-transact @uri-ref])
                    :on-change (r/partial on-change (:peer ctx) (:branch ctx) uri-ref)}]
         [debounced props validated-cmp parse-string to-string code-wrapper]))))

#?(:cljs
   (defn ^:export staging [ctx & [child]]
     (let [source-uri (runtime/state (:peer ctx) [::runtime/domain :domain/fiddle-database :database/uri])
           selected-uri (runtime/state (:peer ctx) [:staging/selected-uri]) ; this is only writabel because the browser's state impl uses reagent cursors all the way to the top
           change-tab #(reset! selected-uri %)]
       (fn [ctx & [child]]
         (let [non-empty-uris (->> @(runtime/state (:peer ctx) [::runtime/partitions nil :stage])
                                   (remove (comp empty? second))
                                   (map first)
                                   (remove (->> @(runtime/state (:peer ctx) [::runtime/domain :domain/databases])
                                                (map (comp :database/uri :domain.database/record))
                                                (into #{@source-uri})))
                                   (map (juxt identity (fn [uri]
                                                         [(if (= uri domain-uri)
                                                            "domains"
                                                            (str uri))])))
                                   (into {}))
               tabs-definition (->> @(runtime/state (:peer ctx) [::runtime/domain :domain/databases])
                                    (reduce (fn [acc hf-db]
                                              (let [uri (get-in hf-db [:domain.database/record :database/uri])]
                                                (update acc uri (fnil conj '()) (:domain.database/name hf-db))))
                                            (into non-empty-uris
                                                  {@source-uri '("src")}))
                                    (mapv (fn [[uri labels]] {:id uri :label (string/join " " labels)}))
                                    (sort-by :label))
               stage (runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :stage @selected-uri])]
           (when-not (contains? (->> tabs-definition (map :id) (into #{})) @selected-uri)
             (reset! selected-uri (or (hyperfiddle.domain/dbname->uri "$" @(runtime/state (:peer ctx) [::runtime/domain]))
                                      @source-uri)))
           (fragment
             :topnav
             [horizontal-tabs
              :model selected-uri
              :tabs tabs-definition
              :on-change change-tab]
             ^{:key (str @selected-uri)}
             [staging-control ctx selected-uri]
             (when child
               [child selected-uri stage ctx])))))))

#?(:cljs
   (defn leaf-view [ctx f]
     ; A malformed stage can break bootstrap hydrates, but the root-page is bust, so ignore here
     ; Fix this by branching userland so bootstrap is sheltered from staging area? (There are chickens and eggs)
     (f ctx)))

#?(:cljs
   (defn domain-init! [domain f]
     (let [result (if-let [cljs (:domain/code domain)]
                    (eval/safe-eval-string+ cljs)
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
   (defn page-view [ctx f]
     ; Necessary wrapper div, this is returned from react-render
     [:div {:class (apply css @(runtime/state (:peer ctx) [:pressed-keys]))}
      [:style {:dangerouslySetInnerHTML {:__html (:domain/css (:hypercrud.browser/domain ctx))}}]
      (f ctx)                                               ; nil, seq or reagent component
      ]))

#?(:cljs
   (defn view [page-or-leaf ctx f]
     (let [source-domain @(hc/hydrate (:peer ctx) (:branch ctx) (domain-request [:domain/ident source-domain-ident] (:peer ctx)))
           user-domain-insecure (-> [:domain/ident @(runtime/state (:peer ctx) [::runtime/domain :domain/ident])]
                                    (domain-request-insecure (:peer ctx) (:branch ctx))
                                    (->> (hc/hydrate (:peer ctx) (:branch ctx)))
                                    deref)]
       (if-let [e (or @(runtime/state (:peer ctx) [::runtime/fatal-error])
                      (when (either/left? source-domain) @source-domain))]
         [:div                                              ; necessary wrapper div, it is the react root
          [error-cmp e]
          [staging ctx]]
         (let [ctx (context ctx @source-domain @user-domain-insecure)
               domain (:hypercrud.browser/domain ctx)]
           ; f is nil, seq or reagent component
           ^{:key domain}
           [domain-init! domain
            (case page-or-leaf
              ; The foundation comes with special root markup which means the foundation/view knows about page/user (not ide)
              ; Can't ide/user (not page) be part of the userland route?
              :page (r/partial page-view ctx f)
              :leaf (r/partial leaf-view ctx f))])))))

(defn confirm [message]
  #?(:clj  (throw (ex-info "confirm unsupported by platform" nil))
     :cljs (js/confirm message)))

(defn navigable? [route state]
  (and (not= route (get-in state [::runtime/partitions nil :route]))
       (or (->> (dissoc (::runtime/partitions state) nil)
                (every? (comp empty? :stage second)))
           (confirm "Unstaged work will be lost on navigate, are you sure?"))))

(def LEVEL-NONE 0)
(def LEVEL-GLOBAL-BASIS 1)
(def LEVEL-DOMAIN 2)
(def LEVEL-USER 3)
(def LEVEL-ROUTE 4)
(def LEVEL-LOCAL-BASIS 5)
(def LEVEL-HYDRATE-PAGE 6)

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
          LEVEL-USER (actions/refresh-user rt (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
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
