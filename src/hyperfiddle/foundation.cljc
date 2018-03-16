(ns hyperfiddle.foundation
  (:require [clojure.string :as string]
            [cuerdas.core :as cuerdas]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.core :as hc]
            [hypercrud.compile.reader :as reader]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
    #?(:cljs [hypercrud.ui.control.code :refer [code*]])
    #?(:cljs [hypercrud.ui.css :refer [classes]])
    #?(:cljs [hypercrud.ui.stale :as stale])
            [hypercrud.util.core :as util :refer [update-existing]]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.foundation.actions :as foundation-actions]
            [hyperfiddle.runtime :as runtime]
            [promesa.core :as p]))


(def domain-uri #uri "datomic:free://datomic:4334/domains")
(def auth0-redirect-path "/auth0")                          ; ide

(defn hostname->hf-domain-name
  ([ctx]
   (hostname->hf-domain-name (:hostname ctx) (:hyperfiddle-hostname ctx)))
  ([hostname hyperfiddle-hostname]
    ; buggy
   (string/replace hostname (str "." hyperfiddle-hostname) "")))

(defn alias? [hf-domain-name]
  ; 'dustingetzcom.hyperfiddle.net' 'www.hyperfiddle.net'
  ; 'dustingetzcom', 'www', 'hyperfiddle'
  (string/includes? hf-domain-name "."))

(defn domain-request [hf-domain-name peer]
  (let [e (if (alias? hf-domain-name)
            [:domain/aliases hf-domain-name]
            [:domain/ident hf-domain-name])]
    (->EntityRequest e nil
                     (hc/db peer domain-uri nil)
                     [:db/id
                      :domain/aliases
                      :domain/environment
                      :domain/fiddle-repo
                      :domain/home-route
                      :domain/members
                      :domain/ident
                      :domain/router
                      :user/sub])))

(defn domain-owner? [user-profile domain]
  (let [sub (:sub user-profile)]
    (or (some-> (:user/sub domain) (= sub))
        (contains? (set (:domain/members domain)) sub))))

(defn user-profile->ident [user-profile]
  (-> user-profile :email (cuerdas/replace #"\@.+$" "") (cuerdas/slug)))

(defn error-cmp [e]
  [:div
   [:h1 "Fatal error"]
   [:fieldset [:legend "(pr-str e)"]
    [:pre (pr-str e)]]
   [:fieldset [:legend "(ex-data e)"]                       ; network error
    [:pre (pr-str (ex-data e))]]                            ; includes :body key
   [:fieldset [:legend "(.-stack e)"]                       ; network error
    [:pre (.-stack e)]]])

(defn process-domain [domain]
  (-> (into {} domain) (update-existing :domain/environment reader/read-string) #_"todo this can throw"))

(defn context [ctx route]
  (let [domain @(runtime/state (:peer ctx) [::runtime/domain])
        _ (assert domain "Bootstrapping failed to fetch the domain")
        domain (process-domain domain)]
    (assoc ctx
      :hypercrud.browser/domain domain
      :hypercrud.browser/invert-route (reactive/partial routing/invert-route domain))))

(defn local-basis [page-or-leaf global-basis route ctx f]
  (concat
    (:domain global-basis)
    (f global-basis route (context ctx route))))

(defn api [page-or-leaf route ctx f]
  (f route (context ctx route)))

#?(:cljs
   (defn staging [peer]
     (let [stage-val @(runtime/state peer [:stage])
           edn (util/pprint-str stage-val 70)]
       ; todo this can throw
       [code* edn #(runtime/dispatch! peer (foundation-actions/reset-stage peer (reader/read-edn-string %)))])))

#?(:cljs
   (defn leaf-view [route ctx f]
     ; A malformed stage can break bootstrap hydrates, but the root-page is bust, so ignore here
     ; Fix this by branching userland so bootstrap is sheltered from staging area? (There are chickens and eggs)
     (f route (context ctx route))))

#?(:cljs
   (defn page-view [route ctx f]
     (if-let [e @(runtime/state (:peer ctx) [::runtime/fatal-error])]
       [:div.hyperfiddle-foundation
        [error-cmp e]
        [staging (:peer ctx)]]

       (let [ctx (context ctx route)]
         [:div {:class (apply classes "hyperfiddle-foundation" @(runtime/state (:peer ctx) [:pressed-keys]))}
          (f route ctx)                                     ; nil, seq or reagent component
          (if @(runtime/state (:peer ctx) [:staging-open])
            [staging (:peer ctx)])]))))

#?(:cljs
   (defn view [page-or-leaf route ctx f]
     (case page-or-leaf
       ; The foundation comes with special root markup which means the foundation/view knows about page/user (not ide)
       ; Can't ide/user (not page) be part of the userland route?
       :page (page-view route ctx f)
       :leaf (leaf-view route ctx f))))

(defn confirm [message]
  #?(:clj  (throw (ex-info "confirm unsupported by platform" nil))
     :cljs (js/confirm message)))

(defn navigable? [route {:keys [encoded-route ::runtime/partitions] :as state}]
  (and (not= route encoded-route)
       (or (empty? (dissoc partitions nil))
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
(defn bootstrap-data [rt init-level load-level encoded-route] ;branch and aux as parameter?
  (if (>= init-level load-level)
    (p/resolved nil)
    (-> (condp = (inc init-level)
          LEVEL-GLOBAL-BASIS (foundation-actions/refresh-global-basis rt (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
          LEVEL-DOMAIN (foundation-actions/refresh-domain rt (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
          LEVEL-ROUTE (let [branch-aux {:hyperfiddle.ide/foo "page"}] ;ide
                        (p/resolved (runtime/dispatch! rt [:add-partition nil (runtime/decode-route rt encoded-route) branch-aux])))
          LEVEL-LOCAL-BASIS (foundation-actions/refresh-partition-basis rt nil (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
          LEVEL-HYDRATE-PAGE (foundation-actions/hydrate-partition rt nil nil (partial runtime/dispatch! rt) #(deref (runtime/state rt))))
        (p/then #(bootstrap-data rt (inc init-level) load-level encoded-route)))))
