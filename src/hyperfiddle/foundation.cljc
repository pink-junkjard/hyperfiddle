(ns hyperfiddle.foundation
  (:refer-clojure :exclude [read-string])
  (:require [cats.monad.either :as either :refer [branch left right]]
            [clojure.string :as string]
    #?(:cljs [contrib.css :refer [classes]])
            [contrib.data :refer [update-existing]]
            [contrib.eval :as eval]
            [contrib.reactive :as r]
            [contrib.reader :refer [read-string read-edn-string]]
            [contrib.string :refer [pprint-str]]
            [cuerdas.core :as str]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.core :as hc]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.Err :as Err]
    #?(:cljs [hypercrud.ui.control.code :refer [code*]])
    #?(:cljs [hypercrud.ui.stale :as stale])
            [hyperfiddle.foundation.actions :as foundation-actions]
            [hyperfiddle.runtime :as runtime]
            [promesa.core :as p]))


(def domain-uri #uri "datomic:free://datomic:4334/domains")
(def auth0-redirect-path "/auth0")                          ; ide

(defn hostname->hf-domain-name
  ([ctx]
   (hostname->hf-domain-name (:hostname ctx) (:hyperfiddle-hostname ctx)))
  ([hostname hyperfiddle-hostname]
   (-> (string/replace hostname (str "." hyperfiddle-hostname) "") ; buggy
       #_cuerdas/slug                                       ; no unicode for now. Slug what we actually got, so we don't care what's in the database.
       )))

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
                      :domain/code
                      :domain/css                           ; domain-css
                      :domain/disable-javascript
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
  (-> user-profile :email (str/replace #"\@.+$" "") (str/slug)))

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
      [:fieldset [:legend "(ex-data e)"]                    ; network error
       [:pre (:data e)]]                                    ; includes :body key
      [:fieldset [:legend "(.-stack e)"]                    ; network error
       [:pre (.-stack e)]]])])

(defn process-domain [domain]
  (-> (into {} domain) (update-existing :domain/environment read-string) #_"todo this can throw"))

(defn context [ctx source-domain]
  (let [domain @(runtime/state (:peer ctx) [::runtime/domain])
        _ (assert domain "Bootstrapping failed to fetch the domain")
        domain (process-domain domain)]
    (assoc ctx
      :hypercrud.browser/domain domain
      :hypercrud.browser/invert-route (r/partial routing/invert-route domain)
      :hypercrud.browser/source-domain (process-domain source-domain))))

(defn local-basis [page-or-leaf global-basis route ctx f]
  (concat
    (:domain global-basis)
    (f global-basis route ctx)))

(defn api [page-or-leaf route ctx f]
  (let [source-domain-req (domain-request "hyperfiddle" (:peer ctx))]
    (into [source-domain-req]
          (when-let [source-domain (hc/hydrate-api (:peer ctx) (:branch ctx) source-domain-req)]
            (let [ctx (context ctx source-domain)]
              (f route ctx))))))

#?(:cljs
   (defn staging [peer]
     (let [stage-val @(runtime/state peer [:stage])
           edn (pprint-str stage-val 70)]
       ; todo this can throw
       [code* edn #(runtime/dispatch! peer (foundation-actions/reset-stage peer (read-edn-string %)))])))

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
     [:div {:class (apply classes "hyperfiddle-foundation" @(runtime/state (:peer ctx) [:pressed-keys]))}
      [:style {:dangerouslySetInnerHTML {:__html (:domain/css (:hypercrud.browser/domain ctx))}}]
      (f route ctx)                                         ; nil, seq or reagent component
      (if @(runtime/state (:peer ctx) [:staging-open])
        [staging (:peer ctx)])]))

#?(:cljs
   (defn page-or-leaf1 [page-or-leaf route ctx f]
     (case page-or-leaf
       ; The foundation comes with special root markup which means the foundation/view knows about page/user (not ide)
       ; Can't ide/user (not page) be part of the userland route?
       :page (page-view route ctx f)
       :leaf (leaf-view route ctx f))))

#?(:cljs
   (defn view [page-or-leaf route ctx f]
     (let [source-domain @(hc/hydrate (:peer ctx) (:branch ctx) (domain-request "hyperfiddle" (:peer ctx)))]
       (if-let [e (or @(runtime/state (:peer ctx) [::runtime/fatal-error])
                      (when (either/left? source-domain) @source-domain))]
         [:div.hyperfiddle-foundation
          [error-cmp e]
          [staging (:peer ctx)]]
         (let [ctx (context ctx @source-domain)
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
          LEVEL-GLOBAL-BASIS (foundation-actions/refresh-global-basis rt (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
          LEVEL-DOMAIN (foundation-actions/refresh-domain rt (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
          LEVEL-ROUTE (let [branch-aux {:hyperfiddle.ide/foo "page"}] ;ide
                        (try (let [route (runtime/decode-route rt encoded-route)]
                               (when-let [e (routing/invalid-route? route)] (throw e))
                               (runtime/dispatch! rt [:add-partition nil route branch-aux]))
                             (p/resolved nil)
                             (catch #?(:cljs :default :clj Exception) e
                               (runtime/dispatch! rt [:set-error e])
                               (p/rejected e))))
          LEVEL-LOCAL-BASIS (foundation-actions/refresh-partition-basis rt nil (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
          LEVEL-HYDRATE-PAGE (if (or (not= initial-global-basis @(runtime/state rt [::runtime/global-basis])) dirty-stage?)
                               (foundation-actions/hydrate-partition rt nil nil (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
                               (p/resolved nil)))
        (p/then #(bootstrap-data rt (inc init-level) load-level encoded-route initial-global-basis dirty-stage?)))))
