(ns hyperfiddle.ide
  (:require [cats.monad.either :as either]
            [cats.core :as cats :refer [mlet]]
            [hypercrud.types.ThinEntity :refer [->ThinEntity]]
            [hypercrud.util.non-fatal :refer [try-either]]
            [hypercrud.client.core :as hc]
            [hyperfiddle.appval.domain.foundation :as foundation]
            [hypercrud.browser.core :as browser]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.browser.routing :as routing]
            [hypercrud.state.actions.core :as actions]
            [hypercrud.state.actions.util :as actions-util]
            [hyperfiddle.appval.domain.core :as hf]
    #?(:cljs [hypercrud.ui.navigate-cmp :as navigate-cmp])))


;(def domain {})

(defn ide-route [route]
  {:code-database "root"
   :link-id :hyperfiddle/main
   :entity #entity["$" (:link-id route)]})

(defn update-hf-domain [hf-domain target-domain code-database]
  (let [target-source-uri (->> (:domain/code-databases target-domain)
                               (filter #(= (:dbhole/name %) code-database))
                               first
                               :dbhole/uri)]
    (update hf-domain :domain/code-databases
            (fn [repos]
              (->> repos
                   (map (fn [repo]
                          (if (= "root" (:dbhole/name repo))
                            (assoc-in repo [:repository/environment "$"] target-source-uri)
                            repo)))
                   set)))))

(let [always-user (atom :user)]
  (defn ide-context [ctx hf-domain target-domain target-route user-profile]
    (let [hf-domain (foundation/process-domain-legacy hf-domain)]
      (assoc ctx
        :debug "hf"
        :display-mode always-user
        :domain (update-hf-domain hf-domain target-domain (or (:code-database (:foo ctx)) ; you aren't crazy, this is weird af
                                                              (:code-database target-route)))
        :peer (:peer-ide ctx)
        :foo {:code-database (:code-database target-route)}
        :target-domain target-domain
        :target-route target-route
        :user-profile user-profile))))

(let [dispatch!-factory (fn [dispatch! action]
                          ; todo filter available actions
                          (dispatch! action)
                          nil)]
  (defn target-context [ctx target-domain target-route user-profile]
    (let [processed-domain (foundation/process-domain-legacy target-domain)]
      (assoc ctx
        :debug "target"
        :dispatch! (reactive/partial dispatch!-factory (:dispatch! ctx))
        :display-mode (reactive/cursor (.-state-atom (:peer ctx)) [:display-mode])
        :domain processed-domain
        :peer (:peer-user ctx)
        ; repository is needed for transact! in topnav
        ; repository is illegal as well, it should just be domain/databases
        :repository (->> (:domain/code-databases processed-domain)
                         (filter #(= (:dbhole/name %) (:code-database target-route)))
                         first
                         (into {}))
        :target-domain target-domain
        :target-route (if (and (= "root" (:code-database target-route)) ; todo "root" is a valid user string, check uri
                               (#{17592186060855 [:db/ident :hyperfiddle/main]} (:link-id target-route)))
                        ; hack to prevent infinite loop
                        {:code-database "root"
                         :link-id 17592186045564
                         :request-params {:entity (->ThinEntity "$domains" 17592186045506)}}
                        target-route)
        :user-profile user-profile))))

(defn local-basis [foo global-basis domain route]
  (let [{:keys [domain ide user]} global-basis
        ; basis-maps: List[Map[uri, t]]
        user-basis (get user (:code-database route))
        topnav-basis (Get user (:code-database foo))        ; todo the only repo uri is needed from user. dont need the environment as well
        basis-maps (case foo
                     "page" (concat [user-basis] (vals ide))
                     "ide" (concat [topnav-basis] (vals ide))
                     "user" [user-basis])
        local-basis (->> basis-maps                         ; Userland api-fn should filter irrelevant routes
                         (apply concat)
                         (apply concat)
                         (apply sorted-map))]
    #_(determine-local-basis (hydrate-route ...))
    local-basis))

(defn api [foo domain route state-val ctx]
  (let [ide-domain-request (hf/domain-request "hyperfiddle" (:peer ctx))
        ide-domain (hc/hydrate-api (:peer ctx) ide-domain-request)
        user-profile (:user-profile state-val)]
    (case foo
      "page" (concat [ide-domain-request]
                     (browser/request-from-route (ide-route route) (ide-context ctx ide-domain domain route user-profile)))
      "ide" (concat [ide-domain-request]
                    (browser/request-from-route route (ide-context ctx ide-domain domain route user-profile)))
      "user" (browser/request-from-route route (target-context ctx domain route user-profile)))))

#?(:cljs
   (defn page-on-click [ctx target-domain route event]
     (when (and route (.-altKey event))
       (let [can-soft-nav? (->> (:domain/code-databases target-domain)
                                ; only if the user domain has the root code-database
                                (filter #(and (= (:dbhole/name %) "root")
                                              (= (:dbhole/uri %) hf/root-uri)))
                                (empty?)
                                not)]
         (if can-soft-nav?
           ((:dispatch! ctx) (fn [dispatch! get-state]
                               (let [encoded-route (routing/encode route)]
                                 (when (actions-util/navigable? encoded-route (get-state))
                                   (actions/set-route (:peer ctx) encoded-route dispatch! get-state)))))
           (let [encoded-route (routing/encode route (str "hyperfiddle." (:hyperfiddle-hostname ctx)))]
             ; todo push this window.location set up to the appfn atom watcher
             (aset js/window "location" encoded-route)))
         (.stopPropagation event)))))

#?(:cljs
   (defn view [foo domain route ctx]
     (let [ide-domain-request (hf/domain-request "hyperfiddle" (:peer ctx))
           ide-domain (hc/hydrate-api (:peer ctx) ide-domain-request)
           user-profile @(reactive/cursor (.-state-atom (:peer ctx)) [:user-profile])]
       (case foo
         "page" (let [ctx (-> (ide-context ctx ide-domain domain route user-profile)
                              (assoc :navigate-cmp navigate-cmp/navigate-cmp
                                     :page-on-click (reactive/partial page-on-click ctx domain)))]
                  (browser/ui-from-route (ide-route route) ctx))
         ; On SSR side this is only ever called as "page", but it could be differently (e.g. turbolinks)
         ; On Browser side, also only ever called as "page", but it could be configured differently (client side render the ide, server render userland...?)
         "ide" (browser/ui-from-route route (ide-context ctx ide-domain domain route user-profile))
         "user" (browser/ui-from-route route (target-context ctx domain route user-profile))))))

(declare ^:export target-ui-context nil)                    ; its gone tho