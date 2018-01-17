(ns hyperfiddle.ide
  (:require [hypercrud.client.core :as hc]
            [hypercrud.browser.core :as browser]
            [hypercrud.browser.routing :as routing]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.state.actions.core :as actions]
            [hypercrud.state.actions.util :as actions-util]
            [hyperfiddle.appval.domain.foundation :as foundation] ; hack
            [hyperfiddle.appval.domain.core :as foundation2]

    #?(:cljs [hypercrud.ui.navigate-cmp :as navigate-cmp])
    #?(:cljs [hypercrud.browser.browser-ui :as browser-ui])))


(def root-uri #uri "datomic:free://datomic:4334/root")      ; I don't understand this magic constant fully

(defn ide-route [route]
  {:code-database "root"
   :link-id :hyperfiddle/main
   :entity #entity["$" (:link-id route)]})

(let [always-user (atom :user)]
  (defn ide-context [ctx ide-domain target-domain target-route user-profile]
    (let [target-repo nil                                   ; from ide route query parameter, blocked on custom ide router?
          target-repo (or target-repo (:code-database target-route))
          ide-domain (foundation/process-domain-legacy ide-domain)]
      (assoc ctx
        :debug "ide"
        :display-mode always-user
        :peer (:peer-ide ctx)
        :target-domain target-domain
        :target-route target-route
        :user-profile user-profile
        :domain (let [target-source-uri (->> (:domain/code-databases target-domain)
                                             (filter #(= (:dbhole/name %) target-repo))
                                             first
                                             :dbhole/uri)]
                  (update ide-domain :domain/code-databases
                          (fn [repos]
                            (->> repos
                                 (map (fn [repo]
                                        (if (= "root" (:dbhole/name repo))
                                          (assoc-in repo [:repository/environment "$"] target-source-uri)
                                          repo)))
                                 set))))))))

(let [dispatch!-factory (fn [dispatch! action]
                          ; todo filter available actions
                          (dispatch! action)
                          nil)]
  (defn target-context [ctx domain route user-profile]
    (let [processed-domain (foundation/process-domain-legacy domain)]
      (assoc ctx
        :debug "target"
        :dispatch! (reactive/partial dispatch!-factory (:dispatch! ctx))
        :display-mode (reactive/cursor (.-state-atom (:peer ctx)) [:display-mode])
        :domain processed-domain
        :peer (:peer-user ctx)
        ; repository is needed for transact! in topnav
        :repository (->> (:domain/code-databases processed-domain)
                         (filter #(= (:dbhole/name %) (:code-database route)))
                         first
                         (into {}))
        :target-domain domain
        :target-route (if (and (= "root" (:code-database route)) ; todo "root" is a valid user string, check uri
                               (#{17592186060855 [:db/ident :hyperfiddle/main]} (:link-id route)))
                        ; hack to prevent infinite loop
                        {:code-database "root"
                         :link-id 17592186045564
                         :request-params {:entity #entity["$domains" 17592186045506]}}
                        route)
        :user-profile user-profile))))

(defn local-basis [ide-or-user target-repo global-basis domain route]
  ;local-basis-ide and local-basis-user
  (let [{:keys [domain ide user]} global-basis
        ; basis-maps: List[Map[uri, t]]
        user-basis (get user (:code-database route))
        ide-basis (get global-basis target-repo)            ; flag, unfinished
        basis-maps (case ide-or-user
                     "page" (concat [user-basis] (vals ide)) ; dead code i think?
                     "ide" (concat [ide-basis] (vals ide))
                     "user" [user-basis])
        local-basis (->> basis-maps                         ; Userland api-fn should filter irrelevant routes
                         (apply concat)
                         (apply concat)
                         (apply sorted-map))]
    #_(determine-local-basis (hydrate-route ...))
    local-basis))

(defn api [foo domain route state-val ctx]
  (let [ide-domain-request (foundation2/domain-request "hyperfiddle" (:peer ctx))
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
                                              (= (:dbhole/uri %) root-uri)))
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
   (def ^:export target-ui-context nil))                    ; its gone tho

#?(:cljs
   (defn view-page [domain route ctx]
     (let [ide-domain (hc/hydrate-api (:peer ctx) (foundation2/domain-request "hyperfiddle" (:peer ctx)))
           hide-ide (foundation2/alias? (foundation2/hostname->hf-domain-name (:hostname ctx) (:hyperfiddle-hostname ctx)))
           user-profile @(reactive/cursor (.-state-atom (:peer ctx)) [:user-profile])
           ctx (assoc ctx :navigate-cmp navigate-cmp/navigate-cmp
                          :page-on-click (reactive/partial page-on-click ctx domain))]
       [:div.hyperfiddle-ide

        (if-not hide-ide
          [browser/ui-from-route (ide-route route) (ide-context ctx ide-domain domain route user-profile)])

        (let [ctx (target-context ctx domain route user-profile)]
          ; This is different than foo=user because it is special css at root attach point
          [browser/ui-from-route route ctx "app-browser"])])))

#?(:cljs
   (defn view [foo domain route ctx]
     (let [ide-domain (hc/hydrate-api (:peer ctx) (foundation2/domain-request "hyperfiddle" (:peer ctx)))
           user-profile @(reactive/cursor (.-state-atom (:peer ctx)) [:user-profile])]
       (case foo
         "page" (view-page domain route ctx)
         ; On SSR side this is only ever called as "page", but it could be differently (e.g. turbolinks)
         ; On Browser side, also only ever called as "page", but it could be configured differently (client side render the ide, server render userland...?)
         "ide" (browser/ui-from-route route (ide-context ctx ide-domain domain route user-profile))
         "user" (browser/ui-from-route route (target-context ctx domain route user-profile))))))
