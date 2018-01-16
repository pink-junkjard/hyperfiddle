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
    #?(:cljs [hypercrud.ui.navigate-cmp :as navigate-cmp])
    #?(:cljs [hypercrud.browser.core :as browser])
    #?(:cljs [hyperfiddle.appval.domain.foundation-view :as foundation-view])))


;(def domain {})

(defn ide-route [ctx]
  ; Depends on the parsed browser location which happens inside app-ui/ui
  ; We can probably invert the logic so that we can do this from the outside.
  (let [target-user-fiddle (get-in ctx [:target-route :link-id])]
    {:code-database "root"
     :link-id :hyperfiddle/main
     :entity #entity["$" target-user-fiddle]}))

(defn- with-decoded-route [state-value f]
  (either/branch
    (if-let [state-route (:encoded-route state-value)]
      (try-either (routing/decode state-route))
      (either/right nil))
    (constantly nil)
    f))

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
        :foo {:type "ide"
              :code-database (:code-database target-route)}
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
        :foo {:type "user"}
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

(defn ^:export target-ui-context [ctx]
  (-> (target-context ctx (:target-domain ctx) (:target-route ctx) (:user-profile ctx))
      (dissoc :page-on-click)))

(defn- with-ide-ctx [maybe-decoded-route state-value ctx f]
  (let [hf-domain-request (hf/domain-request "hyperfiddle" (:peer ctx))]
    (concat
      [hf-domain-request]
      (-> (cats/sequence [(hc/hydrate (:peer ctx) hf-domain-request)])
          (either/branch
            (constantly nil)
            (fn [[target-domain hf-domain]]
              (let [decoded-route (foundation/->decoded-route maybe-decoded-route target-domain)
                    ctx (-> (ide-context ctx hf-domain target-domain decoded-route (:user-profile state-value))
                            (update :debug str "-r"))]
                (f ctx))))))))

(defn- decoded-route->user-request [target-domain maybe-decoded-route state-value ctx user-api-fn]
  (when-let [decoded-route (foundation/->decoded-route maybe-decoded-route target-domain)]
    (let [ctx (-> (target-context ctx target-domain decoded-route (:user-profile state-value))
                  (update :debug str "-r"))]
      (user-api-fn ctx))))

(defn api [domain foo state-val ctx]
  (let [browser-api-fn (fn [ctx] (browser/request-from-route (:target-route ctx) ctx))
        ; IDE is always a browser, but userland could be something different.
        userland-api-fn browser-api-fn #_ (:domain/api-fn domain)]
    (case (:type foo)
      "page" (with-decoded-route state-val #(concat
                                              (with-ide-ctx % state-val ctx (fn [ctx] (browser/request-from-route (ide-route ctx) ctx)))
                                              (decoded-route->user-request domain % state-val ctx userland-api-fn)))
      "ide" (with-decoded-route state-val #(with-ide-ctx % state-val ctx browser-api-fn))
      "user" (with-decoded-route state-val #(decoded-route->user-request domain % state-val ctx userland-api-fn)))))

(defn local-basis [foo global-basis domain route]
  ; Given all the reachable dbs, return only from this route.
  ; If hc browser, we can prune a lot.
  ; If userland is a fn, local-basis is global-basis (minus domain)
  ; The foundation takes care of the domain.

  (let [{:keys [domain ide user]} global-basis
        ; basis-maps: List[Map[uri, t]]

        ; This is browser's local-basis.

        ; userland needs to optimize the basis. 1) by running the api-fn and see what is queried. 2) configure a predicate, hyperfiddle.ide/local-basis
        ; Appfn can have local-basis, it defaults to global-basis. They'd have to code it since there's no link structure hints.

        user-basis (get user (:code-database route))        ; hardcoded browser route knowledge
        topnav-basis (get user (:code-database foo))        ; todo the only repo uri is needed from user. dont need the environment as well
        basis-maps (condp = (:type foo)
                     ; everybody using foundation has domain - even the IDE's new-anchor popover needs to do a subdomain lookup on the server side
                     "page" (concat [user-basis] (vals ide))
                     "ide" (concat [topnav-basis] (vals ide))
                     "user" [user-basis])
        local-basis (->> basis-maps                         ; Userland api-fn should filter irrelevant routes
                         (apply concat)
                         (apply concat)
                         (apply sorted-map))]

    ; Local-basis is for the subset of databases visible now for this fiddle.
    ; Does not include popovers or navigates, they have their own local basis.
    ; In the future, we can do better than reachable-basis if we hydrate and see what came out.
    #_(hydrate-route rt hostname foo state-val)
    #_(determine-local-basis)
    local-basis))

#?(:cljs
   (let [page-on-click (fn [ctx target-domain route event]
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
                             (.stopPropagation event))))]
     (defn hf-ui-context [ctx hf-domain target-domain target-route user-profile]
       (-> (ide-context ctx hf-domain target-domain target-route user-profile)
           (assoc :navigate-cmp navigate-cmp/navigate-cmp
                  :page-on-click (reactive/partial page-on-click ctx target-domain))))))


#?(:cljs
   (defn view [domain hf-domain decoded-route ctx]
     (let [ctx (-> (hf-ui-context ctx hf-domain domain decoded-route @(reactive/cursor (.-state-atom (:peer ctx)) [:user-profile]))
                   (update :debug str "-v"))]
       (browser/ui-from-route (ide-route ctx) ctx))))