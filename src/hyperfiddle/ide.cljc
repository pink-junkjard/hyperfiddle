(ns hyperfiddle.ide
  (:require [hypercrud.client.core :as hc]
            [cuerdas.core :as str]
            [hypercrud.browser.core :as browser]
            [hypercrud.browser.routing :as routing]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.state.actions.core :as actions]
            [hypercrud.state.actions.util :as actions-util]
            [hyperfiddle.appval.domain.foundation :as foundation] ; hack
            [hyperfiddle.appval.domain.core :as foundation2]
            [hypercrud.util.core :refer [unwrap]]
            [hypercrud.browser.routing :as routing]
            [hypercrud.util.string :as hc-string]
            [taoensso.timbre :as timbre]

    #?(:cljs [hypercrud.ui.navigate-cmp :as navigate-cmp])
    #?(:cljs [hypercrud.browser.browser-ui :as browser-ui])))


(def root-uri #uri "datomic:free://datomic:4334/root")      ; I don't understand this magic constant fully

(defn ide-route [route]
  {:code-database "root"
   :link-id :hyperfiddle/main
   :entity #entity["$" (:link-id route)]})

(defprotocol SplitRuntime
  (sub-rt [rt foo ide-repo]))

(def -sub-rt (memoize sub-rt))

(let [always-user (atom :user)]
  (defn ide-context [ctx ide-domain target-domain ide-route target-route user-profile]
    (let [ide-domain (foundation/process-domain-legacy ide-domain)
          peer (if (= "ide" (.-foo (:peer ctx)))
                 (:peer ctx)
                 ; Converting from page -> ide, means we're an ide at the root with a code-database.
                 (-sub-rt (:peer ctx) "ide" (:code-database target-route)))]
      (assoc ctx
        :debug "ide"
        :display-mode always-user
        :peer peer
        :target-domain target-domain
        :target-route target-route
        :user-profile user-profile
        :domain (let [target-source-uri (->> (:domain/code-databases target-domain)
                                             (filter #(= (:dbhole/name %) (.-ide-repo peer)))
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
    {:post [(:repository %) (-> % :repository :dbhole/uri)]}
    (let [processed-domain (foundation/process-domain-legacy domain)
          peer (-sub-rt (:peer ctx) "user" nil)]
      (assoc ctx
        :debug "target"
        :dispatch! (reactive/partial dispatch!-factory (:dispatch! ctx))
        :display-mode (reactive/cursor (.-state-atom (:peer ctx)) [:display-mode])
        :domain processed-domain
        :peer peer
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

(defn canonical-route [domain route]
  {:pre [domain (string? route)]
   :post [(string? %)]}
  (case route
    "/" (routing/encode (unwrap (hc-string/safe-read-edn-string (:domain/home-route domain))))
    route))

(defn local-basis [ide-or-user global-basis -domain route ctx]
  ;local-basis-ide and local-basis-user
  (let [target-repo nil                                     ; figure it out from custom router
        {:keys [domain ide user]} global-basis
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
    #_(determine-local-basis (hydrate-route route ...))
    local-basis))

; Domain can be removed with a double foundation
; Reactive pattern obfuscates params
; Foo can be ignored for now
(defn api [foo -domain route ctx]
  {:pre [route (str/starts-with? route "/")]
   :post [(seq %)]}
  ; We can actually call into the foundation a second time here to get the ide-domain
  (let [ide-domain-q (foundation2/domain-request "hyperfiddle" (:peer ctx))
        ide-domain (hc/hydrate-api (:peer ctx) ide-domain-q)
        user-profile @(reactive/cursor (.-state-atom (:peer ctx)) [:user-profile])

        user-fiddle-qs (fn [route] (if -domain (browser/request-from-route route (target-context ctx -domain route user-profile))))
        ide-fiddle-qs (fn [route] (if ide-domain (browser/request-from-route route (ide-context ctx ide-domain -domain route nil user-profile))))
        ide-root-qs (fn [route] (if ide-domain (browser/request-from-route (ide-route route) (ide-context ctx ide-domain -domain nil route user-profile))))]

    ; This route over-applies canonical but it works out and really simplifies the validation
    (if-let [route (routing/decode (canonical-route -domain route))]
      (case foo
        "page" (concat [ide-domain-q] (user-fiddle-qs route) (ide-root-qs route))
        "ide" (concat [ide-domain-q] (ide-fiddle-qs route))
        "user" (concat (user-fiddle-qs route)))
      (do (timbre/warn "hyperfiddle.ide: invalid route" route)
          #{} #_"No mechanism to propogate the error yet"))))

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
   (defn view-page [-domain route target-repo ctx]
     (let [ide-domain (hc/hydrate-api (:peer ctx) (foundation2/domain-request "hyperfiddle" (:peer ctx)))
           hide-ide (foundation2/alias? (foundation2/hostname->hf-domain-name (:hostname ctx) (:hyperfiddle-hostname ctx)))
           user-profile @(reactive/cursor (.-state-atom (:peer ctx)) [:user-profile])
           ctx (assoc ctx :navigate-cmp navigate-cmp/navigate-cmp
                          :page-on-click (reactive/partial page-on-click ctx -domain))]
       [:div.hyperfiddle-ide
        (if-not hide-ide
          [browser/ui-from-route (ide-route route) (ide-context ctx ide-domain -domain route target-repo user-profile)])
        ; This is different than foo=user because it is special css at root attach point
        [browser/ui-from-route route (target-context ctx -domain route user-profile) "app-browser"]])))

#?(:cljs
   ; Route is managed by the domain; Domain will not be available here soon.
   (defn view [foo -domain route ctx]                       ; pass most as ref for reactions
     (let [target-repo nil                                  ; figure it out! custom router, or nil flag
           ide-domain (hc/hydrate-api (:peer ctx) (foundation2/domain-request "hyperfiddle" (:peer ctx)))
           route (routing/decode route)
           user-profile @(reactive/cursor (.-state-atom (:peer ctx)) [:user-profile])]
       (case foo
         "page" [view-page -domain (canonical-route -domain route) target-repo ctx]
         ; On SSR side this is only ever called as "page", but it could be differently (e.g. turbolinks)
         ; On Browser side, also only ever called as "page", but it could be configured differently (client side render the ide, server render userland...?)
         "ide" [browser/ui-from-route route (ide-context ctx ide-domain -domain route target-repo user-profile)]
         "user" [browser/ui-from-route route (target-context ctx -domain route user-profile)]))))
