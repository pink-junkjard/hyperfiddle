(ns hyperfiddle.ide
  (:require [cuerdas.core :as str]
            [hypercrud.browser.core :as browser]
            [hypercrud.browser.routing :as routing]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.core :as hc]
    #?(:cljs [hypercrud.react.react-fragment :refer [react-fragment]])
    #?(:cljs [hypercrud.ui.navigate-cmp :as navigate-cmp])
            [hypercrud.util.core :refer [unwrap]]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.util.string :as hc-string]
            [hyperfiddle.foundation :as foundation]
            [taoensso.timbre :as timbre]

    ; pull in public ui deps
    ; todo these hc.ui.* should be reduced to one require e.g. [hypercrud.ui]
    #?(:cljs [hypercrud.react.react-fragment])
            [hypercrud.ui.auto-control]
    #?(:cljs [hypercrud.ui.result])

    ; pull in the entire ide app for reference from user-land
            [hyperfiddle.ide.actions]
            [hyperfiddle.ide.fiddles.domain-code-database]
            [hyperfiddle.ide.fiddles.fiddle-links.bindings]
    #?(:cljs [hyperfiddle.ide.fiddles.fiddle-links.renderer])
    #?(:cljs [hyperfiddle.ide.fiddles.main])
            [hyperfiddle.ide.fiddles.topnav]
    #?(:cljs [hyperfiddle.ide.fiddles.user-dashboard])
            [hyperfiddle.ide.util]))


(defn ide-route [route]
  {:code-database "root"
   :link-id :hyperfiddle/main
   :entity #entity["$" (:link-id route)]})

(defprotocol SplitRuntime
  (sub-rt [rt foo target-repo])
  (target-repo [rt]))

(def -sub-rt (memoize sub-rt))

(let [always-user (atom :user)
      constantly-nil (constantly nil)]
  ; ide is overloaded, these ide-context functions are exclusive to (top)
  ; despite being in the namespace (hyperfiddle.ide) which encompasses the union of target/user (bottom) and ide (top)
  (defn- *-ide-context [ctx ide-domain target-domain ?user-profile]
    {:pre [ide-domain target-domain (seq (-> target-domain :domain/code-databases))]
     :post [(seq (-> % :hypercrud.browser/domain :domain/code-databases))]}
    (assoc ctx
      :hypercrud.browser/debug "ide"
      :hypercrud.browser/domain (let [target-source-uri (->> (:domain/code-databases target-domain)
                                                             (filter #(= (:dbhole/name %) (target-repo (:peer ctx))))
                                                             first
                                                             :dbhole/uri)]
                                  (-> (foundation/process-domain-legacy ide-domain)
                                      (update :domain/code-databases
                                              (fn [repos]
                                                (->> repos
                                                     (map (fn [repo]
                                                            (if (= "root" (:dbhole/name repo))
                                                              (assoc-in repo [:repository/environment "$"] target-source-uri)
                                                              repo)))
                                                     set)))))
      :hypercrud.browser/page-on-click constantly-nil       ; disable alt-nav up top
      :hypercrud.ui/display-mode always-user
      :target-domain target-domain                          ; todo rename :target-domain to :hyperfiddle.ide/target-domain
      :user-profile ?user-profile)))

(defn leaf-ide-context [ctx ide-domain target-domain ?user-profile]
  ; ide leaf-context does not have enough information to set hyperfiddle.ide/target-route
  ; this means ide popovers CANNOT access it
  (*-ide-context ctx ide-domain target-domain ?user-profile))

(defn page-ide-context [ctx ide-domain target-domain target-route ?user-profile]
  {:pre [target-route]}
  (-> (update ctx :peer #(-sub-rt % "ide" (:code-database target-route)))
      ; hyperfiddle.ide/target-route is ONLY available to inlined IDE (no deferred popovers)
      (assoc :target-route target-route)                    ; todo rename :target-route to :hyperfiddle.ide/target-route
      (*-ide-context ide-domain target-domain ?user-profile)))

(def activate-ide? (complement foundation/alias?))

(let [dispatch!-factory (fn [dispatch! action]
                          ; todo filter available actions
                          (dispatch! action)
                          nil)]
  (defn- *-target-context [ctx domain route user-profile]
    {:pre [domain (seq (-> domain :domain/code-databases))]
     :post [(seq (-> % :hypercrud.browser/domain :domain/code-databases))]}
    (assoc ctx
      :hypercrud.browser/debug "target"
      :hypercrud.browser/domain (foundation/process-domain-legacy domain)
      :hypercrud.ui/display-mode (reactive/cursor (.-state-atom (:peer ctx)) [:display-mode])
      :dispatch! (reactive/partial dispatch!-factory (:dispatch! ctx))
      :ide-active (activate-ide? (foundation/hostname->hf-domain-name ctx))
      :user-profile user-profile

      ; these target values only exists to allow the topnav to render in the bottom/user
      ; IF we MUST to support that, this should probably be done higher up for both ide and user at the same time
      ; and SHOULD ONLY be applied for ide within ide (other user fns don't get access to these values)
      :target-domain domain                                 ; todo rename :target-domain to :hyperfiddle.ide/target-domain
      :target-route route                                   ; todo rename :target-route to :hyperfiddle.ide/target-route
      )))

(defn leaf-target-context [ctx domain route user-profile]
  (*-target-context ctx domain route user-profile))

(defn page-target-context [ctx domain route user-profile]
  (-> (update ctx :peer #(-sub-rt % "user" nil))
      (*-target-context domain route user-profile)))

(defn canonical-route [domain route]
  {:pre [domain (string? route)]
   :post [(string? %)]}
  (case route
    "/" (if-let [home-route (:domain/home-route domain)]
          (routing/encode (unwrap (hc-string/safe-read-edn-string home-route)))
          "/" #_"no home-route, maybe a brand new domain with no fiddles")
    route))

(defn local-basis [ide-or-user global-basis -domain #_"offensive" route ctx]
  ;local-basis-ide and local-basis-user
  (let [route (routing/decode (canonical-route -domain route))
        {:keys [domain ide user]} global-basis
        ; basis-maps: List[Map[uri, t]]
        user-basis (get user (:code-database route))
        ide-basis (get ide (target-repo (:peer ctx)))
        basis-maps (case ide-or-user
                     "page" (concat (vals user) #_"for schema in topnav"
                                    (vals ide))             ; dead code i think?
                     "ide" (concat [ide-basis] (vals ide) (vals user))
                     "user" [user-basis])
        local-basis (->> basis-maps                         ; Userland api-fn should filter irrelevant routes
                         (apply concat)
                         ;(apply concat)
                         sort
                         ;(apply sorted-map)
                         )]
    (timbre/debug (pr-str local-basis))
    #_(determine-local-basis (hydrate-route route ...))
    local-basis))

; Domain can be removed with a double foundation
; Reactive pattern obfuscates params
; Foo can be ignored for now
(defn api [foo -domain route ctx]
  {:pre [-domain route
         (str/starts-with? route "/")]
   :post [#_(seq %)]}
  ; We can actually call into the foundation a second time here to get the ide-domain
  (let [ide-domain-q (foundation/domain-request "hyperfiddle" (:peer ctx))
        ide-domain (hc/hydrate-api (:peer ctx) ide-domain-q)
        user-profile @(reactive/cursor (.-state-atom (:peer ctx)) [:user-profile])]
    ; This route over-applies canonical but it works out and really simplifies the validation
    (let [?route (routing/decode (canonical-route -domain route))]
      (case foo
        "page" (concat [ide-domain-q]
                       (if (and ?route -domain)
                         (browser/request-from-route ?route (page-target-context ctx -domain ?route user-profile)))
                       (if (and (activate-ide? (foundation/hostname->hf-domain-name ctx)) ide-domain)
                         (browser/request-from-route (ide-route ?route) (page-ide-context ctx ide-domain -domain ?route user-profile))))
        "ide" (concat [ide-domain-q]
                      (if ide-domain
                        (browser/request-from-route ?route (leaf-ide-context ctx ide-domain -domain user-profile))))
        "user" (if (and ?route -domain)
                 (browser/request-from-route ?route (leaf-target-context ctx -domain ?route user-profile)))))))

#?(:cljs
   (defn view-page [-domain ?route ctx]
     (let [ide-domain (hc/hydrate-api (:peer ctx) (foundation/domain-request "hyperfiddle" (:peer ctx)))
           ide-active (activate-ide? (foundation/hostname->hf-domain-name ctx))
           user-profile @(reactive/cursor (.-state-atom (:peer ctx)) [:user-profile])
           ctx (assoc ctx :navigate-cmp navigate-cmp/navigate-cmp)]
       (react-fragment
         :view-page
         (if ide-active
           (if ide-domain
             [browser/ui-from-route (ide-route ?route) (page-ide-context ctx ide-domain -domain ?route user-profile) "hyperfiddle-ide-topnav"]
             [:div "loading... (ide bootstrap, you edited ide-domain)"]))
         (if ?route
           ; This is different than foo=user because it is special css at root attach point
           [browser/ui-from-route ?route (page-target-context ctx -domain ?route user-profile) "hyperfiddle-ide-user"])))))

#?(:cljs
   ; Route is managed by the domain; Domain will not be available here soon.
   (defn view [foo -domain route ctx]                       ; pass most as ref for reactions
     (let [route (routing/decode (canonical-route -domain route))
           ide-domain (hc/hydrate-api (:peer ctx) (foundation/domain-request "hyperfiddle" (:peer ctx)))
           user-profile @(reactive/cursor (.-state-atom (:peer ctx)) [:user-profile])]
       (case foo
         "page" (view-page -domain route ctx)               ; component, seq-component or nil
         ; On SSR side this is only ever called as "page", but it could be differently (e.g. turbolinks)
         ; On Browser side, also only ever called as "page", but it could be configured differently (client side render the ide, server render userland...?)
         "ide" [browser/ui-from-route route (leaf-ide-context ctx ide-domain -domain user-profile)]
         "user" [browser/ui-from-route route (leaf-target-context ctx -domain route user-profile)]))))
