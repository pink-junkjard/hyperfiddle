(ns hyperfiddle.ide
  (:require [cuerdas.core :as str]
    #?(:cljs [hypercrud.browser.browser-ui :as browser-ui])
            [hypercrud.browser.core :as browser]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.core :as hc]
    #?(:cljs [hypercrud.react.react-fragment :refer [react-fragment]])
    #?(:cljs [hypercrud.ui.navigate-cmp :as navigate-cmp])
            [hypercrud.util.core :refer [unwrap xorxs update-existing]]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.util.string :as hc-string]
            [hyperfiddle.foundation :as foundation]
            [hyperfiddle.ide-rt :as ide-rt]
            [hyperfiddle.runtime :as runtime]
    #?(:cljs [reagent.core :as reagent])
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
            [hyperfiddle.ide.fiddles.topnav]
    #?(:cljs [hyperfiddle.ide.fiddles.user-dashboard])
            [hyperfiddle.ide.util]))


(defn ide-route [route]
  {:code-database "root"
   :fiddle-id :hyperfiddle/topnav
   :request-params [#entity["$" (:fiddle-id route)]]})

(def -sub-rt (memoize ide-rt/sub-rt))

(let [always-user (atom :user)
      constantly-nil (constantly nil)]
  ; ide is overloaded, these ide-context functions are exclusive to (top)
  ; despite being in the namespace (hyperfiddle.ide) which encompasses the union of target/user (bottom) and ide (top)
  (defn- *-ide-context [ctx ide-domain ?user-profile]
    {:pre [ide-domain]
     :post [(seq (-> % :hypercrud.browser/domain :domain/code-databases))]}
    (-> ctx
        (assoc :hypercrud.browser/debug "ide"
               :hypercrud.browser/page-on-click constantly-nil ; disable alt-nav up top
               :hypercrud.ui/display-mode always-user
               :target-domain (:hypercrud.browser/domain ctx) ; todo rename :target-domain to :hyperfiddle.ide/target-domain
               :user-profile ?user-profile)
        (update :hypercrud.browser/domain
                (fn [domain]
                  (let [target-source-uri (->> (:domain/code-databases domain)
                                               (filter #(= (:dbhole/name %) (ide-rt/target-repo (:peer ctx))))
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
                                       set))))))))))

(defn leaf-ide-context [ctx ide-domain ?user-profile]
  ; ide leaf-context does not have enough information to set hyperfiddle.ide/target-route
  ; this means ide popovers CANNOT access it
  (*-ide-context ctx ide-domain ?user-profile))

(defn page-ide-context [ctx ide-domain target-route ?user-profile]
  {:pre [target-route]}
  (-> (update ctx :peer #(-sub-rt % "ide" (:code-database target-route)))
      ; hyperfiddle.ide/target-route is ONLY available to inlined IDE (no deferred popovers)
      (assoc :target-route target-route)                    ; todo rename :target-route to :hyperfiddle.ide/target-route
      (*-ide-context ide-domain ?user-profile)))

(def activate-ide? (complement foundation/alias?))

(let [dispatch!-factory (fn [dispatch! action]
                          ; todo filter available actions
                          (dispatch! action)
                          nil)]
  (defn- *-target-context [ctx route user-profile]
    (assoc ctx
      :hypercrud.browser/debug "target"
      :hypercrud.ui/display-mode (reactive/cursor (.-state-atom (:peer ctx)) [:display-mode])
      :dispatch! (reactive/partial dispatch!-factory (:dispatch! ctx))
      :ide-active (activate-ide? (foundation/hostname->hf-domain-name ctx))
      :user-profile user-profile

      ; these target values only exists to allow the topnav to render in the bottom/user
      ; IF we MUST to support that, this should probably be done higher up for both ide and user at the same time
      ; and SHOULD ONLY be applied for ide within ide (other user fns don't get access to these values)
      :target-domain (:hypercrud.browser/domain ctx)        ; todo rename :target-domain to :hyperfiddle.ide/target-domain
      :target-route route                                   ; todo rename :target-route to :hyperfiddle.ide/target-route
      )))

(defn leaf-target-context [ctx route user-profile]
  (*-target-context ctx route user-profile))

(defn page-target-context [ctx route user-profile]
  (-> (update ctx :peer #(-sub-rt % "user" nil))
      (*-target-context route user-profile)))

(defn route-decode [domain s]
  {:pre [domain (string? s)]}
  (case s
    "/" (unwrap (hc-string/safe-read-edn-string (:domain/home-route domain)))

    (routing/decode s)))

(defn route-encode [domain route]
  ; use domain to canonicalize
  (routing/encode route))

(defn local-basis [foo global-basis route ctx]
  ;local-basis-ide and local-basis-user
  (let [{:keys [domain ide user]} global-basis
        ; basis-maps: List[Map[uri, t]]
        user-basis (get user (:code-database route))
        ide-basis (get ide (ide-rt/target-repo (:peer ctx))) ; Why don't we call ide-route-decode? I guess we don't care?
        basis-maps (case foo
                     "page" (concat (vals user) #_"for schema in topnav"
                                    (vals ide))             ; dead code i think?
                     "ide" (concat [ide-basis] (vals ide) (vals user))
                     "user" [user-basis])
        local-basis (->> basis-maps (apply concat) sort)]   ; Userland api-fn should filter irrelevant routes
    (timbre/debug (pr-str local-basis))
    #_(determine-local-basis (hydrate-route route ...))
    local-basis))

; Reactive pattern obfuscates params
; Foo can be ignored for now
(defn api [foo ?route ctx]
  {:pre [?route (not (string? ?route))]
   :post [#_(seq %)]}
  ; We can actually call into the foundation a second time here to get the ide-domain
  (let [ide-domain-q (foundation/domain-request "hyperfiddle" (:peer ctx))
        ide-domain (hc/hydrate-api (:peer ctx) ide-domain-q)
        user-profile @(reactive/cursor (.-state-atom (:peer ctx)) [:user-profile])]
    (case foo
      "page" (concat [ide-domain-q]
                     (if ?route
                       (browser/request-from-route ?route (page-target-context ctx ?route user-profile)))
                     (if (and (activate-ide? (foundation/hostname->hf-domain-name ctx)) ide-domain)
                       (browser/request-from-route (ide-route ?route) (page-ide-context ctx ide-domain ?route user-profile))))
      "ide" (concat [ide-domain-q]
                    (if ide-domain
                      (browser/request-from-route ?route (leaf-ide-context ctx ide-domain user-profile))))
      "user" (if ?route
               (browser/request-from-route ?route (leaf-target-context ctx ?route user-profile))))))

#?(:cljs
   (defn view-page [?route ctx]
     (let [ide-domain (hc/hydrate-api (:peer ctx) (foundation/domain-request "hyperfiddle" (:peer ctx)))
           ide-active (activate-ide? (foundation/hostname->hf-domain-name ctx))
           user-profile @(reactive/cursor (.-state-atom (:peer ctx)) [:user-profile])
           ctx (assoc ctx :navigate-cmp (reagent/partial navigate-cmp/navigate-cmp (reagent/partial runtime/encode-route (:peer ctx))))]
       (react-fragment
         :view-page
         (if ide-active
           (if ide-domain
             (let [ctx (-> (page-ide-context ctx ide-domain ?route user-profile)
                           (assoc :hypercrud.ui/ui-error browser-ui/ui-error-inline))]
               [browser/ui-from-route (ide-route ?route) ctx "topnav hidden-print"])
             [:div "loading... (ide bootstrap, you edited ide-domain)"]))
         (if ?route
           (let [class (str "hyperfiddle-user" (if ide-active " hyperfiddle-ide-user" ""))]
             ; This is different than foo=user because it is special css at root attach point
             [browser/ui-from-route ?route (page-target-context ctx ?route user-profile) class]))))))

#?(:cljs
   (defn view [foo route ctx]                               ; pass most as ref for reactions
     (let [ide-domain (hc/hydrate-api (:peer ctx) (foundation/domain-request "hyperfiddle" (:peer ctx)))
           user-profile @(reactive/cursor (.-state-atom (:peer ctx)) [:user-profile])]
       (case foo
         "page" (view-page route ctx)                       ; component, seq-component or nil
         ; On SSR side this is only ever called as "page", but it could be differently (e.g. turbolinks)
         ; On Browser side, also only ever called as "page", but it could be configured differently (client side render the ide, server render userland...?)
         "ide" [browser/ui-from-route route (leaf-ide-context ctx ide-domain user-profile)]
         "user" [browser/ui-from-route route (leaf-target-context ctx route user-profile)]))))
