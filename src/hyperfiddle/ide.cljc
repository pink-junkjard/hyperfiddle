(ns hyperfiddle.ide
  (:require [bidi.bidi :as bidi]
            [contrib.data :refer [unwrap]]
            [contrib.reactive :as reactive]
    #?(:cljs [contrib.reagent :refer [fragment]])
            [contrib.rfc3986 :refer [split-fragment]]
            [contrib.string :refer [abc safe-read-edn-string]]
            [hypercrud.browser.base :as base]
    #?(:cljs [hypercrud.browser.browser-ui :as browser-ui])
            [hypercrud.browser.core :as browser]
            [hypercrud.browser.routing :as routing]
            [hypercrud.browser.router :as router]
            [hypercrud.browser.system-fiddle :refer [system-fiddle?]]
            [hypercrud.client.core :as hc]
    #?(:cljs [hypercrud.ui.error :as ui-error])
    #?(:cljs [hypercrud.ui.navigate-cmp :as navigate-cmp])
    #?(:cljs [hypercrud.ui.stale :as stale])
            [hyperfiddle.foundation :as foundation]
            [hyperfiddle.io.hydrate-requests :refer [hydrate-one!]]
            [hyperfiddle.runtime :as runtime]
            [promesa.core :as p]
    #?(:cljs [reagent.core :as reagent])
            [taoensso.timbre :as timbre]

    ; pull in public ui deps
    ; todo these hc.ui.* should be reduced to one require e.g. [hypercrud.ui]
    #?(:cljs [contrib.reagent])
            [hypercrud.ui.auto-control]
    #?(:cljs [hypercrud.ui.result])

    ; pull in the entire ide app for reference from user-land
            [hyperfiddle.ide.actions]
            [hyperfiddle.ide.fiddles.domain]
            [hyperfiddle.ide.fiddles.fiddle-links.bindings]
    #?(:cljs [hyperfiddle.ide.fiddles.fiddle-links.renderer])
    #?(:cljs [hyperfiddle.ide.fiddles.fiddle-src :refer [fiddle-src-renderer]])
            [hyperfiddle.ide.fiddles.schema]
    #?(:cljs [hyperfiddle.ide.fiddles.schema-attribute])
    #?(:cljs [hyperfiddle.ide.fiddles.topnav :as topnav])
    #?(:cljs [hyperfiddle.ide.fiddles.user-dashboard])))

(defn domain [rt hyperfiddle-hostname hostname]
  (let [domain-basis (if-let [global-basis @(runtime/state rt [::runtime/global-basis])]
                       (:domain global-basis)
                       (->> @(runtime/state rt [::runtime/partitions])
                            (some (fn [[_ partition]]
                                    (->> (:local-basis partition)
                                         (filter (fn [[k _]] (= foundation/domain-uri k)))
                                         seq)))))
        stage nil
        hf-domain-name (foundation/hostname->hf-domain-name hostname hyperfiddle-hostname)
        request (foundation/domain-request hf-domain-name rt)]
    (-> (hydrate-one! rt (into {} domain-basis) stage request)
        (p/then (fn [domain]
                  (if (nil? (:db/id domain))
                    ; terminate when domain not found
                    (throw (ex-info "Domain does not exist" {:hyperfiddle.io/http-status-code 404
                                                             :domain-name hf-domain-name}))
                    domain))))))

(defn ide-route [[fiddle datomic-args service-args frag :as route]]
  ; Don't impact the request! Topnav can always use :target-route
  [:hyperfiddle/topnav [#entity["$" (base/legacy-fiddle-ident->lookup-ref fiddle)]]])

(let [always-user (atom :user)
      constantly-nil (constantly nil)]
  ; ide is overloaded, these ide-context functions are exclusive to (top)
  ; despite being in the namespace (hyperfiddle.ide) which encompasses the union of target/user (bottom) and ide (top)
  (defn- *-ide-context [ctx ide-domain]
    {:pre [ide-domain]}
    (-> ctx
        (assoc :hypercrud.browser/page-on-click constantly-nil ; disable alt-nav up top
               :hypercrud.ui/display-mode always-user
               :target-domain (:hypercrud.browser/domain ctx) ; todo rename :target-domain to :hyperfiddle.ide/target-domain
               :user-profile @(runtime/state (:peer ctx) [:user-profile]))
        (update :hypercrud.browser/domain
                (fn [domain]
                  (-> (foundation/process-domain ide-domain)
                      (assoc-in [:domain/environment "$"] (:domain/fiddle-repo domain))))))))

(defn leaf-ide-context [ctx ide-domain]
  ; ide leaf-context does not have enough information to set hyperfiddle.ide/target-route
  ; this means ide popovers CANNOT access it
  (*-ide-context ctx ide-domain))

(defn page-ide-context [ctx ide-domain target-route]
  {:pre [target-route]}
  (-> (assoc ctx
        ::runtime/branch-aux {::foo "ide"}
        ; hyperfiddle.ide/target-route is ONLY available to inlined IDE (no deferred popovers)
        :target-route target-route)                         ; todo rename :target-route to :hyperfiddle.ide/target-route
      (*-ide-context ide-domain)))

(defn activate-ide? [x]
  ; To userland this "www" constant would require more complicated rules
  ; for mapping a hostname to a domain-ident
  (let [should-be-active (and (not (foundation/alias? x))
                              (not (= "www" x)))
        ; WWW needs a way to activate this. For now we can mirror the domain on www2
        explicitly-active false]
    (or explicitly-active should-be-active)))

(defn- *-target-context [ctx route]
  (assoc ctx
    :hypercrud.browser/page-on-click (let [branch-aux {:hyperfiddle.ide/foo "page"}]
                                       #?(:cljs (reactive/partial browser-ui/page-on-click (:peer ctx) nil branch-aux)))
    :hypercrud.ui/display-mode (runtime/state (:peer ctx) [:display-mode])
    :ide-active (activate-ide? (foundation/hostname->hf-domain-name ctx))
    :user-profile @(runtime/state (:peer ctx) [:user-profile])

    ; these target values only exists to allow the topnav to render in the bottom/user
    ; IF we MUST to support that, this should probably be done higher up for both ide and user at the same time
    ; and SHOULD ONLY be applied for ide within ide (other user fns don't get access to these values)
    :target-domain (:hypercrud.browser/domain ctx)          ; todo rename :target-domain to :hyperfiddle.ide/target-domain
    :target-route route                                     ; todo rename :target-route to :hyperfiddle.ide/target-route
    ))

(defn leaf-target-context [ctx route]
  (*-target-context ctx route))

(defn page-target-context [ctx route]
  (-> (assoc ctx ::runtime/branch-aux {::foo "user"})
      (*-target-context route)))

(defn ->bidi-consistency-wrapper [{:keys [handler route-params] :as ?r}]
  ; Bidi's interface is inconsistent and makes you understand two ways to identify a route
  ; "bidi/match" return val syntax, is the bad one
  ; Canonicalize on the "bidi/path-for" syntax
  (if ?r (apply conj [handler] (mapcat identity route-params))))

(defn bidi->hf [[handler & ?route-params :as ?r]]
  (if ?r
    [handler
     (->> ?route-params                                     ; bidi gives us alternating k/v
          (partition-all 2)
          (map vec)
          sort                                              ; order by keys for hyperfiddle, router should use kw or int
          (mapv second)                                     ; drop keys; hyperfiddle params are associative by index
          )]))

(defn bidi-match->path-for "adapt bidi's inconsistent interface" [[h & ps :as ?r]]
  (if ?r {:handler h :route-params ps}))

(defn ->bidi [[fiddle args :as ?r]]
  (assert (not (system-fiddle? fiddle)) "bidi router doesn't handle sys links")
  ; this is going to generate param names of 0, 1, ... which maybe doesn't work for all routes
  ; we would need to disallow bidi keywords for this to be valid. Can bidi use ints? I think not :(
  (if ?r (apply conj [fiddle] (mapcat vector (abc) args))))

(defn route-decode [rt path-and-frag]
  {:pre [(string? path-and-frag)]}
  (let [[path frag] (split-fragment path-and-frag)
        domain @(runtime/state rt [:hyperfiddle.runtime/domain])
        home-route (some-> domain :domain/home-route safe-read-edn-string unwrap) ; in hf format
        router (some-> domain :domain/router safe-read-edn-string unwrap)]
    (case path
      "/" (router/assoc-frag home-route frag)
      (or (if (= "/_/" (subs path-and-frag 0 3)) (routing/decode (subs path-and-frag 2) #_"include leading /"))
          (some-> router (bidi/match-route path-and-frag) ->bidi-consistency-wrapper bidi->hf)
          (routing/decode path-and-frag)))))

(defn route-encode [rt [fiddle :as route]]
  (let [domain @(runtime/state rt [:hyperfiddle.runtime/domain])
        router (some-> domain :domain/router safe-read-edn-string unwrap)
        home-route (some-> domain :domain/home-route safe-read-edn-string unwrap)
        home-route (if router (bidi->hf home-route) home-route)]
    (or
      (if (system-fiddle? fiddle) (str "/_" (routing/encode route)))
      (if (= route home-route) "/")
      (if router (apply bidi/path-for router (->bidi route)))
      (routing/encode route))))

(defn local-basis [global-basis route ctx]
  ;local-basis-ide and local-basis-user
  (let [{:keys [ide user]} global-basis
        basis (case (get-in ctx [::runtime/branch-aux ::foo])
                "page" (concat ide user)
                "ide" (concat ide user)
                "user" user)
        basis (sort basis)]                                 ; Userland api-fn should filter irrelevant routes
    (timbre/debug (pr-str basis))
    #_(determine-local-basis (hydrate-route route ...))
    basis))

; Reactive pattern obfuscates params
(defn api [route ctx]
  {:pre [route (not (string? route))]
   :post [#_(seq %)]}
  ; We can actually call into the foundation a second time here to get the ide-domain
  (let [ide-domain-q (foundation/domain-request "hyperfiddle" (:peer ctx))
        ide-domain (hc/hydrate-api (:peer ctx) (:branch ctx) ide-domain-q)]
    (case (get-in ctx [::runtime/branch-aux ::foo])
      "page" (concat [ide-domain-q]
                     (if route
                       (browser/request-from-route route (page-target-context ctx route)))
                     (if (and (activate-ide? (foundation/hostname->hf-domain-name ctx)) ide-domain)
                       (browser/request-from-route (ide-route route) (page-ide-context ctx ide-domain route))))
      "ide" (concat [ide-domain-q]
                    (if ide-domain
                      (browser/request-from-route route (leaf-ide-context ctx ide-domain))))
      "user" (if route
               (browser/request-from-route route (leaf-target-context ctx route))))))

(defn -dumb-loading [e]
  ; todo inline staging area?
  [:div
   [:h3 "Fatal error"]
   [:pre (pr-str e)]])

#?(:cljs
   (defn view-page [?route ctx]
     (let [dev (activate-ide? (foundation/hostname->hf-domain-name ctx))
           src-mode (let [[_ _ _ frag] ?route] (topnav/src-mode? frag)) ; Immoral - :src bit is tunneled in userland fragment space
           ctx (assoc ctx :navigate-cmp (reagent/partial navigate-cmp/navigate-cmp (reagent/partial runtime/encode-route (:peer ctx))))]
       (fragment
         :view-page

         ; IDE
         (when dev
           [stale/loading
            (stale/can-be-loading? ctx)
            @(hc/hydrate (:peer ctx) (:branch ctx) (foundation/domain-request "hyperfiddle" (:peer ctx)))
            -dumb-loading
            (fn topnav [ide-domain]
              (let [ctx (-> (page-ide-context ctx ide-domain ?route)
                            (assoc :hypercrud.ui/error (reactive/constantly ui-error/error-inline)))]
                (fragment                                   ; These are the same data, just different views.
                  :_
                  [browser/ui-from-route (ide-route ?route)
                   ctx #_(assoc ctx :user-renderer hyperfiddle.ide.fiddles.topnav/renderer)
                   "topnav hidden-print"]
                  (if src-mode
                    [browser/ui-from-route (ide-route ?route)
                     (assoc ctx :user-renderer fiddle-src-renderer)
                     "devsrc"]))))])

         ; Production
         (if (and ?route (not src-mode))
           [browser/ui-from-route ?route (page-target-context ctx ?route) (str "hyperfiddle-user" (if dev " hyperfiddle-ide-user" ""))]))))) ; This is different than foo=user because it is special css at root attach point

#?(:cljs
   (defn view [route ctx]                                   ; pass most as ref for reactions
     (let [ide-domain (hc/hydrate-api (:peer ctx) (:branch ctx) (foundation/domain-request "hyperfiddle" (:peer ctx)))]
       (case (get-in ctx [::runtime/branch-aux ::foo])
         "page" (view-page route ctx)                       ; component, seq-component or nil
         ; On SSR side this is only ever called as "page", but it could be differently (e.g. turbolinks)
         ; On Browser side, also only ever called as "page", but it could be configured differently (client side render the ide, server render userland...?)
         "ide" [browser/ui-from-route route (leaf-ide-context ctx ide-domain)]
         "user" [browser/ui-from-route route (leaf-target-context ctx route)]))))
