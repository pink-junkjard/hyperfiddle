(ns hyperfiddle.ide
  (:require
    [cats.core :refer [mlet return >>=]]
    [cats.labs.promise]
    [cats.monad.either :refer [branch]]
    [clojure.string :as str]
    #?(:cljs [contrib.css :refer [css]])
    [contrib.ct :refer [unwrap]]
    [contrib.reactive :as r]
    #?(:cljs [contrib.reagent :refer [fragment]])
    [contrib.rfc3986 :refer [split-fragment]]
    [contrib.string :refer [safe-read-edn-string empty->nil]]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.browser-request :refer [request-from-route]]
    #?(:cljs [hypercrud.browser.browser-ui :as browser-ui])
    [hypercrud.browser.context :as context]
    [hypercrud.browser.routing :as routing]
    [hypercrud.browser.router :as router]
    [hypercrud.browser.router-bidi :as router-bidi]
    [hyperfiddle.ide.system-fiddle :refer [system-fiddle?]]
    #?(:cljs [hyperfiddle.ui :as ui])
    #?(:cljs [hypercrud.ui.error :as ui-error])
    #?(:cljs [hypercrud.ui.stale :as stale])
    [hyperfiddle.data]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.io.hydrate-requests :refer [hydrate-one!]]
    [hyperfiddle.runtime :as runtime]
    [taoensso.timbre :as timbre]

    ; pull in the entire ide app for reference from user-land
    #?(:cljs [hyperfiddle.ide.fiddles.domain])
    #?(:cljs [hyperfiddle.ide.fiddles.fiddle-links.renderer])
    #?(:cljs [hyperfiddle.ide.fiddles.fiddle-src :refer [fiddle-src-renderer]])
    [hyperfiddle.ide.fiddles.schema]
    #?(:cljs [hyperfiddle.ide.fiddles.schema-attribute])
    #?(:cljs [hyperfiddle.ide.fiddles.topnav :as topnav])))


(defn domain [rt domain-eid]
  (let [domains-basis (->> (if-let [global-basis @(runtime/state rt [::runtime/global-basis])]
                             (:domain global-basis)
                             (->> @(runtime/state rt [::runtime/partitions])
                                  (some (fn [[_ partition]]
                                          (->> (:local-basis partition)
                                               (filter (fn [[k _]] (= foundation/domain-uri k)))
                                               seq)))))
                           (into {}))
        stage nil]
    (mlet [raw-domain (->> (foundation/domain-request domain-eid rt)
                           (hydrate-one! rt domains-basis stage))
           :let [domain (if (nil? (:db/id raw-domain))
                          ; terminate when domain not found
                          (throw (ex-info "Domain not found" {:hyperfiddle.io/http-status-code 404
                                                              :domain-eid domain-eid}))
                          (foundation/process-domain raw-domain))]]
      (return domain))))

(defn magic-ide-fiddle? [fiddle-ident domain-ident]
  (and (not= foundation/source-domain-ident domain-ident)
       (= "hyperfiddle.ide" (namespace fiddle-ident))))

(defn ide-fiddle-route [[fiddle datomic-args service-args frag :as route] ctx]
  ; Don't impact the request! Topnav can always use :target-route
  (let [ide-domain (magic-ide-fiddle? fiddle (get-in ctx [:hypercrud.browser/domain :domain/ident]))
        ?target-fiddle (if-not ide-domain [#entity["$" (base/legacy-fiddle-ident->lookup-ref fiddle)]])]
    [:hyperfiddle/topnav ?target-fiddle]))

; ide is overloaded, these ide-context functions are exclusive to (top)
; despite being in the namespace (hyperfiddle.ide) which encompasses the union of target/user (bottom) and ide (top)
(defn- *-ide-context [ctx]
  (-> ctx
      (assoc :hypercrud.ui/display-mode (r/track identity :hypercrud.browser.browser-ui/user))
      (context/source-mode)))

(defn leaf-ide-context [ctx]
  (*-ide-context ctx))

(defn page-ide-context [ctx]
  (-> (assoc ctx ::runtime/branch-aux {::foo "ide"})
      (*-ide-context)))

(defn- *-target-context [ctx]
  (assoc ctx
    :hyperfiddle.ui/iframe-on-click (let [branch-aux {:hyperfiddle.ide/foo "page"}]
                                      #?(:cljs (r/partial browser-ui/page-on-click (:peer ctx) nil branch-aux)))
    :hypercrud.ui/display-mode (runtime/state (:peer ctx) [:display-mode])
    :hyperfiddle.ui/debug-tooltips (:active-ide? (runtime/host-env (:peer ctx)))))

(defn leaf-target-context [ctx]
  (*-target-context ctx))

(defn page-target-context [ctx]
  (-> (assoc ctx ::runtime/branch-aux {::foo "user"})
      (*-target-context)))

(defn route-decode [rt path-and-frag]
  {:pre [(string? path-and-frag)]}
  (let [[path frag] (split-fragment path-and-frag)
        domain @(runtime/state rt [:hyperfiddle.runtime/domain])
        home-route (some-> domain :domain/home-route safe-read-edn-string (->> (unwrap #(timbre/error %)))) ; in hf format
        router (some-> domain :domain/router safe-read-edn-string (->> (unwrap #(timbre/error %))))]

    ;(if (= "/!ide/" (subs path-and-frag 0 6)) (routing/decode (subs path-and-frag 5)))
    (or
      (case path
        "/" (if home-route (router/assoc-frag home-route frag))
        (or (if (= "/_/" (subs path-and-frag 0 3)) (routing/decode (subs path-and-frag 2) #_"include leading /"))
            ; fiddle namespace, hyperfiddle.ide, overlays userland route space
            (if router (router-bidi/decode router path-and-frag))
            (routing/decode path-and-frag)))
      [:hyperfiddle.system/not-found])))

(defn route-encode [rt [fiddle _ _ frag :as route]]
  {:post [(str/starts-with? % "/")]}
  (let [domain @(runtime/state rt [:hyperfiddle.runtime/domain])
        router (some-> domain :domain/router safe-read-edn-string (->> (unwrap #(timbre/error %))))
        home-route (some-> domain :domain/home-route safe-read-edn-string (->> (unwrap #(timbre/error %))))
        home-route (if router (router-bidi/bidi->hf home-route) home-route)]
    ;(case (namespace fiddle)) "hyperfiddle.ide" (str "/!ide/" (routing/encode route))
    (or
      (if (system-fiddle? fiddle) (str "/_" (routing/encode route)))
      (if (= (router/dissoc-frag route) home-route) (if (empty->nil frag) (str "/#" frag) "/"))
      ; fiddle namespace, hyperfiddle.ide, overlays userland route space
      (if router (router-bidi/encode router route))
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

; todo should summon route via context/target-route. but there is still tension in the data api for deferred popovers
(defn api [[fiddle :as route] ctx]
  {:pre [route (not (string? route))]}
  (case (get-in ctx [::runtime/branch-aux ::foo])
    "page" (into
             (when true #_(:active-ide? (runtime/host-env (:peer ctx))) ; true for embedded src mode
               (request-from-route (ide-fiddle-route route ctx) (page-ide-context ctx)))
             (if (magic-ide-fiddle? fiddle (get-in ctx [:hypercrud.browser/domain :domain/ident]))
               (request-from-route route (page-ide-context ctx))
               (request-from-route route (page-target-context ctx))))
    "ide" (request-from-route route (leaf-ide-context ctx))
    "user" (request-from-route route (leaf-target-context ctx))))

#?(:cljs
   (defn view-page [[fiddle :as route] ctx]
     (let [src-mode (let [[_ _ _ frag] route] (topnav/src-mode? frag)) ; Immoral - :src bit is tunneled in userland fragment space
           ide-ctx (page-ide-context ctx)
           ide-route (ide-fiddle-route route ctx)
           +topnav-ctx (base/data-from-route ide-route ide-ctx)
           +account-ctx (>>= +topnav-ctx #(hyperfiddle.data/browse+ % :hf/iframe :account))
           r?user (branch +account-ctx (constantly (r/track identity nil)) :hypercrud.browser/data)
           ; Feature flags are needed in IDE and userland (for widgets)
           ide-ctx (assoc ide-ctx ::user r?user)
           ctx (assoc ctx ::user r?user)
           {:keys [:active-ide?]} (runtime/host-env (:peer ctx))]

       (fragment
         :view-page

         ; Topnav
         (when active-ide?
           [ui/iframe
            (assoc ide-ctx :hypercrud.ui/error (r/constantly ui-error/error-inline))
            {:route ide-route
             :class "hidden-print"}])

         ; Content area
         (if (magic-ide-fiddle? fiddle (get-in ctx [:hypercrud.browser/domain :domain/ident]))

           ^{:key :primary-content}
           [ui/iframe ide-ctx {:route route :class "devsrc"}] ; primary, blue background (IDE), magic ide route like /hyperfiddle.ide/domain

           (fragment
             :primary-content
             (when (and active-ide? src-mode)               ; primary, blue background (IDE)   /:posts/:hello-world#:src
               [ui/iframe ide-ctx                           ; srcmode is equal to topnav route but a diff renderer
                {:route (ide-fiddle-route route ctx)
                 :class (css "container-fluid" "devsrc")
                 :user-renderer fiddle-src-renderer}])

             ; User content view in both prod and ide. What if src-mode and not-dev ? This draws nothing
             (when-not src-mode                             ; primary, white background (User)   /:posts/:hello-world
               (let [ctx (page-target-context ctx)]
                 [ui/iframe ctx {:route route
                                 :class (css "container-fluid" "hyperfiddle-user"
                                             (when active-ide? "hyperfiddle-ide")
                                             (some-> ctx :hypercrud.ui/display-mode deref name (->> (str "display-mode-"))))}]))))))))

#?(:cljs
   ; todo should summon route via context/target-route. but there is still tension in the data api for deferred popovers
   (defn view [[fiddle :as route] ctx]                      ; pass most as ref for reactions
     (case (namespace fiddle)
       ;"hyperfiddle.ide" [ui/iframe (leaf-ide-context ctx) {:route route}]
       (case (get-in ctx [::runtime/branch-aux ::foo])
         "page" (view-page route ctx)                       ; component, seq-component or nil
         ; On SSR side this is only ever called as "page", but it could be differently (e.g. turbolinks)
         ; On Browser side, also only ever called as "page", but it could be configured differently (client side render the ide, server render userland...?)
         "ide" [ui/iframe (leaf-ide-context ctx) {:route route}]
         "user" [ui/iframe (leaf-target-context ctx) {:route route}]))))
