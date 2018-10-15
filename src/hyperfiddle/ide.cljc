(ns hyperfiddle.ide
  (:require
    [cats.core :refer [mlet return]]
    [cats.labs.promise]
    [clojure.string :as str]
    #?(:cljs [contrib.css :refer [css]])
    [contrib.ct :refer [unwrap]]
    [contrib.ednish :refer [decode-ednish encode-ednish]]
    [contrib.reactive :as r]
    [contrib.reader :refer [read-edn-string+ read-edn-string!]]
    [contrib.rfc3986 :refer [split-fragment encode-rfc3986-pchar]]
    [contrib.string :refer [empty->nil]]
    #?(:cljs [contrib.ui :refer [easy-checkbox radio-with-label]])
    [contrib.uri :refer [->URI]]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.browser-request :refer [request-from-route]]
    #?(:cljs [hypercrud.browser.browser-ui :as browser-ui])
    [hypercrud.browser.context :as context]
    [hypercrud.browser.routing :as routing]
    [hypercrud.browser.router :as router]
    [hypercrud.browser.router-bidi :as router-bidi]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.ide.system-fiddle :refer [system-fiddle?]]
    #?(:cljs [hyperfiddle.ui :as ui])
    #?(:cljs [hyperfiddle.ide.hf-live :as hf-live])
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

(defn topnav-route [[fiddle datomic-args service-args frag :as route] ctx]
  ; Don't impact the request! Topnav can always use :target-route
  (let [ide-domain (magic-ide-fiddle? fiddle (get-in ctx [:hypercrud.browser/domain :domain/ident]))
        ?target-fiddle (if-not ide-domain [#entity["$" (base/legacy-fiddle-ident->lookup-ref fiddle)]])]
    [:hyperfiddle/topnav ?target-fiddle]))

(defn ide-route [[fiddle-ident :as route] ctx]
  (let [params (when-not (magic-ide-fiddle? fiddle-ident (get-in ctx [:hypercrud.browser/domain :domain/ident]))
                 [[:fiddle/ident fiddle-ident]])]
    [:hyperfiddle/ide params]))

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
                                      #?(:cljs (r/partial browser-ui/frame-on-click (:peer ctx) nil branch-aux)))
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
        domain @(runtime/state rt [::runtime/domain])
        home-route (some-> domain :domain/home-route read-edn-string+ (->> (unwrap #(timbre/error %)))) ; in hf format
        router (some-> domain :domain/router read-edn-string+ (->> (unwrap #(timbre/error %))))]

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
  (let [domain @(runtime/state rt [::runtime/domain])
        router (some-> domain :domain/router read-edn-string+ (->> (unwrap #(timbre/error %))))
        home-route (some-> domain :domain/home-route read-edn-string+ (->> (unwrap #(timbre/error %))))
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
                "user" (if (:active-ide? (runtime/host-env (:peer ctx)))
                         (merge user (select-keys ide [(->URI "datomic:free://datomic:4334/hyperfiddle-users")]))
                         user))
        basis (sort basis)]                                 ; Userland api-fn should filter irrelevant routes
    (timbre/debug (pr-str basis))
    #_(determine-local-basis (hydrate-route route ...))
    basis))

; todo should summon route via context/target-route. but there is still tension in the data api for deferred popovers
(defn api [[fiddle :as route] ctx]
  {:pre [route (not (string? route))]}
  (case (get-in ctx [::runtime/branch-aux ::foo])
    "page" (let [ide-ctx (page-ide-context ctx)]
             (into
               (cond
                 (:active-ide? (runtime/host-env (:peer ctx)))
                 (concat (request-from-route (topnav-route route ctx) ide-ctx)
                         (request-from-route (ide-route route ctx) ide-ctx))

                 @(runtime/state (:peer ctx) [::runtime/domain :domain/environment :enable-hf-live?])
                 (request-from-route (ide-route route ctx) ide-ctx))
               (if (magic-ide-fiddle? fiddle (get-in ctx [:hypercrud.browser/domain :domain/ident]))
                 (request-from-route route ide-ctx)
                 (request-from-route route (page-target-context ctx)))))
    "ide" (request-from-route route (leaf-ide-context ctx))
    "user" (request-from-route route (leaf-target-context ctx))))

(defn read-fragment-only-hf-src [frag]
  (let [frag (some-> frag decode-ednish read-edn-string+ (->> (unwrap (constantly nil))))]
    (if (#{:hf.src} (some-> frag namespace keyword))
      frag)))

#?(:cljs
   (defn primary-content-ide [ide-ctx content-ctx route]
     (let [state (r/atom {:edn-fiddle false})]
       (fn [ide-ctx content-ctx route]
         [:div.row.hyperfiddle.hf-live.unp.no-gutters {:key "primary-content"}
          [:div.result.col-sm
           [:div "Result:"
            (into [:span.hyperfiddle.hf-live.radio-group]
                  (->> [{:label "api" :tooltip "What the API client sees" :value :hypercrud.browser.browser-ui/api}
                        {:label "data" :tooltip "Ignore :fiddle/renderer" :value :hypercrud.browser.browser-ui/xray}
                        {:label "view" :tooltip "Use :fiddle/renderer" :value :hypercrud.browser.browser-ui/user}]
                       (map (fn [props]
                              [radio-with-label
                               (assoc props :checked (= (:value props) @(runtime/state (:peer ide-ctx) [:display-mode]))
                                            :on-change (r/comp (r/partial runtime/dispatch! (:peer ide-ctx)) actions/set-display-mode))]))))]
           [ui/iframe content-ctx
            {:route route
             :class (css "hyperfiddle-user"
                         "hyperfiddle-ide"
                         "hf-live"
                         (some-> content-ctx :hypercrud.ui/display-mode deref name (->> (str "display-mode-"))))}]]
          (let [as-edn (r/cursor state [:edn-fiddle])]
            [:div.src.col-sm
             [:div "Interactive Hyperfiddle editor:" [contrib.ui/easy-checkbox-boolean " EDN?" as-edn {:class "hyperfiddle hf-live"}]]
             [ui/iframe ide-ctx
              {:route (ide-route route content-ctx)
               :initial-tab (let [[_ _ _ frag] route] (read-fragment-only-hf-src frag))
               :user-renderer (if @as-edn hf-live/result-edn fiddle-src-renderer)
               :class (css "devsrc" "hf-live")}]])]))))

#?(:cljs
   (defn primary-content-production "No ide layout markup" [content-ctx route]
     ^{:key :primry-content}
     [ui/iframe content-ctx
      {:route (router/dissoc-frag route)
       :class (css "hyperfiddle-user"
                   "hyperfiddle-ide"
                   (some-> content-ctx :hypercrud.ui/display-mode deref name (->> (str "display-mode-"))))}]
     ))

#?(:cljs
   (defn view-page [[fiddle :as route] ctx]
     (let [ide-ctx (page-ide-context ctx)
           is-magic-ide-fiddle (magic-ide-fiddle? fiddle (get-in ctx [:hypercrud.browser/domain :domain/ident]))
           {:keys [:active-ide?]} (runtime/host-env (:peer ctx))
           is-editor (and active-ide?
                          (not is-magic-ide-fiddle)
                          (not (system-fiddle? fiddle))     ; Schema editor, console links
                          )]

       [:<> {:key "view-page"}

        ; Topnav
        (when active-ide?
          [ui/iframe
           (assoc ide-ctx :hypercrud.ui/error (r/constantly ui-error/error-inline))
           {:route (topnav-route route ctx)
            :class "hidden-print"}])

        ; Primary content area
        (cond
          ; tunneled ide route like /hyperfiddle.ide/domain - primary, blue background (IDE),
          is-magic-ide-fiddle ^{:key :primary-content} [ui/iframe ide-ctx {:route route :class "devsrc"}]

          is-editor [primary-content-ide ide-ctx (page-target-context ctx) route]

          :else [primary-content-production (page-target-context ctx) route]
          )])))

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
