(ns hyperfiddle.ide
  (:require
    [bidi.bidi :as bidi]
    [cats.monad.either :as either]
    [cats.labs.promise]
    [contrib.base-64-url-safe :as base64-url-safe]
    #?(:cljs [contrib.css :refer [css]])
    [contrib.ct :refer [unwrap]]
    [contrib.ednish :refer [decode-ednish]]
    [contrib.pprint :as pprint]
    [contrib.reactive :as r]
    [contrib.reader :refer [read-edn-string+]]
    [contrib.string :refer [or-str]]
    #?(:cljs [contrib.ui :refer [easy-checkbox radio-with-label]])
    [hypercrud.browser.base :as base]
    [hypercrud.browser.browser-request :refer [request-from-route]]
    [hypercrud.types.DbRef :refer [->DbRef]]
    [hypercrud.types.EntityRequest :refer [->EntityRequest]]
    [hypercrud.types.ThinEntity :refer [->ThinEntity]]
    #?(:cljs [hypercrud.ui.error :as ui-error])
    [hyperfiddle.actions :as actions]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.routes :as routes]
    [hyperfiddle.runtime :as runtime]
    #?(:cljs [hyperfiddle.ui :as ui])
    #?(:cljs [hyperfiddle.ui.iframe :refer [iframe-cmp]])
    #?(:cljs [hypercrud.ui.stale :as stale])
    [taoensso.timbre :as timbre]

    ; pull in the entire ide app for reference from user-land
    #?(:cljs [hyperfiddle.ide.fiddles.fiddle-src :refer [fiddle-src-renderer]])
    [hyperfiddle.ide.fiddles.schema]
    #?(:cljs [hyperfiddle.ide.fiddles.schema-attribute])
    #?(:cljs [hyperfiddle.ide.fiddles.topnav :as topnav])
    [hyperfiddle.project :as project]))


(defn user-route [ctx]
  @(runtime/state (:peer ctx) [::runtime/partitions nil :route]))

(defn stateless-login-url
  ([ctx] (stateless-login-url ctx (domain/url-encode (runtime/domain (:peer ctx)) (user-route ctx))))
  ([ctx state]
   (let [{:keys [build hostname]} nil
         {:keys [domain client-id]} (-> (runtime/domain (:peer ctx)) domain/environment :auth0)]
     (str domain "/login?"
          "client=" client-id
          "&scope=" "openid email profile"
          "&state=" (base64-url-safe/encode state)
          "&redirect_uri=" (str "http://" hostname (bidi/path-for (routes/build build) :auth0-redirect))))))

(defn magic-ide-fiddle? [fiddle-ident]
  ; Write this as s/conform
  (and (keyword? fiddle-ident) (= "hyperfiddle.ide" (namespace fiddle-ident))))

(defn topnav-route [[fiddle datomic-args service-args frag :as route] ctx]
  ; Don't impact the request! Topnav can always use :target-route
  (let [ide-domain (magic-ide-fiddle? fiddle)
        ?target-fiddle (if-not ide-domain [(->ThinEntity "$" (base/legacy-fiddle-ident->lookup-ref fiddle))])]
    [:hyperfiddle/topnav ?target-fiddle]))

(defn ide-route [[fiddle-ident :as route] ctx]
  (let [params (when-not (magic-ide-fiddle? fiddle-ident)
                 [[:fiddle/ident fiddle-ident]])]
    [:hyperfiddle/ide params]))

(defn api [route ctx]
  (doall (request-from-route (topnav-route route ctx) ctx))
  (doall (request-from-route (ide-route route ctx) ctx))
  (doall (request-from-route route ctx))
  nil)

(defn read-fragment-only-hf-src [frag]
  (let [frag (some-> frag decode-ednish read-edn-string+ (->> (unwrap (constantly nil))))]
    (if (#{:hf.src} (some-> frag namespace keyword))
      frag)))

#?(:cljs
   (defn result-edn [val ctx props]
     (let [s (-> val
                 #_(as-> $ (if (seq attrs) (select-keys $ attrs) $)) ; omit elided fiddle attrs
                 (pprint/pprint-str 50))]
       [contrib.ui/code (assoc props                        ; Class ends up not on the codemirror, todo
                          :value s
                          :read-only true)])))

#?(:cljs
   (defn frame-on-click [rt route event]
     (when (and route (.-altKey event))                     ; under what circumstances is route nil?
       (let [native-event (.-nativeEvent event)
             anchor (-> (.composedPath native-event) (aget 0) (.matches "a"))
             anchor-descendant (-> (.composedPath native-event) (aget 0) (.matches "a *"))]
         (when-not (or anchor anchor-descendant)
           (.stopPropagation event)
           (js/window.open (domain/url-encode (runtime/domain rt) route) "_blank"))))))

#?(:cljs
   (defn user-iframe [ctx]
     (let [display-mode (runtime/state (:peer ctx) [:display-mode])
           ctx (assoc ctx
                 :hypercrud.ui/display-mode display-mode
                 :hyperfiddle.ui/debug-tooltips true
                 :hyperfiddle.ui.iframe/on-click (r/partial frame-on-click (:peer ctx)))
           route nil]
       [iframe-cmp ctx
        {:route route
         :class (css "hyperfiddle-user"
                     "hyperfiddle-ide"
                     "hf-live"
                     (some-> @display-mode name (->> (str "display-mode-"))))}])))

#?(:cljs
   (defn user-view [ctx]
     (let [code-str (assert false "todo build new peer")
           code+ (project/eval-domain-code!+ code-str)]
       [:<>
        (when (either/left? code+)
          (let [e @code+]
            (timbre/error e)
            (let [href (domain/url-encode (runtime/domain (:peer ctx)) [:hyperfiddle.ide/domain [[:domain/ident (-> (runtime/domain (:peer ctx)) domain/ident)]]])
                  message (if-let [cause-message (some-> e ex-cause ex-message)]
                            cause-message
                            (ex-message e))]
              [:h6 {:style {:text-align "center" :background-color "lightpink" :margin 0 :padding "0.5em 0"}}
               "Exception evaluating " [:a {:href href} [:code ":domain/code"]] ": " message])))
        ^{:key "user-iframe"}
        [user-iframe ctx]])))

#?(:cljs
   (defn left-right-editor [ctx route]
     (let [state (r/atom {:edn-fiddle false})]
       (fn [ctx route]
         [:div.row.hyperfiddle.hf-live.unp.no-gutters {:key "primary-content"}
          [:div.result.col-sm
           [:div.url-toolbar
            [:button {:class "refresh"} "↻"]
            [:input.url {:type "text" :value (domain/url-encode (runtime/domain (:peer ctx)) route)}]
            (into [:span]
                  (->> [{:label "edn" :tooltip "What the API client sees" :value :hypercrud.browser.browser-ui/api}
                        {:label "data" :tooltip "Ignore :fiddle/renderer" :value :hypercrud.browser.browser-ui/xray}
                        {:label "view" :tooltip "Use :fiddle/renderer" :value :hypercrud.browser.browser-ui/user}]
                       (map (fn [props]
                              [radio-with-label
                               (assoc props :checked (= (:value props) @(runtime/state (:peer ctx) [:display-mode]))
                                            :on-change (r/comp (r/partial runtime/dispatch! (:peer ctx)) actions/set-display-mode))]))))]
           [user-view ctx]]
          (let [as-edn (r/cursor state [:edn-fiddle])]
            [:div.src.col-sm
             [:div "Interactive Hyperfiddle editor:" [contrib.ui/easy-checkbox-boolean " edn" as-edn {:class "hyperfiddle hf-live"}]]
             [iframe-cmp ctx
              {:route (ide-route route ctx)
               :initial-tab (let [[_ _ _ frag] route] (read-fragment-only-hf-src frag))
               :user-renderer (if @as-edn result-edn fiddle-src-renderer)
               :class (css "devsrc" "hf-live")}]])]))))

#?(:cljs
   (defn view [ctx]
     (let [[fiddle :as route] (user-route ctx)]
       [:<> {:key "view-page"}
        [iframe-cmp
         (assoc ctx :hypercrud.ui/error (r/constantly ui-error/error-inline))
         {:route (topnav-route route ctx)
          :class "hidden-print"}]
        (if (magic-ide-fiddle? fiddle)
          ; tunneled ide route like /hyperfiddle.ide/domain - primary, blue background (IDE),
          ^{:key :primary-content}
          [iframe-cmp ctx {:route route :class "devsrc"}]
          [left-right-editor ctx route])])))
