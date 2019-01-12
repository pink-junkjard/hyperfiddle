(ns hyperfiddle.ide
  (:require
    [cats.monad.either :as either]
    [contrib.base-64-url-safe :as base64-url-safe]
    [contrib.css :refer [css]]
    [contrib.pprint :as pprint]
    [contrib.reactive :as r]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.routes :as routes]
    [hyperfiddle.project :as project]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui :as ui]
    [hyperfiddle.ui.iframe :refer [iframe-cmp]]
    [taoensso.timbre :as timbre]

    ; pull in the entire ide app for reference from user-land
    [hyperfiddle.ide.fiddles.fiddle-src]
    [hyperfiddle.ide.fiddles.schema]
    [hyperfiddle.ide.fiddles.schema-attribute]
    [hyperfiddle.ide.fiddles.topnav]))


(defn stateless-login-url
  ([ctx] (stateless-login-url ctx (domain/url-encode (runtime/domain (:peer ctx)) @(runtime/state (:peer ctx) [::runtime/partitions nil :route]))))
  ([ctx state]
   (let [{:keys [fqdn ide-domain] :as domain} (runtime/domain (:peer ctx))
         {:keys [domain client-id]} (get-in (domain/environment domain) [:auth0 ide-domain])]
     (str domain "/login?"
          "client=" client-id
          "&scope=" "openid email profile"
          "&state=" (base64-url-safe/encode state)
          "&redirect_uri=" (str "http://" fqdn "/auth0")))))

(defn result-edn [val ctx props]
  (let [s (-> val
              #_(as-> $ (if (seq attrs) (select-keys $ attrs) $)) ; omit elided fiddle attrs
              (pprint/pprint-str 50))]
    [contrib.ui/code (assoc props                           ; Class ends up not on the codemirror, todo
                       :value s
                       :read-only true)]))

(defn frame-on-click [rt route event]
  (when (and route (.-altKey event))                        ; under what circumstances is route nil?
    (let [native-event (.-nativeEvent event)
          anchor (-> (.composedPath native-event) (aget 0) (.matches "a"))
          anchor-descendant (-> (.composedPath native-event) (aget 0) (.matches "a *"))]
      (when-not (or anchor anchor-descendant)
        (.stopPropagation event)
        (js/window.open (domain/url-encode (runtime/domain rt) route) "_blank")))))

(defn user-iframe [ctx]
  (let [display-mode (runtime/state (:peer ctx) [:display-mode])
        ctx (assoc ctx
              :hypercrud.ui/display-mode display-mode
              :hyperfiddle.ui/debug-tooltips true
              :hyperfiddle.ui.iframe/on-click (r/partial frame-on-click (:peer ctx)))
        [_ [user-fiddle] user-args] @(:hypercrud.browser/route ctx)
        user-route (into [user-fiddle] user-args)]
    [:<>
     [:pre (pr-str user-route)]
     #_[iframe-cmp ctx
        {:route user-route
         :class (css "hyperfiddle-user"
                     "hyperfiddle-ide"
                     "hf-live"
                     (some-> @display-mode name (->> (str "display-mode-"))))}]]))

(defn user-view [ctx]
  [user-iframe ctx]
  #_(let [code-str (assert false "todo build new peer")
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
       [user-iframe ctx]]))

(defn primary-content-production [ctx]
  [:pre (js/pprint-str (domain/environment (runtime/domain (:peer ctx))))]
  #_[user-view ctx])