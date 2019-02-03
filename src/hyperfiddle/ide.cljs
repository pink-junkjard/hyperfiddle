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
    [hyperfiddle.ide.fiddles.schema-attribute]
    [hyperfiddle.ide.fiddles.topnav]
    [hyperfiddle.ide.preview.view]))


(defn stateless-login-url
  ([ctx] (stateless-login-url ctx (domain/url-encode (runtime/domain (:peer ctx)) @(runtime/state (:peer ctx) [::runtime/partitions nil :route]))))
  ([ctx state]
   (let [{:keys [hyperfiddle.ide/fqdn ide-domain] :as domain} (runtime/domain (:peer ctx))
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

(defn user-view [ctx]
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
