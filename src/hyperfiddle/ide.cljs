(ns hyperfiddle.ide
  (:require
    [cats.monad.either :as either]
    [contrib.base-64-url-safe :as base64-url-safe]
    [contrib.ednish :as ednish]
    [contrib.pprint :as pprint]
    [contrib.reader :as reader]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.runtime :as runtime]

    ; pull in the entire ide app for reference from user-land
    [hyperfiddle.ide.fiddles.fiddle-src]
    [hyperfiddle.ide.fiddles.schema-attribute]
    [hyperfiddle.ide.fiddles.topnav]
    [hyperfiddle.ide.preview.view]))


(defn parse-ide-fragment [s-fragment]
  (let [fragment (some-> s-fragment ednish/decode-ednish reader/read-edn-string+ (either/branch (constantly nil) identity))]
    (when (= "hf.src" (some-> fragment namespace))
      fragment)))

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
