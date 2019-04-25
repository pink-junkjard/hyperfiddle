(ns hyperfiddle.ide
  (:require
    [cats.monad.either :as either]
    [contrib.base-64-url-safe :as base64-url-safe]
    [contrib.data :refer [unqualify]]
    [contrib.ednish :as ednish]
    [contrib.reader :as reader]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui.staging]

    ; pull in the entire ide app for reference from user-land
    [hyperfiddle.ide.edit]
    [hyperfiddle.ide.fiddles.fiddle-src]
    [hyperfiddle.ide.fiddles.schema-attribute]
    [hyperfiddle.ide.fiddles.schema-editor]
    [hyperfiddle.ide.fiddles.topnav]
    [hyperfiddle.ide.preview.view]))


(defn parse-ide-fragment [s-fragment]
  (let [fragment (some-> s-fragment ednish/decode-ednish reader/read-edn-string+ (either/branch (constantly nil) identity))]
    (when (and (keyword? fragment)
               (#{"hf.src" "hf"} (namespace fragment)))
      (keyword "hf" (unqualify fragment)))))

(defn stateless-login-url
  ([ctx] (stateless-login-url ctx (domain/url-encode (runtime/domain (:peer ctx)) @(runtime/state (:peer ctx) [::runtime/partitions foundation/root-branch :route]))))
  ([ctx state]
   (let [{:keys [hyperfiddle.ide.directory/service-uri
                 hyperfiddle.ide.directory/ide-domain] :as domain} (runtime/domain (:peer ctx))
         {:keys [domain client-id]} (get-in (domain/environment domain) [:auth0 ide-domain])
         redirect-uri (str service-uri (domain/api-path-for (runtime/domain (:peer ctx)) :hyperfiddle.ide/auth0-redirect))]
     (str domain "/login?"
          "client=" client-id
          "&scope=" "openid email profile"
          "&state=" (base64-url-safe/encode state)
          "&redirect_uri=" redirect-uri))))

(defn ide-stage [ctx]
  ; Could also be inlined:
  ; http://hyperfiddle.hyperfiddle.site/:hyperfiddle.ide!edit/(:fiddle!ident,:hyperfiddle.ide!edit)
  [hyperfiddle.ui.staging/inline-stage ctx
   (->> (hyperfiddle.runtime/domain (:peer ctx))
        :hyperfiddle.ide.domain/user-dbname->ide
        (map (fn [[user-dbname ide-dbname]] {:id ide-dbname
                                             :label (domain/dbname-label user-dbname)}))
        (sort-by :label))])
