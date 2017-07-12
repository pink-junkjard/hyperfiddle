(ns hypercrud.form.option
  (:require [cats.core :as cats :refer-macros [mlet]]
            [cats.monad.exception :as exception]
            [cljs.reader :as reader]
            [hypercrud.browser.browser-request :as browser-request]
            [hypercrud.browser.anchor :as anchor]
            [hypercrud.browser.user-bindings :as user-bindings]
            [hypercrud.client.core :as hc]
            [hypercrud.client.schema :as schema-util]
            [hypercrud.compile.eval :refer [eval-str]]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.util.core :as util]))

(defn default-label-renderer [v]
  (cond
    (instance? cljs.core/Keyword v) (name v)
    :else (str v))
  #_(condp (fn [x c] (instance? c x)) v
      cljs.core/Keyword (name v)
      (str v)))

(defn build-label [colspec result param-ctx]
  (->> (partition 4 colspec)
       (mapv (fn [[conn fe attr maybe-field]]
               ; Custom label renderers? Can't use the attribute renderer, since that
               ; is how we are in a select options in the first place.
               (let [ident (-> attr :attribute/ident)
                     value (get-in result [(-> fe :find-element/name) ident])
                     user-renderer (-> param-ctx :fields ident :label-renderer)
                     {f :value error :error} (if-not (empty? user-renderer) (eval-str user-renderer))]
                 (if error (.warn js/console (str "Bad label rendererer " user-renderer)))
                 (if-not f
                   (default-label-renderer value)
                   (try
                     (f value)
                     (catch js/Error e
                       (.warn js/console "user error in label-renderer: " (str e))
                       (default-label-renderer value)))))))
       (interpose ", ")
       (apply str)))

; todo should use upcoming ui/request unified abstraction
(defn hydrate-options [options-anchor param-ctx]            ; needs to return options as [[:db/id label]]
  (assert options-anchor)
  ; This needs to be robust to partially constructed anchors
  (let [link (let [request (if-let [link (-> options-anchor :anchor/link :db/id)]
                             (browser-request/request-for-link (:root-db param-ctx) link))
                   resp (if request (hc/hydrate (:peer param-ctx) request))]
               (exception/extract resp nil))
        route (anchor/build-anchor-route options-anchor param-ctx)]
    ; we are assuming we have a query link here
    (mlet [q (if-let [qstr (-> link :link/request :link-query/value)] ; We avoid caught exceptions when possible
               (exception/try-on (reader/read-string qstr))
               (exception/failure nil))                     ; is this a success or failure? Doesn't matter - datomic will fail.
           result (let [params-map (merge (:query-params route) (q-util/build-dbhole-lookup (:link/request link) param-ctx))
                        query-value (q-util/->queryRequest q (:link/request link) params-map param-ctx)]
                    (hc/hydrate (:peer param-ctx) query-value))
           ; schema is allowed to be nil if the link only has anchors and no data dependencies
           schema (exception/try-or-else (hc/hydrate (:peer param-ctx) (schema-util/schema-request (:root-db param-ctx) nil)) nil)]
          (let [indexed-schema (->> (mapv #(get % "?attr") schema) (util/group-by-assume-unique :attribute/ident))
                colspec (form-util/determine-colspec result link indexed-schema param-ctx)
                ; options have custom renderers which get user bindings
                param-ctx (user-bindings/user-bindings link param-ctx)]
            (cats/return
              (->> result
                   (mapv (fn [relation]
                           (let [[conn fe attr maybe-field] (first (partition 4 colspec))
                                 entity (get relation (-> fe :find-element/name))]
                             [(:db/id entity) (build-label colspec relation param-ctx)])))))))))
