(ns hypercrud.browser.browser-ui
  (:require [cats.core :as cats :refer-macros [mlet]]
            [cats.monad.exception :as exception :refer-macros [try-on]]
            [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [hypercrud.browser.link-util :as link-util]
            [hypercrud.browser.auto-anchor :as auto-anchor]
            [hypercrud.browser.auto-anchor-formula :refer [auto-formula]]
            [hypercrud.browser.auto-anchor-txfn :refer [auto-txfn]]
            [hypercrud.browser.auto-link :as auto-link]
            [hypercrud.browser.browser-request :as browser-request]
            [hypercrud.browser.links :as links]
            [hypercrud.browser.user-bindings :as user-bindings]
            [hypercrud.client.core :as hc]
            [hypercrud.client.schema :as schema-util]
            [hypercrud.compile.eval :refer [eval-str]]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.platform.safe-render :refer [safe-user-renderer]]
            [hypercrud.types :as types]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.util :as util]))


(defn hydrate-link [peer link-dbid param-ctx]
  (if (auto-link/system-link? link-dbid)
    (let [system-link-idmap (-> link-dbid :id)]
      (->> (auto-link/request-for-system-link system-link-idmap)
           (mapv #(if % (hc/hydrate (:peer param-ctx) %)
                        (exception/success nil)))
           (reduce #(mlet [acc %1 v %2] (cats/return (conj acc v))) (exception/success []))
           (cats/fmap #(auto-link/hydrate-system-link system-link-idmap % param-ctx))))
    (hc/hydrate peer (browser-request/request-for-link link-dbid))))

(declare user-result)

(defn ui [{query-params :query-params :as params-map} param-ctx]
  (mlet [link (hydrate-link (:peer param-ctx) (:link-dbid params-map) param-ctx)
         request (try-on
                   (case (link-util/link-type link)
                     :link-query (let [link-query (:link/request link)
                                       q (some-> link-query :link-query/value reader/read-string)
                                       params-map (merge query-params (q-util/build-dbhole-lookup link-query))]
                                   (q-util/query-value q link-query params-map param-ctx))
                     :link-entity (q-util/->entityRequest (:link/request link) (:query-params params-map))
                     :link-blank nil
                     nil))
         result (if request (hc/hydrate (:peer param-ctx) request) (exception/success nil))
         ; schema is allowed to be nil if the link only has anchors and no data dependencies
         schema (exception/try-or-else (hc/hydrate (:peer param-ctx) (schema-util/schema-request nil)) nil)]
        (cats/return
          (let [indexed-schema (->> (mapv #(get % "?attr") schema) (util/group-by-assume-unique :attribute/ident))
                param-ctx (assoc param-ctx                  ; provide defaults before user-bindings run. TODO query side
                            :query-params query-params
                            :schema indexed-schema
                            :read-only (or (:read-only param-ctx) (constantly false)))

                ; ereq doesn't have a fe yet; wrap with a fe.
                ; Doesn't make sense to do on server since this is going to optimize away anyway.

                result (cond

                         (instance? types/EntityRequest request) ; But the ereq might return a vec for cardinality many
                         (cond
                           ; order matters here a lot!
                           (nil? result) nil
                           (empty? result) (if (.-a request)
                                             ; comes back as [] sometimes if cardinaltiy many request. this is causing problems as nil or {} in different places.
                                             ; Above comment seems backwards, left it as is
                                             (case (-> ((:schema param-ctx) (.-a request)) :attribute/cardinality :db/ident)
                                               :db.cardinality/one {}
                                               :db.cardinality/many []))
                           (map? result) {"entity" result}
                           (coll? result) (mapv (fn [relation] {"entity" relation}) result))


                         (instance? types/QueryRequest request)
                         (cond
                           (-> link :link/request :link-query/single-result-as-entity?) (first result)
                           :else result))

                colspec (form-util/determine-colspec result link param-ctx)
                system-anchors (auto-anchor/auto-anchors (auto-link/system-anchors link result param-ctx))]

            (case (get param-ctx :display-mode)             ; default happens higher, it influences queries too
              :user ((user-result link param-ctx) result colspec (auto-anchor/merge-anchors system-anchors (auto-anchor/auto-anchors (:link/anchor link))) (user-bindings/user-bindings link param-ctx))
              :xray (auto-control/result result colspec (auto-anchor/merge-anchors system-anchors (auto-anchor/auto-anchors (:link/anchor link))) (user-bindings/user-bindings link param-ctx))
              :root (auto-control/result result colspec system-anchors param-ctx))))))

(defn safe-ui [& args]
  ; there can be a hydrate error, or a javascript error in hyperfiddle, or
  ; a js error in a user-fn.
  (try
    (let [dom-or-e (apply ui args)]
      (if (exception/failure? dom-or-e)
        [:pre (or (-> dom-or-e .-e .-data)
                  (pr-str (-> dom-or-e .-e)))]              ; hydrate error
        (.-v dom-or-e)))                                    ; happy path
    (catch :default e                                       ; js errors? Why do we need this.
      [:pre (.-stack e)])))

(defn link-user-fn [link]
  (if-not (empty? (:link/renderer link))
    (let [{user-fn :value error :error} (eval-str (:link/renderer link))]
      (if error
        (fn [result colspec anchors param-ctx]
          [:pre (pprint/pprint error)])
        (fn [result colspec anchors param-ctx]
          [safe-user-renderer user-fn result colspec anchors param-ctx])))))

(defn user-result [link param-ctx]
  ; only need a safewrap on other people's user-fns; this context's user fn only needs the topmost safewrap.
  (let [user-fn (first (remove nil? [(:user-renderer param-ctx) (link-user-fn link) auto-control/result]))]
    (fn [result colspec anchors param-ctx]
      (let [anchor-index (->> anchors
                              (mapv (juxt #(-> % :anchor/ident) identity)) ; [ repeating entity attr ident ]
                              (into {}))
            with-inline-result (fn [ident param-ctx f]
                                 (let [anchor (get anchor-index ident)
                                       params-map (links/build-anchor-route anchor param-ctx)]
                                   @(ui params-map (assoc param-ctx :user-renderer f))))
            param-ctx (assoc param-ctx
                        :link-fn (fn [ident label param-ctx]
                                   (let [anchor (get anchor-index ident)
                                         props (-> (links/build-anchor-props anchor param-ctx)
                                                   #_(dissoc :style) #_"custom renderers don't want colored links")]
                                     [(:navigate-cmp param-ctx) props label]))
                        :with-inline-result with-inline-result
                        )]
        ; result is relation or set of relations
        (user-fn result colspec anchors param-ctx)))))