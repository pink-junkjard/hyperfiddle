(ns hypercrud.browser.browser-ui
  (:require [cats.core :as cats :refer-macros [mlet]]
            [cats.monad.exception :as exception :refer-macros [try-on]]
            [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [hypercrud.browser.link-util :as link-util]
            [hypercrud.browser.auto-anchor :as auto-anchor]
            [hypercrud.browser.auto-link :as auto-link]
            [hypercrud.browser.browser-request :as browser-request]
            [hypercrud.browser.anchor :as anchor]
            [hypercrud.browser.user-bindings :as user-bindings]
            [hypercrud.client.core :as hc]
            [hypercrud.client.reagent :refer [connect]]
            [hypercrud.client.schema :as schema-util]
            [hypercrud.compile.eval :refer [eval-str]]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.platform.safe-render :refer [safe-user-renderer]]
            [hypercrud.types.EntityRequest :refer [EntityRequest]]
            [hypercrud.types.QueryRequest :refer [QueryRequest]]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.util.core :as util]))


(defn with-hydrated-link [link-dbid param-ctx comp]
  (connect {:dbval [[hc/*root-conn-id* nil]]}
           (fn [{[root-db] :dbval}]
             (if (auto-link/system-link? link-dbid)
               (let [system-link-idmap (-> link-dbid :id)]
                 (connect {:hydrate (auto-link/request-for-system-link root-db system-link-idmap)}
                          (fn [results]
                            (let [r (reduce #(mlet [acc %1 v %2] (cats/return (conj acc v)))
                                            (exception/success [])
                                            (:hydrate results))]
                              [comp (cats/fmap #(auto-link/hydrate-system-link system-link-idmap % #_[owner conn e a] param-ctx) r)])))))
             (connect {:hydrate [(browser-request/request-for-link root-db link-dbid)]}
                      (fn [{[link] :hydrate}] [comp link])))))

(declare user-result)

(defn safe [f & args]
  ; reports: hydrate failure, hyperfiddle javascript error, user-fn js error
  (try
    (let [dom-or-e (apply f args)]
      (if (exception/failure? dom-or-e)
        [:pre (or (-> dom-or-e .-e .-data)
                  (pr-str (-> dom-or-e .-e)))]              ; hydrate error
        (.-v dom-or-e)))                                    ; happy path
    (catch :default e                                       ; js errors? Why do we need this.
      [:pre (.-stack e)])))

(def never-read-only (constantly false))

(defn ui-thing [result link param-ctx]
  (js/console.log (str "render: ui-thing[" (:debug param-ctx) "]"))
  (let [colspec (form-util/determine-colspec result link param-ctx)
        system-anchors (auto-anchor/auto-anchors (auto-link/system-anchors link result param-ctx))]
    (case (:display-mode param-ctx)                         ; default happens higher, it influences queries too
      :user ((user-result link param-ctx) result colspec (auto-anchor/merge-anchors system-anchors (auto-anchor/auto-anchors (:link/anchor link))) (user-bindings/user-bindings link param-ctx))
      :xray (auto-control/result result colspec (auto-anchor/merge-anchors system-anchors (auto-anchor/auto-anchors (:link/anchor link))) (user-bindings/user-bindings link param-ctx))
      :root (auto-control/result result colspec system-anchors param-ctx))))

(defn query-ui [link param-ctx]
  (let [link-query (:link/request link)
        q (some-> link-query :link-query/value reader/read-string)]
    (q-util/->queryRequest-connect q link-query param-ctx
                                   (fn [request]
                                     (connect {:hydrate [request]}
                                              (fn [{[result] :hydrate}]
                                                (safe (fn [] (cats/fmap (fn [result]
                                                                          (let [result (if (-> link :link/request :link-query/single-result-as-entity?)
                                                                                         (first result)
                                                                                         result)]
                                                                            (ui-thing result link param-ctx)))
                                                                        result)))))))))

(defn entity-ui [link param-ctx]
  (q-util/->entityRequest-connect (:link/request link) param-ctx
                                  (fn [request]
                                    (connect {:hydrate [request]}
                                             (fn [{[result] :hydrate}]
                                               (safe (fn [] (cats/fmap (fn [result]
                                                                         ; ereq doesn't have a fe yet; wrap with a fe.
                                                                         ; Doesn't make sense to do on server since this is going to optimize away anyway.
                                                                         (let [result (cond
                                                                                        ; order matters here a lot!
                                                                                        (nil? result) nil
                                                                                        (empty? result) (if (.-a request)
                                                                                                          ; comes back as [] sometimes if cardinaltiy many request. this is causing problems as nil or {} in different places.
                                                                                                          ; Above comment seems backwards, left it as is
                                                                                                          (case (-> ((:schema param-ctx) (.-a request)) :attribute/cardinality :db/ident)
                                                                                                            :db.cardinality/one {}
                                                                                                            :db.cardinality/many []))
                                                                                        (map? result) {"entity" result}
                                                                                        (coll? result) (mapv (fn [relation] {"entity" relation}) result))]
                                                                           (ui-thing result link param-ctx)))
                                                                       result))))))))

(defn nil-ui [link param-ctx]
  (safe exception/success (ui-thing nil link param-ctx)))

(defn ui-for-link [query-params link param-ctx]
  (connect {:dbval [[hc/*root-conn-id* nil]]}
           (fn [{[root-db] :dbval}]
             (let [schema-request (schema-util/schema-request root-db nil)]
               (connect {:hydrate [schema-request]}
                        (fn [{[schema] :hydrate}]
                          (safe (fn []
                                  (cats/fmap (fn [schema]
                                               (let [indexed-schema (->> (mapv #(get % "?attr") schema) (util/group-by-assume-unique :attribute/ident))
                                                     param-ctx (assoc param-ctx ; provide defaults before user-bindings run. TODO query side
                                                                 :query-params query-params
                                                                 :schema indexed-schema
                                                                 :read-only (or (:read-only param-ctx) never-read-only))
                                                     f (case (link-util/link-type link)
                                                         :link-query query-ui
                                                         :link-entity entity-ui
                                                         :link-blank nil-ui
                                                         nil-ui)]
                                                 (f link param-ctx)))
                                             schema)))))))))

(defn ui' [{query-params :query-params :as route} param-ctx]
  (with-hydrated-link (:link-dbid route) param-ctx
                      (fn [link]
                        (safe (fn []
                                (cats/fmap (fn [link]
                                             (ui-for-link query-params link param-ctx))
                                           link))))))

(defn ui [anchor param-ctx]
  (if (:anchor/link anchor)
    (ui' (anchor/build-anchor-route anchor param-ctx)
         ; entire context must be encoded in the route
         (dissoc param-ctx :result :db :find-element :entity :attribute :value :layout))
    (exception/failure "anchor has no link")))

(defn safe-ui' [& args]
  (apply ui' args))

(defn safe-ui [& args]
  (apply ui args))

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
                                 @(ui (get anchor-index ident) (assoc param-ctx :user-renderer f)))
            param-ctx (assoc param-ctx
                        :link-fn (fn [ident label param-ctx]
                                   (let [anchor (get anchor-index ident)
                                         props (-> (anchor/build-anchor-props anchor param-ctx)
                                                   #_(dissoc :style) #_"custom renderers don't want colored links")]
                                     [(:navigate-cmp param-ctx) props label]))
                        :with-inline-result with-inline-result
                        )]
        ; result is relation or set of relations
        (user-fn result colspec anchors param-ctx)))))