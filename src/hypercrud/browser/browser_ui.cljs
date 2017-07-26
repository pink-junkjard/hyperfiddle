(ns hypercrud.browser.browser-ui
  (:require [cats.core :as cats :refer-macros [mlet]]
            [cats.monad.either :as either]
            [hypercrud.browser.anchor :as anchor]
            [hypercrud.browser.auto-link :as auto-link]
            [hypercrud.browser.base :as base]
            [hypercrud.client.core :as hc]
            [hypercrud.client.schema :as schema-util]
            [hypercrud.compile.eval :refer [eval-str']]
            [hypercrud.platform.safe-render :refer [safe-user-renderer]]
            [hypercrud.types.EntityRequest :refer [EntityRequest]]
            [hypercrud.types.QueryRequest :refer [QueryRequest]]
            [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.util.core :refer [pprint-str]]))


(defn hydrate-link [link-dbid param-ctx]
  (if (auto-link/system-link? link-dbid)
    (let [system-link-idmap (-> link-dbid :id)]
      (->> (auto-link/request-for-system-link (:root-db param-ctx) system-link-idmap)
           (mapv #(if % (hc/hydrate (:peer param-ctx) %) (either/right nil)))
           (cats/sequence)
           (cats/fmap #(auto-link/hydrate-system-link system-link-idmap % param-ctx))))
    (hc/hydrate (:peer param-ctx) (base/meta-request-for-link (:root-db param-ctx) link-dbid))))

(declare user-result)

(defn get-ui-f [link param-ctx]
  (case (:display-mode param-ctx)
    :user (user-result link param-ctx)
    :xray auto-control/result
    :root auto-control/result))

(defn ui' [{query-params :query-params :as route} param-ctx
           ; hack for first pass on select options
           & [override-get-ui-f]]
  (let [get-ui-f (or override-get-ui-f get-ui-f)]
    (mlet [link (hydrate-link (:link-dbid route) param-ctx) ; always latest
           request (base/request-for-link link query-params param-ctx)
           result (if request (hc/hydrate (:peer param-ctx) request) (either/right nil))
           ; schema is allowed to be nil if the link only has anchors and no data dependencies
           schema (hc/hydrate (:peer param-ctx) (schema-util/schema-request (:root-db param-ctx) nil))]
      (base/process-results get-ui-f query-params link request result schema param-ctx))))

(defn ui [anchor param-ctx]
  (if (:anchor/link anchor)
    (mlet [route (anchor/build-anchor-route' anchor param-ctx)]
      (ui' route
           ; entire context must be encoded in the route
           (dissoc param-ctx :result :db :find-element :entity :attribute :value :layout :field)))
    (either/left (str "anchor, " (or (:anchor/ident anchor) (:anchor/prompt anchor)) ", has no link "))))

(defn safe [f & args]
  ; reports: hydrate failure, hyperfiddle javascript error, user-fn js error
  (try
    (either/branch
      (apply f args)
      (fn [e] [:pre (pr-str e)])
      identity)
    (catch :default e                                       ; js errors? Why do we need this.
      [:pre (.-stack e)])))

(defn safe-ui' [& args]
  (apply safe ui' args))

(defn safe-ui [& args]
  (apply safe ui args))

(defn link-user-fn [link]
  (if-not (empty? (:link/renderer link))
    (-> (eval-str' (:link/renderer link))
        (either/branch
          (fn [e] (constantly [:pre (pprint-str e)]))
          (fn [user-fn]
            (fn [result colspec anchors param-ctx] [safe-user-renderer user-fn result colspec anchors param-ctx]))))))

(defn user-result [link param-ctx]
  ; only need a safewrap on other people's user-fns; this context's user fn only needs the topmost safewrap.
  (let [user-fn (or (:user-renderer param-ctx) (link-user-fn link) auto-control/result)]
    (fn [result colspec anchors param-ctx]
      (let [anchor-index (->> anchors
                              (filter :anchor/ident)        ; cannot lookup nil idents
                              (mapv (juxt #(-> % :anchor/ident) identity)) ; [ repeating entity attr ident ]
                              (into {}))
            browse (fn [ident param-ctx f & args]
                     [safe-ui (get anchor-index ident) (assoc param-ctx :user-renderer f #_(if f #(apply f %1 %2 %3 %4 args)))])
            anchor (fn [ident param-ctx label]
                     (let [props (-> (anchor/build-anchor-props (get anchor-index ident) param-ctx)
                                     #_(dissoc :style) #_"custom renderers don't want colored links")]
                       [(:navigate-cmp param-ctx) props label]))
            browse' (fn [ident ctx]
                      (ui (get anchor-index ident) (assoc param-ctx :user-renderer identity)))
            anchor* (fn [ident ctx] (anchor/build-anchor-props (get anchor-index ident) ctx))
            param-ctx (assoc param-ctx
                        :anchor anchor
                        :browse browse
                        :anchor* anchor*
                        :browse' browse'

                        ; backwards compat
                        :with-inline-result browse
                        :link-fn (fn [ident label param-ctx] (anchor ident param-ctx label)))]
        ; result is relation or set of relations
        (user-fn result colspec anchors param-ctx)))))