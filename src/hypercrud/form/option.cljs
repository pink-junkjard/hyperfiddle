(ns hypercrud.form.option
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [cats.monad.exception :as exception]
            [hypercrud.browser.anchor :as anchor]
            [hypercrud.browser.browser-ui :as browser-ui]
            [hypercrud.browser.context :as context]
            [hypercrud.util.monad :refer [exception->either]]))

(defn default-label-renderer [v ctx]
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
               (let [ident (-> attr :db/ident)
                     value (get-in result [(-> fe :find-element/name) ident])
                     renderer (or (-> param-ctx :fields ident :label-renderer) default-label-renderer)]
                 (-> (exception/try-on (renderer value param-ctx))
                     exception->either))))
       (cats/sequence)
       (cats/fmap (fn [labels]
                    (->> labels
                         (interpose ", ")
                         (apply str))))))

(defn options-ui-f [result colspec anchors param-ctx]
  (->> result
       (mapv (fn [relation]
               (let [[conn fe attr maybe-field] (first (partition 4 colspec))
                     entity (get relation (-> fe :find-element/name))
                     label (-> (build-label colspec relation param-ctx)
                               ; It's perfectly possible to properly report this error properly upstream.
                               (either/branch (fn [e] (pr-str e)) identity))]
                 [(:db/id entity) label])))))

(defn hydrate-options' [options-anchor param-ctx]           ; needs to return options as [[:db/id label]]
  (assert options-anchor)                                   ;todo this assert should be within the exception monad
  (mlet [route (anchor/build-anchor-route' options-anchor param-ctx)]
    ; todo we want to at least invoke ui not ui'
    ; probably just want callees to invoke with a custom render fn, and this calls safe-ui
    (browser-ui/ui' route (context/clean param-ctx) (constantly options-ui-f))))
