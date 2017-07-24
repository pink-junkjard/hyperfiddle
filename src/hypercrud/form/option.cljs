(ns hypercrud.form.option
  (:require [cats.core :refer [mlet]]
            [cats.monad.exception :as exception :refer [success] :refer-macros [try-on]]
            [hypercrud.browser.anchor :as anchor]
            [hypercrud.browser.browser-ui :as browser-ui]
            [hypercrud.compile.eval :refer [eval-str']]
            [hypercrud.compile.eval :as eval]))

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
               (let [ident (-> attr :attribute/ident)
                     value (get-in result [(-> fe :find-element/name) ident])
                     label' (if-let [user-fn-str (eval/validate-user-code-str (-> param-ctx :fields ident :label-renderer))]
                              (mlet [user-fn (eval-str' user-fn-str)]
                                (try-on (user-fn value param-ctx)))
                              (success (default-label-renderer value param-ctx)))]
                 ; It's perfectly possible to properly report this error properly upstream.
                 (exception/extract label' (default-label-renderer value param-ctx)))))
       (interpose ", ")
       (apply str)))

(defn hydrate-options [options-anchor param-ctx]            ; needs to return options as [[:db/id label]]
  (assert options-anchor)                                   ;todo this assert should be within the exception monad
  (let [route (exception/extract (anchor/build-anchor-route' options-anchor param-ctx) nil)
        get-ui-f (fn [result colspec anchors param-ctx]
                   (->> result
                        (mapv (fn [relation]
                                (let [[conn fe attr maybe-field] (first (partition 4 colspec))
                                      entity (get relation (-> fe :find-element/name))]
                                  [(:db/id entity) (build-label colspec relation param-ctx)])))))]
    ; todo we want to at least invoke ui not ui' (missing param-ctx dissocs)
    ; probably just want callees to invoke with a custom render fn, and this calls safe-ui
    (browser-ui/ui' route param-ctx (constantly get-ui-f))))
