(ns hypercrud.browser.context
  (:require [contrib.data :refer [unwrap]]
            [contrib.reactive :as r]
            [contrib.try :refer [try-either]]
            [datascript.parser :as parser]
            [hypercrud.browser.routing :as routing]
            [hyperfiddle.foundation.actions :as foundation-actions]
            [hyperfiddle.runtime :as runtime]))


(defn clean [ctx]
  ; why not code-database-uri and all the custom ui/render fns?
  (dissoc ctx
          :route
          :fe-pos :uri :user-with!
          :cell-data :value
          :layout :field
          :label

          :hypercrud.browser/cell                           ; TODO :cell should cascade
          :hypercrud.browser/attribute
          :hypercrud.browser/fat-attribute
          :hypercrud.browser/fiddle
          :hypercrud.browser/find-element
          :hypercrud.browser/links
          :hypercrud.browser/ordered-fes
          :hypercrud.browser/request
          :hypercrud.browser/result
          :relations :relation
          :hypercrud.browser/schema
          :hypercrud.browser/schemas))

(defn route [ctx route]
  {:pre [(if-let [params (second route)] (vector? params) true) ; validate normalized already
         (some-> ctx :hypercrud.browser/domain :domain/fiddle-repo)]}
  (assoc ctx :route (routing/tempid->id route ctx)))

(defn relations [ctx rv]
  {:pre [(r/reactive? rv)]}
  (assoc ctx :relations rv))

(defn relation [ctx rv]
  {:pre [(r/reactive? rv)]}
  ; (assoc ctx :relation @(reactive/cursor (:relations ctx) [i]))
  ; Break the pattern - :relations is not in scope in form case which is a bit of information.
  (assoc ctx :relation rv))

(defn- query-type [query]
  (-> (parser/parse-query query)
      :qfind
      type))

(defn with-relations "Process results into a relation or list of relations"
  [ctx]
  {:pre [(nil? (:relations ctx)) (nil? (:relation ctx))]}
  (case @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/type]) ; fiddle/type not relevant outside this fn
    :entity (if-not @(r/fmap nil? (r/cursor (:hypercrud.browser/request ctx) [:a]))
              (let [[_ [e a]] (get-in ctx [:route])]
                (let [source-symbol (unwrap (try-either (.-dbname e)))] ; make failure impossible
                  (case @(r/cursor (:hypercrud.browser/schemas ctx) [(str source-symbol) a :db/cardinality :db/ident])
                    :db.cardinality/one
                    (relation ctx (r/fmap vector (:hypercrud.browser/result ctx)))

                    :db.cardinality/many
                    (relations ctx (r/fmap (r/partial mapv vector) (:hypercrud.browser/result ctx))))))
              (relation ctx (r/fmap vector (:hypercrud.browser/result ctx))))
    :query (condp = (query-type @(r/cursor (:hypercrud.browser/request ctx) [:query]))
             datascript.parser.FindRel (relations ctx (r/fmap (r/partial mapv vec) (:hypercrud.browser/result ctx)))
             datascript.parser.FindColl (relations ctx (r/fmap (r/partial mapv vector) (:hypercrud.browser/result ctx)))
             datascript.parser.FindTuple (relation ctx (r/fmap vec (:hypercrud.browser/result ctx)))
             datascript.parser.FindScalar (relation ctx (r/fmap vector (:hypercrud.browser/result ctx))))
    :blank ctx
    ctx))

(letfn [(user-with [rt ctx branch uri tx]
          (runtime/dispatch! rt (foundation-actions/with rt (:hypercrud.browser/invert-route ctx) branch uri tx)))]
  (defn find-element [ctx fe-pos]
    (let [fe (r/cursor (:hypercrud.browser/ordered-fes ctx) [fe-pos])
          dbname (str @(r/cursor fe [:source-symbol]))
          uri (when dbname
                (get-in ctx [:hypercrud.browser/domain :domain/environment dbname]))
          user-with! (r/partial user-with (:peer ctx) ctx (:branch ctx) uri)]
      (assoc ctx
        :hypercrud.browser/find-element fe
        :hypercrud.browser/schema (r/cursor (:hypercrud.browser/schemas ctx) [dbname])
        :fe-pos fe-pos
        :uri uri
        :user-with! user-with!))))

(let [f (fn [cell-data ctx]
          (if-let [owner-fn (:owner-fn ctx)]
            (owner-fn @cell-data ctx)))]
  (defn cell-data [ctx]                                     ; "dependent"
    {:pre [(:fe-pos ctx)]}
    (let [cell-data (r/cursor (:relation ctx) [(:fe-pos ctx)])]
      (assoc ctx :owner (r/track f cell-data ctx)
                 :cell-data cell-data))))

(letfn [(default [default-v v] (or v default-v))]
  (defn field [ctx field]
    {:pre [(not (r/reactive? field))]}
    (let [attr-ident (:attribute field)
          fat-attr (->> (r/cursor (:hypercrud.browser/schema ctx) [attr-ident])
                        (r/fmap (r/partial default {:db/ident attr-ident})))]
      (assoc ctx
        :hypercrud.browser/field field
        :hypercrud.browser/attribute attr-ident
        :hypercrud.browser/fat-attribute fat-attr))))

(defn value [ctx rv]
  {:pre [(r/reactive? rv)]}
  (assoc ctx :value rv))

(defn -field-getter-dumb [ctx a]
  #_(r/cursor (:hypercrud.browser/find-element ctx) [:fields i])
  (->> @(r/cursor (:hypercrud.browser/find-element ctx) [:fields])
       (filter #(= (:attribute %) a))
       first))

(letfn [(get-value-f [attr fields]
          (->> fields
               (filter #(= (:attribute %) attr))
               first
               :cell-data->value))]
  (defn relation-path [ctx [dependent i a]]
    (as-> ctx ctx
          (assoc ctx :layout :block)
          ;(with-relations)                                    ; already here
          ;(relation (reactive/atom [domain]))                 ; already here
          (if (and i) (find-element ctx i) ctx)
          (if (and i dependent) (cell-data ctx) ctx)
          (if (and i a) (field ctx (-field-getter-dumb ctx a)) ctx)
          (if (and i dependent a)
            (let [cell-extractor @(->> (r/cursor (:hypercrud.browser/find-element ctx) [:fields])
                                       (r/fmap (r/partial get-value-f a)))]
              (value ctx (r/fmap cell-extractor (:cell-data ctx))))
            ctx))))
