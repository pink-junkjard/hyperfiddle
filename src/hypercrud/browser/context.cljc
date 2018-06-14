(ns hypercrud.browser.context
  (:require
    [contrib.reactive :as r]
    [datascript.parser :as parser]
    ;[hypercrud.browser.routing :as routing]
    ;[hyperfiddle.actions :as actions]
    [hyperfiddle.runtime :as runtime]))


(defn clean [ctx]
  ; why not code-database-uri and all the custom ui/render fns?
  (dissoc ctx
          :route
          :fe-pos :uri :user-with!
          :cell-data
          :value                                            ; conflicts with browser_ui/:value
          :hyperfiddle.ui/layout

          :hypercrud.browser/attribute
          :hypercrud.browser/fat-attribute
          :hypercrud.browser/fiddle
          :hypercrud.browser/find-element
          :hypercrud.browser/links
          :hypercrud.browser/ordered-fes
          :hypercrud.browser/request
          :hypercrud.browser/result
          :relations :relation                              ; fixme
          :hypercrud.browser/schema
          :hypercrud.browser/schemas))

(defn source-mode [ctx]
  (-> ctx
      (assoc :hypercrud.browser/page-on-click (r/constantly nil) ; disable alt-click
             :hypercrud.ui/display-mode (r/track identity :hypercrud.browser.browser-ui/user))
      (update :hypercrud.browser/domain
              (fn [domain]
                (assoc-in (:hypercrud.browser/source-domain ctx) [:domain/environment "$"] (:domain/fiddle-repo domain))))))

(defn route [ctx route]                                     ; circular, this can be done sooner
  {:pre [(if-let [params (second route)] (vector? params) true) ; validate normalized already
         (some-> ctx :hypercrud.browser/domain :domain/fiddle-repo)]}
  (assoc ctx :route (hypercrud.browser.routing/tempid->id route ctx)))

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
    :entity (if-let [a @(r/cursor (:hypercrud.browser/request ctx) [:a])]
              (let [dbname @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/pull-database])]
                (case @(r/cursor (:hypercrud.browser/schemas ctx) [dbname a :db/cardinality :db/ident])
                  :db.cardinality/one
                  (relation ctx (r/fmap vector (:hypercrud.browser/result ctx)))

                  :db.cardinality/many
                  (relations ctx (r/fmap (r/partial mapv vector) (:hypercrud.browser/result ctx)))))
              (relation ctx (r/fmap vector (:hypercrud.browser/result ctx))))
    :query (condp = (query-type @(r/cursor (:hypercrud.browser/request ctx) [:query]))
             datascript.parser.FindRel (relations ctx (r/fmap (r/partial mapv vec) (:hypercrud.browser/result ctx)))
             datascript.parser.FindColl (relations ctx (r/fmap (r/partial mapv vector) (:hypercrud.browser/result ctx)))
             datascript.parser.FindTuple (relation ctx (r/fmap vec (:hypercrud.browser/result ctx)))
             datascript.parser.FindScalar (relation ctx (r/fmap vector (:hypercrud.browser/result ctx))))
    :blank ctx
    ctx))

(letfn [(user-with [rt ctx branch uri tx]
          (runtime/dispatch! rt (hyperfiddle.actions/with rt (:hypercrud.browser/invert-route ctx) branch uri tx)))]
  (defn set-find-element [ctx fe-pos]
    (let [fe (r/cursor (:hypercrud.browser/ordered-fes ctx) [fe-pos])
          dbname (str @(r/cursor fe [:source-symbol]))
          uri (when dbname
                (get-in ctx [:hypercrud.browser/domain :domain/environment dbname]))
          user-with! (r/partial user-with (:peer ctx) ctx (:branch ctx) uri)]
      (-> ctx
          (assoc :hypercrud.browser/find-element fe
                 :hypercrud.browser/schema (r/cursor (:hypercrud.browser/schemas ctx) [dbname])
                 :fe-pos fe-pos
                 :uri uri
                 :user-with! user-with!)
          (as-> ctx (case (:type @fe)
                      ; Weird that field is not reactive
                      :aggregate (assoc ctx :hypercrud.browser/field (-> @fe :fields first))
                      :variable (assoc ctx :hypercrud.browser/field (-> @fe :fields first))
                      :pull ctx))))))

(letfn [(default [default-v v] (or v default-v))]
  (defn field [ctx field]
    {:pre [(not (r/reactive? field))]}                      ; weird not reactive
    (let [attr-ident (:attribute field)
          fat-attr (->> (r/cursor (:hypercrud.browser/schema ctx) [attr-ident])
                        (r/fmap (r/partial default {:db/ident attr-ident})))]
      (assoc ctx
        :hypercrud.browser/field field
        :hypercrud.browser/attribute attr-ident
        :hypercrud.browser/fat-attribute fat-attr))))

(letfn [(set-attribute [ctx a]
          (->> @(r/cursor (:hypercrud.browser/find-element ctx) [:fields #_i])
               (filter #(= (:attribute %) a))
               first
               (field ctx)))]
  (defn focus [ctx i a]
    ;(with-relations)                                    ; already here
    ;(relation (reactive/atom [domain]))                 ; already here
    (cond-> ctx
            (and i) (set-find-element i)
            (and i a) (set-attribute a))))

(defn legacy-cell-data [ctx]
  {:pre [(:fe-pos ctx)]}
  (if (:relation ctx)
    (assoc ctx :cell-data (r/cursor (:relation ctx) [(:fe-pos ctx)]))
    ctx))

(defn legacy-value [ctx]                                    ; attr-value
  (if (and (:cell-data ctx) (not= '* (:hypercrud.browser/attribute ctx)))
    (let [rv (r/fmap (:cell-data->value (:hypercrud.browser/field ctx)) (:cell-data ctx))]
      (assoc ctx :value rv))
    ctx))

(defn legacy-ctx [ctx]                                      ; Legacy adapter for user formulas and txfns.
  (let [[i a] [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]
        ctx (if i (legacy-cell-data ctx) ctx)
        ctx (if a (legacy-value ctx) ctx)]
    ctx))

(defn entity [ctx]                                          ; misnamed, more than entity
  (let [[i a] [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    (or (if i (:cell-data (legacy-cell-data ctx)))
        (r/atom nil))))                                     ; can return nil, should return (atom nil)

(defn value "Aggregate, attr-value, etc, and can be nil."
  [ctx]
  (let [ctx (legacy-ctx ctx)
        [i a] [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    (or (if a (:value ctx))
        (if i (:cell-data ctx))
        (r/atom nil))))
