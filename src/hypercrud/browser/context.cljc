(ns hypercrud.browser.context
  (:require
    [contrib.data :refer [ancestry-common ancestry-divergence]]
    [contrib.reactive :as r]
    [hypercrud.browser.field :as field]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.runtime :as runtime]))


(defn clean [ctx]
  ; why not code-database-uri and all the custom ui/render fns?
  (dissoc ctx
          :route                                            ; Route is unrelated to the hyper-control ontology
          :hypercrud.ui/error
          :hyperfiddle.ui/layout

          :hypercrud.browser/data
          :hypercrud.browser/fiddle
          :hypercrud.browser/field
          :hypercrud.browser/links
          :hypercrud.browser/parent
          :hypercrud.browser/path
          :hypercrud.browser/schemas))

(defn source-mode [ctx]
  (-> ctx
      (assoc :hyperfiddle.ui/iframe-on-click (r/constantly nil) ; disable alt-click
             :hypercrud.ui/display-mode (r/track identity :hypercrud.browser.browser-ui/user))
      (update :hypercrud.browser/domain
              (fn [domain]
                (update (:hypercrud.browser/source-domain ctx) :domain/databases
                        (fn [dbs]
                          (->> dbs
                               (remove #(= "$" (:domain.database/name %)))
                               (cons {:domain.database/name "$"
                                      :domain.database/record (:domain/fiddle-database domain)})
                               vec)))))))

(defn attribute-segment? [path-segment]
  (or (keyword? path-segment)
      (= '* path-segment)))

(defn find-element-segment? [path-segment]
  (integer? path-segment))

(defn segment-type [segment]
  (cond
    (attribute-segment? segment) :attribute
    (find-element-segment? segment) :element
    :else :naked))

(defn segment-type-2 [segment]
  (cond
    (= '* segment) :splat
    (= :db/id segment) :db/id
    (keyword? segment) :attribute
    (integer? segment) :element                             ; can be a variable element, an aggregate element, etc
    ; it can also be entity-[], which has implied :element, this also happens in the query [?e ...] case
    :else :naked-or-element))

(defn target-route [ctx] @(runtime/state (:peer ctx) [::runtime/partitions nil :route]))

(defn dbname [ctx] (some->> (:hypercrud.browser/field ctx) (r/fmap ::field/source-symbol) deref str))

(defn uri
  ([ctx] (uri (dbname ctx) ctx))
  ([dbname ctx] (some-> dbname (domain/dbname->uri (:hypercrud.browser/domain ctx)))))

(defn with-tx! [ctx tx]
  (let [uri (uri ctx)
        _ (assert uri)                                      ; todo downstream action should be validating this
        invert-route (:hypercrud.browser/invert-route ctx)]
    (runtime/dispatch! (:peer ctx) (actions/with (:peer ctx) invert-route (:branch ctx) uri tx))))

(defn hydrate-attribute [ctx ident & ?more-path]
  (r/cursor (:hypercrud.browser/schemas ctx) (concat [(dbname ctx) ident] ?more-path)))

(defn- set-parent [ctx]
  (assoc ctx :hypercrud.browser/parent (dissoc ctx :hypercrud.browser/data)))

(defn- set-parent-data [ctx]
  (update ctx :hypercrud.browser/parent (fnil into {}) (select-keys ctx [:hypercrud.browser/data])))

(defn find-child-field [path-segment field]
  (->> (::field/children field)
       (filter #(= (::field/path-segment %) path-segment))
       first))

(defn find-parent-field [ctx]
  (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/field]))

(letfn [(focus-segment [ctx path-segment]                   ; attribute or fe segment
          #_(assert (or (not (:hypercrud.browser/data ctx))   ; head has no data, can focus without calling row
                      (not= :db.cardinality/many @(r/fmap ::field/cardinality (:hypercrud.browser/field ctx))))
                  (str "Cannot focus directly from a cardinality/many (do you need a table wrap?). current path: " (:hypercrud.browser/path ctx) ", attempted segment: " path-segment))
          (let [field (r/fmap (r/partial find-child-field path-segment) (:hypercrud.browser/field ctx))
                ctx (-> ctx
                        (set-parent)
                        (update :hypercrud.browser/path conj path-segment)
                        (assoc :hypercrud.browser/field field))]
            (if-not (:hypercrud.browser/data ctx)
              ctx                                           ; head
              (-> (set-parent-data ctx)                     ; body
                  (assoc :hypercrud.browser/data
                         (let [f (r/fmap ::field/get-value field)]
                           #_(assert @f (str "focusing on a non-pulled attribute: " (pr-str (:hypercrud.browser/path ctx)) "."))
                           (r/fapply f (:hypercrud.browser/data ctx))))))))]
  (defn focus "Throws if you focus a higher dimension" [ctx relative-path]
    (reduce focus-segment ctx relative-path)))

(defn row "Toggle :many into :one as we spread through the rows" [ctx rval]
  {:pre [(r/reactive? rval)]}
  (assert (= :db.cardinality/many @(r/fmap ::field/cardinality (:hypercrud.browser/field ctx)))
          (str "`context/row` is only valid on cardinality/many. current path: " (pr-str (:hypercrud.browser/path ctx))))
  (-> ctx
      (set-parent)
      (set-parent-data)
      (assoc :hypercrud.browser/data rval)
      (update :hypercrud.browser/field
              (partial r/fmap (r/partial r/last-arg-first assoc ::field/cardinality :db.cardinality/one)))))

(defn refocus "focus common ancestor" [ctx path]
  {:pre [ctx]
   :post [%]}
  (let [current-path (:hypercrud.browser/path ctx)
        common-ancestor-path (ancestry-common current-path path)
        unwind-offset (- (count current-path) (count common-ancestor-path))
        common-ancestor-ctx ((apply comp (repeat unwind-offset :hypercrud.browser/parent)) ctx)]
    (focus common-ancestor-ctx (ancestry-divergence path current-path))))

(defn identity [ctx]
  ; When looking at an attr of type ref, figure out it's identity, based on all the ways it can be pulled.
  ; What if we pulled children without identity? Then we can't answer the question (should assert this)
  (or
    @(contrib.reactive/cursor (:hypercrud.browser/data ctx) [:db/ident])
    @(contrib.reactive/cursor (:hypercrud.browser/data ctx) [:db/id])
    @(:hypercrud.browser/data ctx)))
