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
          :uri                                              ; todo deprecate
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
  (assert (not (contains? #{:head :body} path-segment)))
  (and (or (keyword? path-segment)
           (= '* path-segment))))

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
    (keyword? segment) :attribute
    (integer? segment) :element
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

(defn set-data-source [ctx]
  ; todo why cant internals get the uri at the last second
  (assoc ctx :uri (uri ctx)))

(defn find-child-field [path-segment field]
  (->> (::field/children field)
       (filter #(= (::field/path-segment %) path-segment))
       first))

(defn find-parent-field [ctx]
  (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/field]))

(letfn [(focus-segment [ctx path-segment]                   ; attribute or fe segment
          (let [field (r/fmap (r/partial find-child-field path-segment) (:hypercrud.browser/field ctx))
                ctx (-> ctx
                        (set-parent)
                        (update :hypercrud.browser/path conj path-segment)
                        (assoc :hypercrud.browser/field field)
                        (set-data-source))]
            (if-not (:hypercrud.browser/data ctx)
              ctx                                           ; head
              (-> (set-parent-data ctx)                     ; body
                  (assoc :hypercrud.browser/data
                         (let [f (r/fmap ::field/get-value field)]
                           (assert @f (str "focusing on a non-pulled attribute: " (pr-str (:hypercrud.browser/path ctx)) "."))
                           (r/fapply f (:hypercrud.browser/data ctx))))))))]
  (defn focus "Throws if you focus a higher dimension" [ctx relative-path]
    (reduce focus-segment ctx relative-path)))

(defn row "Toggle :many into :one as we spread through the rows" [ctx rval]
  {:pre [(r/reactive? rval)]}
  (assert (= :db.cardinality/many @(r/fmap ::field/cardinality (:hypercrud.browser/field ctx)))
          (str "`context/row` is only valid on cardinality/one. current path: " (pr-str (:hypercrud.browser/path ctx))))
  (-> ctx
      (set-parent-data)
      (assoc :hypercrud.browser/data rval)
      (update :hypercrud.browser/field
              (partial r/fmap (r/partial r/last-arg-first assoc ::field/cardinality :db.cardinality/one)))))

(defn refocus "focus common ancestor" [ctx path]
  {:pre [ctx]
   :post [%
          (empty? (filter #{:head :body} (:hypercrud.browser/path %)))]}
  (assert (empty? (filter #{:head :body} path)) path)
  (assert (empty? (filter #{:head :body} (:hypercrud.browser/path ctx))) (:hypercrud.browser/path ctx))
  (let [current-path (:hypercrud.browser/path ctx)
        common-ancestor-path (ancestry-common current-path path)
        unwind-offset (- (count current-path) (count common-ancestor-path))
        common-ancestor-ctx ((apply comp (repeat unwind-offset :hypercrud.browser/parent)) ctx)]
    (focus common-ancestor-ctx (ancestry-divergence path current-path))))
