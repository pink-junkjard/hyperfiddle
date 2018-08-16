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
          :hypercrud.browser/source-symbol
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

(defn target-route [ctx] @(runtime/state (:peer ctx) [::runtime/partitions nil :route]))

(defn with-tx! [ctx tx]
  (let [uri (domain/dbname->uri (str (:hypercrud.browser/source-symbol ctx)) (:hypercrud.browser/domain ctx))
        invert-route (:hypercrud.browser/invert-route ctx)]
    (runtime/dispatch! (:peer ctx) (actions/with (:peer ctx) invert-route (:branch ctx) uri tx))))

(defn hydrate-attribute [ctx ident & ?more-path]
  (r/cursor (:hypercrud.browser/schemas ctx) (concat [(str (:hypercrud.browser/source-symbol ctx)) ident] ?more-path)))

(defn- set-parent [ctx]
  (assoc ctx :hypercrud.browser/parent (dissoc ctx :hypercrud.browser/data)))

(defn- set-parent-data [ctx]
  (update ctx :hypercrud.browser/parent (fnil into {}) (select-keys ctx [:hypercrud.browser/data])))

(defn set-data-source [ctx field]
  (if-let [source-symbol @(r/cursor field [::field/source-symbol])]
    (let [dbname (str source-symbol)
          uri (when dbname
                (domain/dbname->uri dbname (:hypercrud.browser/domain ctx)))]
      (assoc ctx
        :hypercrud.browser/source-symbol source-symbol
        ; todo why cant internals get the uri from source-symbol at the last second
        :uri uri))
    ctx))

(letfn [(find-child-field [path-segment field]
          ; find-child-field is silly;  we already map over the fields to determine which paths to focus...
          (->> (::field/children field)
               (filter #(= (::field/path-segment %) path-segment))
               first))
        (focus-segment [ctx path-segment]                   ; attribute or fe segment
          (let [field (r/fmap (r/partial find-child-field path-segment) (:hypercrud.browser/field ctx))
                ctx (-> ctx
                        (set-parent)
                        (update :hypercrud.browser/path conj path-segment)
                        (assoc :hypercrud.browser/field field)
                        (set-data-source field))]
            (if-not (:hypercrud.browser/data ctx)
              ctx                                           ; head
              (-> (set-parent-data ctx)                     ; body
                  (assoc :hypercrud.browser/data
                         (let [f (r/fmap ::field/get-value field)]
                           (assert @f (str "focusing on a non-pulled attribute: " (pr-str (:hypercrud.browser/path ctx)) "."))
                           (r/fapply f (:hypercrud.browser/data ctx))))))))]
  (defn focus [ctx relative-path]
    (reduce focus-segment ctx relative-path)))

(defn row "Toggle :many into :one as we spread through the rows" [ctx rval]
  {:pre [(r/reactive? rval)]}
  (assert (= :db.cardinality/many @(r/fmap ::field/cardinality (:hypercrud.browser/field ctx))))
  (assert (:hypercrud.browser/data ctx) (str "`:body` is invalid directly on card/many (do you need a table wrap?). current path: " (pr-str (:hypercrud.browser/path ctx))))
  (assert (r/reactive? (:hypercrud.browser/data ctx)))
  (-> ctx
      (set-parent-data)
      (assoc :hypercrud.browser/data rval)
      (update :hypercrud.browser/field
              (partial r/fmap (r/partial r/last-arg-first assoc ::field/cardinality :db.cardinality/one)))))

(defn refocus "focus common ancestor" [ctx path]
  {:pre [ctx] :post [%]}
  (let [current-path (:hypercrud.browser/path ctx)
        common-ancestor-path (ancestry-common current-path path)
        unwind-offset (- (count current-path) (count common-ancestor-path))
        common-ancestor-ctx ((apply comp (repeat unwind-offset :hypercrud.browser/parent)) ctx)]
    (focus common-ancestor-ctx (ancestry-divergence path current-path))))
