(ns hypercrud.browser.context
  (:require
    [contrib.data :refer [unwrap ancestry-common ancestry-divergence]]
    [contrib.reactive :as r]
    [contrib.string :refer [memoized-safe-read-edn-string]]
    [datascript.parser :as parser]
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

          :hypercrud.browser/attribute
          :hypercrud.browser/data
          :hypercrud.browser/data-cardinality
          :hypercrud.browser/fiddle
          :hypercrud.browser/field
          :hypercrud.browser/fields
          :hypercrud.browser/links
          :hypercrud.browser/parent
          :hypercrud.browser/path
          :hypercrud.browser/result
          :hypercrud.browser/source-symbol
          :hypercrud.browser/schemas))

(defn source-mode [ctx]
  (-> ctx
      (assoc :hypercrud.browser/page-on-click (r/constantly nil) ; disable alt-click
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
  (and (not (contains? #{:head :body :naked} path-segment))        ; todo cannot conflict with :db/ident :head | :body
       (or (keyword? path-segment)
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

(def data-keys [:hypercrud.browser/data :hypercrud.browser/data-cardinality])

(defn- set-parent [ctx]
  (assoc ctx :hypercrud.browser/parent (dissoc ctx data-keys)))

(defn- set-parent-data [ctx]
  (update ctx :hypercrud.browser/parent (fnil into {}) (select-keys ctx data-keys)))

(defn body [ctx & [?data]]
  (case (:hypercrud.browser/data-cardinality ctx)
    :db.cardinality/one (assert (nil? ?data))
    :db.cardinality/many (do (assert ?data (str "`:body` is invalid directly on card/many (do you need a table wrap?). current path: " (pr-str (:hypercrud.browser/path ctx))))
                             (assert (r/reactive? ?data))))
  (let [new-ctx (-> ctx
                    (set-parent)
                    (set-parent-data)
                    (assoc :hypercrud.browser/data-cardinality
                           (if (empty? (:hypercrud.browser/path ctx))
                             :db.cardinality/many           ; list of fe's when setting from results
                             :db.cardinality/one            ; always one map when nested data
                             ))
                    (update :hypercrud.browser/path conj :body)
                    (dissoc :hypercrud.browser/attribute
                            :hypercrud.browser/field))]
    (case (:hypercrud.browser/data-cardinality ctx)
      :db.cardinality/one new-ctx
      :db.cardinality/many (assoc new-ctx :hypercrud.browser/data ?data))))

(letfn [(set-data [ctx cardinality]
          (-> ctx
              (set-parent-data)
              (assoc
                :hypercrud.browser/data (let [f (r/fmap ::field/get-value (:hypercrud.browser/field ctx))]
                                          (assert @f (str "focusing on a non-pulled attribute: " (pr-str (:hypercrud.browser/path ctx)) "."))
                                          (r/fapply f (:hypercrud.browser/data ctx)))
                :hypercrud.browser/data-cardinality cardinality)))
        (query-type [s-query]
          (-> (memoized-safe-read-edn-string s-query)
              unwrap                                        ; known good if we got this far
              parser/parse-query :qfind type))
        (initial-focus [ctx]
          (case @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/type]) ; fiddle/type not relevant outside this fn
            :entity (assoc ctx
                      :hypercrud.browser/data (r/fmap vector (:hypercrud.browser/result ctx))
                      :hypercrud.browser/data-cardinality :db.cardinality/one
                      :hypercrud.browser/path [])
            :query (condp = (query-type @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/query]))
                     datascript.parser.FindRel
                     (assoc ctx
                       :hypercrud.browser/data (r/fmap (r/partial mapv vec) (:hypercrud.browser/result ctx))
                       :hypercrud.browser/data-cardinality :db.cardinality/many
                       :hypercrud.browser/path [])

                     datascript.parser.FindColl
                     (assoc ctx
                       :hypercrud.browser/data (r/fmap (r/partial mapv vector) (:hypercrud.browser/result ctx))
                       :hypercrud.browser/data-cardinality :db.cardinality/many
                       :hypercrud.browser/path [])

                     datascript.parser.FindTuple
                     (assoc ctx
                       :hypercrud.browser/data (r/fmap vec (:hypercrud.browser/result ctx))
                       :hypercrud.browser/data-cardinality :db.cardinality/one
                       :hypercrud.browser/path [])

                     datascript.parser.FindScalar
                     (assoc ctx
                       :hypercrud.browser/data (r/fmap vector (:hypercrud.browser/result ctx))
                       :hypercrud.browser/data-cardinality :db.cardinality/one
                       :hypercrud.browser/path []))
            :blank ctx
            ctx))
        ; find-field is silly;  we already map over the fields to determine which paths to focus...
        (find-field [path-segment fields] (first (filter #(= (::field/path-segment %) path-segment) fields)))
        (focus-segment [ctx path-segment]
          (cond
            (and (= path-segment :naked))
            (-> ctx
                (assoc :hypercrud.browser/parent ctx)
                (dissoc :hypercrud.browser/attribute
                        :hypercrud.browser/data
                        :hypercrud.browser/data-cardinality
                        :hypercrud.browser/field)
                (update :hypercrud.browser/path conj :naked)
                (assoc :hypercrud.browser/attribute :naked))

            ; todo cannot conflict with :db/ident :head
            (and (= path-segment :head) #_(nil? (:hypercrud.browser/data-cardinality ctx)))
            (-> ctx
                (assoc :hypercrud.browser/parent ctx #_(select-keys ctx [:hypercrud.browser/data
                                                                   :hypercrud.browser/data-cardinality
                                                                   :hypercrud.browser/path]))
                (update :hypercrud.browser/path conj :head)
                (dissoc :hypercrud.browser/attribute
                        :hypercrud.browser/data
                        :hypercrud.browser/data-cardinality
                        :hypercrud.browser/field))

            ; todo cannot conflict with :db/ident :body
            (and (= path-segment :body) #_(not= :db.cardinality/one (:hypercrud.browser/data-cardinality ctx)))
            (if (nil? (:hypercrud.browser/data ctx))
              (initial-focus ctx)                           ; todo this isn't :body, this is :hypercrud.browser/result
              (body ctx))

            (attribute-segment? path-segment)
            (let [field (r/fmap (r/partial find-field path-segment) (:hypercrud.browser/fields ctx))]
              (cond-> (-> ctx
                          (set-parent)
                          (update :hypercrud.browser/path conj path-segment)
                          (assoc
                            :hypercrud.browser/attribute path-segment
                            :hypercrud.browser/field field
                            :hypercrud.browser/fields (r/fmap ::field/children field)))
                ; if we are in a head, we dont have data to set
                (not (some #{:head} (:hypercrud.browser/path ctx))) (set-data @(hydrate-attribute ctx path-segment :db/cardinality :db/ident))))

            (find-element-segment? path-segment)
            (let [field (r/fmap (r/partial find-field path-segment) (:hypercrud.browser/fields ctx))
                  source-symbol @(r/cursor field [::field/source-symbol])
                  dbname (str source-symbol)
                  uri (when dbname
                        (domain/dbname->uri dbname (:hypercrud.browser/domain ctx)))]
              (cond-> (-> ctx
                          (set-parent)
                          (update :hypercrud.browser/path conj path-segment)
                          (assoc :hypercrud.browser/field field
                                 :hypercrud.browser/fields (r/fmap ::field/children field)
                                 :hypercrud.browser/source-symbol source-symbol
                                 ; todo why cant internals get the uri from source-symbol at the last second
                                 :uri uri))
                ; if we are in a head, we dont have data to set
                (not (some #{:head} (:hypercrud.browser/path ctx))) (set-data :db.cardinality/one)))))]
  (defn focus [ctx relative-path]
    (reduce focus-segment ctx relative-path)))

(defn refocus "focus common ancestor" [ctx path]
  {:pre [ctx] :post [%]}
  (let [current-path (:hypercrud.browser/path ctx)
        common-ancestor-path (ancestry-common current-path path)
        unwind-offset (- (count current-path) (count common-ancestor-path))
        common-ancestor-ctx ((apply comp (repeat unwind-offset :hypercrud.browser/parent)) ctx)]
    (focus common-ancestor-ctx (ancestry-divergence path current-path))))
