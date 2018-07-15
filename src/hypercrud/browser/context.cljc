(ns hypercrud.browser.context
  (:require
    [contrib.reactive :as r]
    [datascript.parser :as parser]
    [hypercrud.browser.field :as field]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.runtime :as runtime]))


(defn clean [ctx]
  ; why not code-database-uri and all the custom ui/render fns?
  (dissoc ctx
          :route                                            ; Route is unrelated to the hyper-control ontology
          :uri                                              ; todo deprecate
          :user-with!                                       ; todo namespace
          :hyperfiddle.ui/layout

          :hypercrud.browser/attribute
          :hypercrud.browser/data
          :hypercrud.browser/data-cardinality
          :hypercrud.browser/fat-attribute
          :hypercrud.browser/fiddle
          :hypercrud.browser/field
          :hypercrud.browser/fields
          :hypercrud.browser/links
          :hypercrud.browser/path
          :hypercrud.browser/request
          :hypercrud.browser/result
          :hypercrud.browser/source-symbol
          :hypercrud.browser/schema
          :hypercrud.browser/schemas))

(defn source-mode [ctx]
  (-> ctx
      (assoc :hypercrud.browser/page-on-click (r/constantly nil) ; disable alt-click
             :hypercrud.ui/display-mode (r/track identity :hypercrud.browser.browser-ui/user))
      (update :hypercrud.browser/domain
              (fn [domain]
                (assoc-in (:hypercrud.browser/source-domain ctx) [:domain/environment "$"] (:domain/fiddle-repo domain))))))

(defn attribute-segment? [path-segment]
  (and (not (contains? #{:head :body} path-segment))        ; todo cannot conflict with :db/ident :head | :body
       (or (keyword? path-segment)
           (= '* path-segment))))

(defn find-element-segment? [path-segment]
  (integer? path-segment))

(defn- set-parent [ctx]
  (assoc ctx :hypercrud.browser/parent (select-keys ctx [:hypercrud.browser/parent :hypercrud.browser/path])))

(defn- set-parent-data [ctx]
  (update ctx :hypercrud.browser/parent (fnil into {}) (select-keys ctx [:hypercrud.browser/data
                                                                         :hypercrud.browser/data-cardinality])))

(defn body [ctx & [?data]]
  (case (:hypercrud.browser/data-cardinality ctx)
    :db.cardinality/one (assert (nil? ?data))
    :db.cardinality/many (do (assert ?data (str "`:body` is invalid directly on card/many. current path: " (pr-str (:hypercrud.browser/path ctx))))
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
                            :hypercrud.browser/fat-attribute
                            :hypercrud.browser/field))]
    (case (:hypercrud.browser/data-cardinality ctx)
      :db.cardinality/one new-ctx
      :db.cardinality/many (assoc new-ctx :hypercrud.browser/data ?data))))

(letfn [(default [default-v v] (or v default-v))
        (user-with [rt invert-route branch uri tx]
          (runtime/dispatch! rt (actions/with rt invert-route branch uri tx)))
        (set-data [ctx cardinality]
          (-> ctx
              (set-parent-data)
              (assoc
                :hypercrud.browser/data (let [f (r/fmap ::field/get-value (:hypercrud.browser/field ctx))]
                                          (assert @f "focusing on a non-pulled attribute")
                                          (r/fapply f (:hypercrud.browser/data ctx)))
                :hypercrud.browser/data-cardinality cardinality)))
        (query-type [query]
          (-> (parser/parse-query query)
              :qfind
              type))
        (initial-focus [ctx]
          (case @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/type]) ; fiddle/type not relevant outside this fn
            :entity (if-let [a @(r/cursor (:hypercrud.browser/request ctx) [:a])]
                      (let [dbname @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/pull-database])]
                        (case @(r/cursor (:hypercrud.browser/schemas ctx) [dbname a :db/cardinality :db/ident])
                          :db.cardinality/one
                          (assoc ctx
                            :hypercrud.browser/data (r/fmap vector (:hypercrud.browser/result ctx))
                            :hypercrud.browser/data-cardinality :db.cardinality/one
                            :hypercrud.browser/path [])

                          :db.cardinality/many
                          (assoc ctx
                            :hypercrud.browser/data (r/fmap (r/partial mapv vector) (:hypercrud.browser/result ctx))
                            :hypercrud.browser/data-cardinality :db.cardinality/many
                            :hypercrud.browser/path [])))
                      (assoc ctx
                        :hypercrud.browser/data (r/fmap vector (:hypercrud.browser/result ctx))
                        :hypercrud.browser/data-cardinality :db.cardinality/one
                        :hypercrud.browser/path []))
            :query (condp = (query-type @(r/cursor (:hypercrud.browser/request ctx) [:query]))
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
            ; todo cannot conflict with :db/ident :head
            (and (= path-segment :head) #_(nil? (:hypercrud.browser/data-cardinality ctx)))
            (-> ctx
                (assoc :hypercrud.browser/parent (select-keys ctx [:hypercrud.browser/data
                                                                   :hypercrud.browser/data-cardinality
                                                                   :hypercrud.browser/path]))
                (update :hypercrud.browser/path conj :head)
                (dissoc :hypercrud.browser/attribute
                        :hypercrud.browser/data
                        :hypercrud.browser/data-cardinality
                        :hypercrud.browser/fat-attribute
                        :hypercrud.browser/field))

            ; todo cannot conflict with :db/ident :body
            (and (= path-segment :body) #_(not= :db.cardinality/one (:hypercrud.browser/data-cardinality ctx)))
            (if (nil? (:hypercrud.browser/data ctx))
              (initial-focus ctx)                           ; todo this isn't :body, this is :hypercrud.browser/result
              (body ctx))

            (attribute-segment? path-segment)
            (let [field (r/fmap (r/partial find-field path-segment) (:hypercrud.browser/fields ctx))
                  fat-attribute (->> (r/cursor (:hypercrud.browser/schema ctx) [path-segment])
                                     (r/fmap (r/partial default {:db/ident path-segment})))]
              (cond-> (-> ctx
                          (set-parent)
                          (update :hypercrud.browser/path conj path-segment)
                          (assoc
                            :hypercrud.browser/attribute path-segment
                            :hypercrud.browser/fat-attribute fat-attribute
                            :hypercrud.browser/field field
                            :hypercrud.browser/fields (r/fmap ::field/children field)))
                ; if we are in a head, we dont have data to set
                (not (some #{:head} (:hypercrud.browser/path ctx))) (set-data @(r/cursor fat-attribute [:db/cardinality :db/ident]))))

            (find-element-segment? path-segment)
            (let [field (r/fmap (r/partial find-field path-segment) (:hypercrud.browser/fields ctx))
                  source-symbol @(r/cursor field [::field/source-symbol])
                  dbname (str source-symbol)
                  uri (when dbname
                        (get-in ctx [:hypercrud.browser/domain :domain/environment dbname]))]
              (cond-> (-> ctx
                          (set-parent)
                          (update :hypercrud.browser/path conj path-segment)
                          (assoc :hypercrud.browser/field field
                                 :hypercrud.browser/fields (r/fmap ::field/children field)
                                 :hypercrud.browser/schema (r/cursor (:hypercrud.browser/schemas ctx) [dbname])
                                 :hypercrud.browser/source-symbol source-symbol
                                 :uri uri                   ; todo why cant internals get the uri from source-symbol at the last second
                                 :user-with! (r/partial user-with (:peer ctx) (:hypercrud.browser/invert-route ctx) (:branch ctx) uri)))
                ; if we are in a head, we dont have data to set
                (not (some #{:head} (:hypercrud.browser/path ctx))) (set-data :db.cardinality/one)))))]
  (defn focus [ctx relative-path]
    (reduce focus-segment ctx relative-path)))
