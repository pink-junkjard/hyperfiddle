(ns hypercrud.browser.context
  (:require
    [cats.core :refer [mlet return]]
    [cats.monad.either :as either :refer [left right]]
    [clojure.string :as string]
    [contrib.data :refer [ancestry-common ancestry-divergence]]
    [contrib.datomic]
    [contrib.eval :as eval]
    [contrib.reactive :as r]
    [contrib.string]
    [contrib.try$ :refer [try-either]]
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [hypercrud.browser.field :as field]
    [hypercrud.browser.link]
    [hypercrud.browser.q-util]
    [hypercrud.types.DbName :refer [#?(:cljs DbName)]]
    [hypercrud.types.DbRef :refer [->DbRef]]
    [hypercrud.types.ThinEntity :refer [->ThinEntity #?(:cljs ThinEntity)]]
    [hyperfiddle.branch :as branch]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime])
  #?(:clj
     (:import
       (hypercrud.types.DbName DbName)
       (hypercrud.types.ThinEntity ThinEntity))))


(s/def :hypercrud/context
  (s/keys :opt [:hypercrud.browser/data
                :hypercrud.browser/eav]))

(s/def :hypercrud.browser/eav (s/and r/reactive?
                                     #_(s/or :nil (comp nil? deref)
                                           #_#_:eav (s/coll-of (comp not map? deref)))))
(s/def :hypercrud.browser/data r/reactive?)

(defn clean [ctx]
  (dissoc ctx
          :hyperfiddle.ui/layout

          :hypercrud.browser/data
          :hypercrud.browser/eav
          :hypercrud.browser/fiddle
          :hypercrud.browser/field
          :hypercrud.browser/parent
          :hypercrud.browser/path
          :hypercrud.browser/route
          :hypercrud.browser/validation-hints))

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
    (keyword? segment) :attribute
    (integer? segment) :element                             ; can be a variable element, an aggregate element, etc
    ; it can also be entity-[], which has implied :element, this also happens in the query [?e ...] case
    :else :naked-or-element))

(defn dbname [ctx] (some->> (:hypercrud.browser/field ctx) (r/fmap ::field/source-symbol) deref str))

(defn ^:deprecated uri
  ([ctx] (dbname ctx))
  ([dbname ctx] dbname))

(defn ctx->id-lookup "light ctx dependency - needs :branch and :peer"
  ([ctx] (ctx->id-lookup (dbname ctx) ctx))
  ([dbname ctx]
    ; todo what about if the tempid is on a higher branch in the uri?
   (some-> dbname
           (->> (conj [::runtime/partitions (:branch ctx) :tempid-lookups])
                (runtime/state (:peer ctx))
                deref)
           (either/branch #(throw (ex-info % {})) identity))))

(defn underlying-tempid "ctx just needs :branch and :peer" [ctx id]
  ; This muddled thinking is caused by https://github.com/hyperfiddle/hyperfiddle/issues/584
  (cond
    (contrib.datomic/tempid? id) id
    :else (get (ctx->id-lookup ctx) id)))

(defn smart-entity-identifier "Generates the best Datomic lookup ref for a given pull. ctx needs :branch and :peer"
  [ctx {:keys [:db/id :db/ident] :as v}]                    ; v can be a ThinEntity or a pull i guess
  ; This must be called only on refs.
  ; If we have a color, and a (last path), ensure it is a ref.
  ; If we have a color and [] path, it is definitely a ref.
  ; If we have no color, it is a scalar or aggregate.
  ;(assert (::field/data-has-id? @(:hypercrud.browser/field ctx)) "smart-identity works only on refs")

  (let [identity-lookup nil]
    (or (if (underlying-tempid ctx id) id)                  ; the lookups are no good yet, must use the dbid (not the tempid, actions/with will handle that reversal)
        ident
        identity-lookup
        id
        (if-not (map? v) v)                                 ; id-scalar
        nil                                                 ; This is an entity but you didn't pull any identity - error?
        )))

(defn hydrate-attribute [ctx ident & ?more-path]
  (runtime/state (:peer ctx) (concat [::runtime/partitions (:branch ctx) :schemas (dbname ctx)] (cons ident ?more-path))))

(defn- set-parent [ctx]
  (assoc ctx :hypercrud.browser/parent (dissoc ctx :hypercrud.browser/data)))

(defn- set-parent-data [ctx]
  (update ctx :hypercrud.browser/parent (fnil into {}) (select-keys ctx [:hypercrud.browser/data])))

(defn find-child-field [field path-segment ctx]
  (or (->> (::field/children @field)
           (filter #(= (::field/path-segment %) path-segment))
           first)
      (when (keyword? path-segment)
        (let [dbname (str (::field/source-symbol @field))
              schema @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :schemas dbname])]
          (field/summon schema (::field/source-symbol @field) path-segment)))))

(defn find-parent-field [ctx]
  (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/field]))

(defn identify [ctx]
  {:post [(not (map? %))]}
  ; When looking at an attr of type ref, figure out it's identity, based on all the ways it can be pulled.
  ; What if we pulled children without identity? Then we can't answer the question (should assert this)
  (if-let [data (:hypercrud.browser/data ctx)]              ; Guard is for txfn popover call site
    (or @(contrib.reactive/cursor data [:db/ident])
        @(contrib.reactive/cursor data [:db/id])
        @data)))

(defn eav "project eav from data"
  [ctx data]
  (let [e (some-> ctx identify)                             ; this is the parent v
        a (last (:hypercrud.browser/path ctx))              ; todo chop off FE todo
        v (smart-entity-identifier ctx data)]
    (assert (not (map? e)))
    (assert (not (map? a)))
    (assert (not (map? v)))
    ; :extend-via-metadata
    [e a v]))

(letfn [(focus-segment [ctx path-segment]                   ; attribute or fe segment
          #_(assert (or (not (:hypercrud.browser/data ctx)) ; head has no data, can focus without calling row
                        @(r/fmap->> (:hypercrud.browser/field ctx) ::field/cardinality (not= :db.cardinality/many)))
                    (str "Cannot focus directly from a cardinality/many (do you need a table wrap?). current path: " (:hypercrud.browser/path ctx) ", attempted segment: " path-segment))
          (let [field (r/track find-child-field (:hypercrud.browser/field ctx) path-segment ctx)
                ctx (-> ctx
                        (set-parent)
                        (update :hypercrud.browser/path conj path-segment)
                        (assoc :hypercrud.browser/field field))]
            (if-not (:hypercrud.browser/data ctx)
              ctx                                           ; head
              (let [data (let [f (r/fmap ::field/get-value field)]
                           #_(assert @f (str "focusing on a non-pulled attribute: " (pr-str (:hypercrud.browser/path ctx)) "."))
                           (r/fapply f (:hypercrud.browser/data ctx)))]
                (-> (set-parent-data ctx)                   ; body
                    (update :hypercrud.browser/validation-hints #(for [[[p & ps] hint] % :when (= p path-segment)]
                                                                   [ps hint]))
                    (assoc :hypercrud.browser/data data)
                    (assoc :hypercrud.browser/eav (r/fmap (r/partial eav ctx) data))
                    )))))]
  (defn focus "Throws if you focus a higher dimension" [ctx relative-path]
    (reduce focus-segment ctx relative-path)))

(defn row "Toggle :many into :one as we spread through the rows. k is used for filtering validation hints"
  [ctx rval & [k]]
  {:pre [(r/reactive? rval)]}
  (assert @(r/fmap->> (:hypercrud.browser/field ctx) ::field/cardinality (= :db.cardinality/many))
          (str "`context/row` is only valid on cardinality/many. current path: " (pr-str (:hypercrud.browser/path ctx))))
  (-> ctx
      (set-parent)
      (set-parent-data)
      (assoc :hypercrud.browser/data rval)
      (update :hypercrud.browser/eav
              (r/partial r/fmap (fn [[e a v]]
                                  ; (assert (nil? v) "it was not yet in scope") -- nested case, eav is present and well defined
                                  ; FindColl is happy,
                                  ; FindRel (map smart-entity-identifier @rval)
                                  ; Todo normalize FindRel into FindColl when possible
                                  [e a (smart-entity-identifier ctx @rval)])))
      (update :hypercrud.browser/validation-hints #(for [[[p & ps] hint] % :when (= k p)]
                                                     [ps hint]))
      (update :hypercrud.browser/field
              #(r/fmap-> % (assoc ::field/cardinality :db.cardinality/one)))))

(defn refocus "todo unify with refocus'"
  [ctx path]
  {:pre [ctx] :post [(contains? % :hypercrud.browser/eav)]}
  (let [current-path (:hypercrud.browser/path ctx)
        common-ancestor-path (ancestry-common current-path path)
        unwind-offset (- (count current-path) (count common-ancestor-path))
        common-ancestor-ctx ((apply comp (repeat unwind-offset :hypercrud.browser/parent)) ctx)]
    (focus common-ancestor-ctx (ancestry-divergence path current-path))))

(defn id->tempid+ [route ctx]
  (let [invert-id (fn [dbname id]
                    (if (contrib.datomic/tempid? id)
                      id
                      (let [id->tempid (ctx->id-lookup dbname ctx)]
                        (get id->tempid id id))))]
    (try-either (route/invert-route route invert-id))))

(defn tempid->id+ [route ctx]
  (let [invert-id (fn [dbname id]
                    (if (contrib.datomic/tempid? id)
                      (let [tempid->id (-> (ctx->id-lookup dbname ctx)
                                           (set/map-invert))]
                        (get tempid->id id id))
                      id))]
    (try-either (route/invert-route route invert-id))))

(defn normalize-args [porps]
  ; There is some weird shit hitting this assert, like {:db/id nil}
  {:pre [#_(not (map? porps)) #_"legacy"]
   :post [(vector? %) #_"route args are associative by position"]}
  (vec (contrib.data/xorxs porps)))

(defn tag-v-with-color "Tag dbids with color, at the last moment before they render into URLs"
  [ctx v]
  (let [[_ a _] @(:hypercrud.browser/eav ctx)
        valueType (if a
                    @(hydrate-attribute ctx a :db/valueType :db/ident)
                    :db.type/ref)]                          ; if there is no a in scope, we must be a new entity
    (cond
      (instance? ThinEntity v) v                            ; backwards compat with old hfhf formulas which return #entity
      (instance? DbName v) v
      (= valueType :db.type/ref) (->ThinEntity (or (dbname ctx) "$") ; in the tuple case, each element may be a different color, so we need to refocus the ctx here (with find element index) to infer this
                                               (smart-entity-identifier ctx v))

      ; In edge cases, this could be a colorless opaque value (composite or scalar)
      :a-not-ref v)))

(let [eval-string!+ (memoize eval/eval-expr-str!+)]
  (defn build-args+ "Params are EAV-typed (uncolored)"
    [ctx {:keys [:link/fiddle :link/tx-fn] :as link}]
    (mlet [formula-ctx-closure (if-let [formula-str (contrib.string/blank->nil (:link/formula link))]
                                 (eval-string!+ (str "(fn [ctx] \n" formula-str "\n)"))
                                 (either/right (constantly (constantly nil))))
           formula-fn (try-either (formula-ctx-closure ctx))
           :let [v (r/fmap-> (:hypercrud.browser/eav ctx) (get 2))]
           args (try-either @(r/fmap->> v
                                        formula-fn          ; Documented behavior is v in, tuple out, no colors.
                                        normalize-args))]
      (return args))))

(defn ^:export build-route' "There may not be a route! Fiddle is sometimes optional" ; build-route+
  [+args ctx {:keys [:link/fiddle :link/tx-fn] :as link}]
  (if (and (not fiddle) tx-fn)
    (mlet [args +args] (return nil))                        ; :hf/remove doesn't have one by default, :hf/new does, both can be customized
    (mlet [args +args                                       ; part of error chain
           fiddle-id (if fiddle
                       (right (:fiddle/ident fiddle))
                       (left {:message ":link/fiddle required" :data {:link link}}))
           ; Why must we reverse into tempids? For the URL, of course.
           :let [colored-args (mapv (partial tag-v-with-color ctx) args)]
           route (id->tempid+ (hyperfiddle.route/canonicalize fiddle-id colored-args) ctx)
           route (hyperfiddle.route/validate-route+ route)]
      (return route))))

(letfn [(-stable-eav' [?v' [?e ?a _]]
          [?e ?a ?v'])]
  (defn refocus' "focus a link ctx, accounting for link/formula which occludes the natural eav"
    [ctx link-ref]
    {:pre [(s/assert :hypercrud/context ctx)
           (s/assert r/reactive? link-ref)]
     :post [(s/assert (s/cat :ctx :hypercrud/context :route r/reactive?) %)]}
    (let [path (hypercrud.browser.link/read-path @(r/fmap :link/path link-ref))
          ctx (refocus ctx path)
          +args @(r/fmap->> link-ref (build-args+ ctx))
          [v' & vs] (->> +args (contrib.ct/unwrap (constantly nil))) ; EAV sugar is not interested in tuple case, that txfn is way off happy path
          ctx (update ctx :hypercrud.browser/eav (r/partial r/fmap (r/partial -stable-eav' v')))
          r+?route (r/fmap->> link-ref (build-route' +args ctx))]
      [ctx r+?route])))

(defn tree-invalid? "For popover buttons (fiddle level)" [ctx]
  (->> (:hypercrud.browser/validation-hints ctx)
       seq boolean))

(defn leaf-invalid? "The thing that styles red" [ctx]
  (->> (:hypercrud.browser/validation-hints ctx)
       (filter (comp nil? first))
       seq boolean))

(let [impl (fn [rt branch-id dbname]
             (-> (loop [branch-id branch-id
                        tx nil]
                   (let [tx (concat tx @(runtime/state rt [::runtime/partitions branch-id :stage dbname]))]
                     (if (branch/root-branch? branch-id)
                       tx
                       (recur (branch/parent-branch-id branch-id) tx))))
                 hash str))]
  (defn tempid! "unstable"
    ([ctx]
     (let [dbname (or (dbname ctx) "$")]                    ; (assert dbname "no dbname in ctx")
       ; If you don't like $, specify a :fiddle/pull-database or :fiddle/query
       (tempid! dbname ctx)))
    ([dbname ctx] @(r/track impl (:peer ctx) (:branch ctx) dbname))))

(defn- fix-param [ctx param]
  (if (instance? ThinEntity param)
    (smart-entity-identifier ctx param)                     ; throws away the dbname
    param))

(defn validate-query-params+ [q args ctx]
  (mlet [query-holes (try-either (hypercrud.browser.q-util/parse-holes q)) #_"normalizes for :in $"
         :let [[params' unused] (loop [acc []
                                       [arg & next-args :as args] args
                                       [hole & next-holes] query-holes]
                                  (let [[arg next-args] (if (string/starts-with? hole "$")
                                                          (if (instance? DbName arg)
                                                            [(->DbRef (:dbname arg) (:branch ctx)) next-args]
                                                            [(->DbRef hole (:branch ctx)) args])
                                                          [(fix-param ctx arg) next-args])
                                        acc (conj acc arg)]
                                    (if next-holes
                                      (recur acc next-args next-holes)
                                      [acc next-args])))]]
    ;(assert (= 0 (count (filter nil? params')))) ; datomic will give a data source error
    ; validation. better to show the query and overlay the params or something?
    (cond #_#_(seq unused) (either/left {:message "unused param" :data {:query q :params params' :unused unused}})
      (not= (count params') (count query-holes)) (either/left {:message "missing params" :data {:query q :params params' :unused unused}})
      :else-valid (either/right params'))))

(defn stable-entity-key "Like smart-entity-identifier but reverses top layer of tempids to stabilize view keys in branches. You
  must pull db/id to trigger tempid detection! Don't use this in labels."
  [ctx {:keys [:db/id :db/ident] :as v}]
  ; https://github.com/hyperfiddle/hyperfiddle/issues/563 - Regression: Schema editor broken due to smart-id
  ; https://github.com/hyperfiddle/hyperfiddle/issues/345 - Form jank when tempid entity transitions to real entity
  (or (underlying-tempid ctx id)                            ; prefer the tempid for stability
      (smart-entity-identifier ctx v)))

(defn stable-relation-key "Stable key that works on scalars too. ctx is for tempid-reversing"
  [ctx v]
  (or (stable-entity-key ctx v) v))

(defn row-keyfn [ctx row]
  (r/row-keyfn' (partial stable-relation-key ctx) row))

(defn hash-ctx-data [ctx]                                   ; todo there are collisions when two links share the same 'location'
  (when-let [data (:hypercrud.browser/data ctx)]
    (case @(r/fmap ::field/cardinality (:hypercrud.browser/field ctx))
      :db.cardinality/one @(r/fmap->> data (stable-relation-key ctx))
      :db.cardinality/many @(r/fmap->> data
                                       (mapv (r/partial stable-relation-key ctx))
                                       (into #{})
                                       hash)                ; todo scalar
      nil nil #_":db/id has a faked attribute with no cardinality, need more thought to make elegant")))

(defn tempid "stable" [ctx]
  ; recurse all the way up the path? just data + parent-data is relative not fully qualified, which is not unique
  (-> (str (:hypercrud.browser/path ctx) "."
           (hash-ctx-data (:hypercrud.browser/parent ctx)) "."
           (hash-ctx-data ctx))
      hash str))
