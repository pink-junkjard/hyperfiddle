(ns hypercrud.browser.context
  (:require
    [cats.core :refer [mlet return =<< fmap]]
    [cats.monad.either :as either :refer [left right]]
    [contrib.ct :refer [unwrap]]
    [contrib.data :refer [ancestry-common ancestry-divergence]]
    [contrib.datomic]
    [contrib.eval :as eval]
    [contrib.reactive :as r]
    [contrib.reader]
    [contrib.string]
    [contrib.try$ :refer [try-either]]
    [contrib.validation]
    [clojure.set]
    [clojure.spec.alpha :as s]
    [datascript.parser #?@(:cljs [:refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]])]
    [hypercrud.browser.q-util]
    [hypercrud.client.core :as hc]
    [hypercrud.types.ThinEntity :refer [->ThinEntity #?(:cljs ThinEntity)]]
    [hypercrud.util.branch]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.fiddle]
    [hyperfiddle.route]
    [hyperfiddle.runtime :as runtime]
    [taoensso.timbre :as timbre])
  #?(:clj
     (:import
       (hypercrud.types.ThinEntity ThinEntity)
       (datascript.parser FindRel FindColl FindTuple FindScalar Variable Aggregate Pull))))


;(defprotocol Context
;  (schema [ctx]))

(s/def :hypercrud/context
  (s/keys :opt [:hypercrud.browser/route
                :hypercrud.browser/schemas
                :hypercrud.browser/schema
                :hypercrud.browser/link-index
                :hypercrud.browser/result                   ; i think this is compat only. @(data) should infer
                :hypercrud.browser/data-index
                :hypercrud.browser/fiddle
                :hypercrud.browser/qfind
                :hypercrud.browser/result-enclosure
                :hypercrud.browser/validation-hints
                :hypercrud.browser/element
                :hypercrud.browser/pull-enclosure
                :hypercrud.browser/pull-path                     ; used only by refocus and links-in-dimension
                :hypercrud.browser/eav                      ; V is scalar or identity, and is often nil. (int? a) addresses a find-element
                :hyperfiddle.runtime/branch-aux
                :hyperfiddle.ui.iframe/on-click
                :hypercrud.ui/display-mode
                :hyperfiddle.ui/layout
                :hyperfiddle.ui.sort/sort-col]))

(s/def :hypercrud.browser/eav r/reactive?)
(s/def :hypercrud.browser/data r/reactive?)
(s/def :hypercrud.browser/data-index r/reactive?)
(s/def :hypercrud.browser/element r/reactive?)
(s/def :hypercrud.browser/element-index int?)
(s/def :hypercrud.browser/pull-path (s/coll-of keyword?))

(defn clean [ctx]
  (dissoc ctx
          :hypercrud.ui/error
          :hyperfiddle.ui/layout

          :hypercrud.browser/attr-renderers
          :hypercrud.browser/schemas
          :hypercrud.browser/result
          :hypercrud.browser/data-index
          :hypercrud.browser/link-index
          :hypercrud.browser/fiddle
          :hypercrud.browser/qfind
          :hypercrud.browser/element
          :hypercrud.browser/schema
          :hypercrud.browser/eav
          :hypercrud.browser/parent
          :hypercrud.browser/pull-path
          :hypercrud.browser/route
          :hypercrud.browser/validation-hints))

(defn source-mode [ctx]
  (-> ctx
      (assoc :hyperfiddle.ui.iframe/on-click (r/constantly nil) ; disable alt-click
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

(defn segment-type-2 [segment]
  ; BROKEN, always attr now
  (cond
    (= '* segment) :splat
    (keyword? segment) :attribute
    (integer? segment) :element                             ; can be a variable element, an aggregate element, etc
    ; it can also be entity-[], which has implied :element, this also happens in the query [?e ...] case
    :else :naked-or-element))

(defn target-route [ctx] @(runtime/state (:peer ctx) [::runtime/partitions nil :route]))

(defn dbname [ctx]
  (some-> (:hypercrud.browser/element ctx) deref :source :symbol str))

(defn uri                                                   ; equivalent to element or schema, i think?
  ([ctx] (uri (dbname ctx) ctx))
  ([dbname ctx] (some-> dbname (domain/dbname->uri (:hypercrud.browser/domain ctx)))))

(defn ctx->id-lookup "light ctx dependency - needs :branch and :peer"
  ([ctx] (ctx->id-lookup (uri ctx) ctx))
  ([uri ctx]
    ; todo what about if the tempid is on a higher branch in the uri?
   (some-> uri
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
  ; flip params for fmap->
  [ctx {:keys [:db/id :db/ident] :as v}]                    ; v can be a ThinEntity or a pull i guess
  {:post [(not (coll? %))]}
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
        (if-not (coll? v) v)                                ; id-scalar
        nil                                                 ; This is an entity but you didn't pull any identity - error?
        ; Or it could be a relation. Why did this get called?
        )))

(defn summon-schemas-grouped-by-dbname [ctx]
  {:post [(every? #(satisfies? contrib.datomic/SchemaIndexedNormalized %) (vals %))]}
  (-> (->> @(hyperfiddle.runtime/state (:peer ctx) [:hyperfiddle.runtime/partitions (:branch ctx) :schemas])
           (contrib.data/map-keys #(hyperfiddle.domain/uri->dbname % (:hypercrud.browser/domain ctx))))
      (dissoc nil)))

(defn hydrate-attribute [ctx ident & ?more-path]
  (runtime/state (:peer ctx) (concat [::runtime/partitions (:branch ctx) :schemas (uri ctx)] (cons ident ?more-path))))

(defn- set-parent [ctx]
  (assoc ctx :hypercrud.browser/parent (dissoc ctx :hypercrud.browser/result)))

(defn stable-entity-key "Like smart-entity-identifier but reverses top layer of tempids to stabilize view keys in branches. You
  must pull db/id to trigger tempid detection! Don't use this in labels."
  [ctx {:keys [:db/id :db/ident] :as v}]
  ; https://github.com/hyperfiddle/hyperfiddle/issues/563 - Regression: Schema editor broken due to smart-id
  ; https://github.com/hyperfiddle/hyperfiddle/issues/345 - Form jank when tempid entity transitions to real entity
  (or (underlying-tempid ctx id)                            ; prefer the tempid for stability
      (smart-entity-identifier ctx v)))

(defn stable-relation-key "Stable key that works on scalars too. ctx is for tempid-reversing"
  [ctx v]
  (or (stable-entity-key ctx v) v))                         ; bad

(defn row-keyfn [ctx row]
  (r/row-keyfn' (partial stable-relation-key ctx) row))     ; bad

(defn stable-eav-v [[?e a _] ?v']
  {:pre [a
         #_?v'                                              ; Never know with ?v, did they pull identity?
         ]}
  [?e a ?v'])

(defn stable-eav-av "v becomes e. In top cases, this is nil->nil; only Pulls have defined E.
  ?v' can be nil - sparse results."
  [[_ _ ?v] a' ?v']
  {:pre [a']}
  [?v a' ?v'])

(defn stable-eav-a [[_ _ v] a']
  ; v becomes e
  [v a' nil])

(defn ^:export fiddle "Fiddle level ctx adds the result to scope"
  [ctx]
  {:post [(s/assert :hypercrud/context %)
          (contains? % :hypercrud.browser/qfind)            ; but it can be nil for :type :blank
          (:hypercrud.browser/link-index %)]}
  ; Deep inspect the elements to compute the enclosing pull shape for each element
  ; Don't infer any further scopes, it is down-scope's job to infer anything it needs (which might be legacy compat at this point)
  (as-> ctx ctx
        (assoc ctx :hypercrud.browser/qfind (r/fmap-> (:hypercrud.browser/fiddle ctx) hyperfiddle.fiddle/parse-fiddle-data-shape :qfind))
        (assoc ctx :hypercrud.browser/result-enclosure (if @(:hypercrud.browser/qfind ctx)
                                                         (r/ctxf contrib.datomic/result-enclosure ctx
                                                                 :hypercrud.browser/schemas
                                                                 :hypercrud.browser/qfind
                                                                 :hypercrud.browser/result)))
        ; flag for nested
        (assoc ctx :hypercrud.browser/validation-hints (contrib.validation/validate
                                                         (s/get-spec @(r/fmap :fiddle/ident (:hypercrud.browser/fiddle ctx)))
                                                         @(:hypercrud.browser/result ctx)
                                                         (partial row-keyfn ctx)))
        (assoc ctx :hypercrud.browser/eav (r/apply hypercrud.browser.context/stable-eav-av
                                                   [(:hypercrud.browser/eav ctx)
                                                    (r/fmap :fiddle/ident (:hypercrud.browser/fiddle ctx))
                                                    ; What about head vs body? We're in body now!
                                                    (r/track identity nil) #_(:hypercrud.browser/result ctx)]))))

(defn -infer-implicit-fiddle [ctx]
  (or
    (if-not (:hypercrud.browser/qfind ctx)
      (hypercrud.browser.context/fiddle ctx))
    ctx))

(defn stable-element-schema [schemas element]
  {:post [%]}
  (let [{{db :symbol} :source {pull-pattern :value} :pattern} element]
    (get schemas (str db))))

(defn data "Works in any context"                           ; todo provide data! ??
  [{:keys [:hypercrud.browser/qfind
           :hypercrud.browser/element
           :hypercrud.browser/result
           :hypercrud.browser/result-path]}]
  ; Potentially this could infer.
  (if qfind
    (if element
      (if result-path
        (r/cursor result result-path)
        result)
      result)
    nil))

(defn v! [ctx]                                              ; It's just easier to end the reaction earlier
  {:post [(not (coll? %))]}                                 ; V is identity | nil
  ; Don't pass whole results or tuples to smart-entity-identifier, that's dumb
  ; Do we have an element but not a pull?
  ; If no element at all, (just a fiddle), not much to do. Always need an element at least.
  (if-not (:hypercrud.browser/head-sentinel ctx)
    (if (:hypercrud.browser/element ctx)                    ; Otherwise we have a result but not focused at anything identifiable
      ; Sparse resultset, v can still be nil
      (smart-entity-identifier ctx @(data ctx)))))

(defn element [ctx i]                                       ; [nil :seattle/neighborhoods 1234345]
  ; eav is ea, data is a tree and can request from resultpath
  ; If EAV is set here, A becomes set to i. That gives us a unique address of aggregates and variables.
  {:pre [#_(s/assert r/reactive? (:hypercrud.browser/qfind ctx)) ; can be inferred
         #_(s/assert nil? (:hypercrud.browser/element ctx))
         #_(do (println "element: " i) true)
         #_(do (println "... data pre: " (pr-str (some-> (:hypercrud.browser/data ctx) deref))) true)]
   :post [(s/assert r/reactive? (:hypercrud.browser/qfind %))
          (s/assert r/reactive? (:hypercrud.browser/schema %))
          #_(s/assert :hypercrud/context %)
          #_(do (some->> % :hypercrud.browser/data deref pr-str (println "... data post: ")) true)
          #_(every? some? @(:hypercrud.browser/data %))]}   ; bad in header, good in body

  (let [r-element (r/fmap-> (:hypercrud.browser/qfind ctx) datascript.parser/find-elements (get i))]
    (as-> ctx ctx
          (-infer-implicit-fiddle ctx)
          (assoc ctx :hypercrud.browser/element r-element)

          (assoc ctx :hypercrud.browser/schema (r/ctxf stable-element-schema ctx
                                                       :hypercrud.browser/schemas
                                                       :hypercrud.browser/element))
          (assoc ctx :hypercrud.browser/pull-enclosure (r/fmap-> (:hypercrud.browser/result-enclosure ctx)
                                                                 (get i)))
          (condp some [(type @(:hypercrud.browser/qfind ctx))]
            #{FindRel FindTuple} (as-> ctx ctx
                                       (update ctx :hypercrud.browser/result-path (fnil conj []) i)
                                       (assoc ctx :hypercrud.browser/element-index i)) ; used only in labels
            #{FindColl FindScalar} ctx)

          ; Do last, result-path is set
          ; If there is only one element, should we set V?
          ; Hard to interpret it since we don't know what it is (entity, aggregate, var)
          ; Setting :hypercrud.browser/validation-hints doesn't make sense here as specs are keyword oriented
          ; so at the fiddle level, or at the attribute level, but not relations.
          ;
          ; We can set i as the a, but you can't have links on i, we want to leave the fiddle-ident as the a.
          ; a has to be semantic. does it matter?
          (assoc ctx :hypercrud.browser/eav (r/fmap-> (:hypercrud.browser/eav ctx) (stable-eav-v (v! ctx)))))))

(defn -infer-implicit-element "auto-focus single elements - legacy field path compat"
  [ctx]
  (or
    (if-not (:hypercrud.browser/element ctx)
      (if (#{FindColl FindScalar} (type @(:hypercrud.browser/qfind ctx)))
        (hypercrud.browser.context/element ctx 0)))
    ctx))

(defn -validate-qfind-element [ctx]
  {:pre [(s/assert r/reactive? (:hypercrud.browser/qfind ctx))
         (s/assert r/reactive? (:hypercrud.browser/element ctx))]}
  ctx)

(defn attribute "Will still set EA and V if we have it, e.g. works in table-head position (no row set).
  V becomes E"
  [ctx a']
  {:pre [(s/assert :hypercrud/context ctx)
         (s/assert keyword? a')]
   :post [(s/assert :hypercrud/context %)]}
  (let [{:keys [:hypercrud.browser/pull-path
                :hypercrud.browser/result-path
                :hypercrud.browser/qfind]} ctx]
    (as->
      ctx ctx
      (-infer-implicit-fiddle ctx)                          ; ensure result-enclosure
      (-infer-implicit-element ctx)                         ; ensure pull-enclosure
      (-validate-qfind-element ctx)
      (set-parent ctx)
      (update ctx :hypercrud.browser/pull-path conj a')
      (if (:hypercrud.browser/head-sentinel ctx)
        ctx
        (as-> ctx ctx
              (update ctx :hypercrud.browser/result-path (fnil conj []) a') ; no result-path in head
              (update ctx :hypercrud.browser/validation-hints #(for [[[p & ps] hint] % :when (= p a')]
                                                                 [ps hint]))))
      (assoc ctx :hypercrud.browser/pull-enclosure (r/fmap-> (:hypercrud.browser/pull-enclosure ctx)
                                                             (contrib.datomic/pullshape-get a')))
      ; V is for formulas, E is for security and on-change. V becomes E. E is nil if we don't know identity.
      (assoc ctx :hypercrud.browser/eav                     ; insufficent stability on r-?v? fixme
                 (case (if (= a' :db/id)
                         :db.cardinality/one                ; :db/id is currently addressable (e.g. user wants to render that column)
                         (contrib.datomic/cardinality @(:hypercrud.browser/schema ctx) a'))
                   :db.cardinality/many (r/fmap-> (:hypercrud.browser/eav ctx) (stable-eav-a a')) ; dont have v yet
                   :db.cardinality/one (r/fmap-> (:hypercrud.browser/eav ctx) (stable-eav-av a' (v! ctx))))))))

(defn focus "Unwind or go deeper, to where we need to be, within same dimension.
    Throws if you focus a higher dimension.
    This is about navigation through pulledtrees which is why it is path-oriented."
  [ctx relative-path]
  {:pre [(s/assert :hypercrud/context ctx)]
   :post [(s/assert :hypercrud/context %)
          #_(do (println "Field relpath: " relative-path " eav: " (pr-str @(:hypercrud.browser/eav %))) true)]}
  ; Is this legacy compat?
  (reduce (fn [ctx p]
            (cond
              (int? p) (element ctx p)
              ;(#{:db/id #_:db/ident} p) ctx                 ; v is already properly set
              ; db/id is renderable, address it. The controls will unwind to parent ctx. Tangled
              (keyword? p) (attribute ctx p)
              :else (assert false (str "illegal focus: " p))))
          ctx relative-path))

(defn row ""
  [ctx & [k]]
  {:pre [(s/assert :hypercrud/context ctx)]
   :post [(s/assert :hypercrud/context %)
          #_(do (println (:hypercrud.browser/data ctx)) true)]}
  (-> ctx
      (set-parent)
      (update :hypercrud.browser/result-path (fnil conj []) k)        ; no change in eav
      (update :hypercrud.browser/validation-hints #(for [[[p & ps] hint] % :when (= k p)]
                                                     [ps hint]))))

(defn ^:export spread-fiddle "automatically guards :fiddle/type :blank.
  Use in a for comprehension!"
  [ctx]
  (let [fiddle-type @(r/fmap :fiddle/type (:hypercrud.browser/fiddle ctx))]
    (condp some [fiddle-type]
      #{:blank} []                                          ; don't we want the eav?
      #{:query :entity} [(hypercrud.browser.context/fiddle ctx)])))

(defn ^:export spread-rows "spread across resultset row-or-rows; returns [k ctx] for your react key.
  Automatically accounts for query dimension - no-op in the case of FindTuple and FindScalar."
  [ctx & [sort-fn]]
  {:pre [(:hypercrud.browser/qfind ctx)
         (s/assert :hypercrud/context ctx)
         #_(not (:hypercrud.browser/element ctx))]}         ; not yet, except in recursive case
  (let [ctx (assoc ctx :hypercrud.browser/head-sentinel false) ; hack
        sort-fn (or sort-fn identity)
        depth (count (:hypercrud.browser/result-path ctx))]
    (cond                                                   ; We're either fiddle | nested attr (otherwise already spread)

      (= depth 0)                                           ; fiddle level
      (let [{r-qfind :hypercrud.browser/qfind} ctx
            r-ordered-result (:hypercrud.browser/result ctx)] ; don't have a result-path, thus use raw result
        (condp some [(type @r-qfind)]
          ; sorting doesn't index keyfn lookup by design
          #{FindRel FindColl}
          (let [keyfn (partial row-keyfn ctx)
                ; Downstack from here, result is indexed, userland must never look at it, they will be confused.
                ctx (assoc ctx :hypercrud.browser/result (r/fmap->> r-ordered-result (contrib.data/group-by-unique keyfn)))]
            (assert (:hypercrud.browser/result ctx))
            ; Rows are ordered, but the result value is indexed for lookup (not order)
            ; So drive index-key by row order
            (for [k (->> (sort-fn @r-ordered-result)        ; client side sorting – should happen in backend
                         (map keyfn))]
              [k (row ctx k)]))

          #{FindTuple FindScalar}
          [ctx]))

      (> depth 0)                                           ; nested attr
      (let [keyfn (partial stable-entity-key ctx)           ; Group again by keyfn, we have seq and need lookup
            ;r-ordered-result (data ctx)                    ; remember row order
            r-ordered-result (r/cursor (:hypercrud.browser/result ctx) (:hypercrud.browser/result-path ctx)) ; exploded for clarity
            ; Deep update result in-place, at result-path, to index it. Don't clobber it!
            ctx (assoc ctx :hypercrud.browser/result (r/fmap-> (:hypercrud.browser/result ctx)
                                                               (update-in (:hypercrud.browser/result-path ctx)
                                                                          ; not r/partial
                                                                          (partial contrib.data/group-by-unique keyfn))))]
        (for [k (->> (sort-fn @r-ordered-result)
                     (map keyfn))]
          [k (row ctx k)])))))

(defn ^:export spread-elements "yields [i ctx] foreach element.
  All query dimensions have at least one element."
  [ctx]
  {:pre [(:hypercrud.browser/qfind ctx)
         (not (:hypercrud.browser/element ctx))]
   #_#_:post [(s/assert :hypercrud/context %)]}
  (let [r-qfind (:hypercrud.browser/qfind ctx)]
    ; No unsequence here? What if find elements change? Can we use something other than (range) as keyfn?
    (for [i (range (count (datascript.parser/find-elements @r-qfind)))]
      [i (element ctx i)])))

; var first, then can always use db/id on row. No not true – collisions! It is the [?e ?f] product which is unique

; keyfn must support entities & scalars (including aggregates)
; scalars may not be unique
; entities might not be unique either, its the combination that is unique
; lift out of current context in this case?
; Make sure to optimize the single-fe case to use the identity/value which will be unique

(defn link-identifiers [fiddle-ident link]
  (let [idents (-> (set (:link/class link))
                   (conj (some-> link :link/fiddle :fiddle/ident))
                   ; fiddle-ident is named in the link/path (e.g. to name a FindScalar)
                   (conj (some-> link :link/path hyperfiddle.fiddle/read-path)) ; nil is semantically valid and well-defined
                   (conj (some-> link :link/tx-fn (subs 1) keyword)))]
    [idents link]))

(defn link-criteria-match? [?corcs [index-key v]]
  (clojure.set/superset? index-key (contrib.data/xorxs ?corcs #{})))

(defn -indexed-links-at [fiddle]
  (->> (:fiddle/links fiddle)
       (map (partial link-identifiers (:fiddle/ident fiddle)))))

(defn links-at "where criterias is some of #{ident txfn class class2}.
  Links include all links reachable by navigating :ref :one. (Change this by specifying an :ident)
  The index internals are reactive."
  [ctx criterias]                              ; criterias can contain nil, meaning toptop
  (r/fmap->> (:hypercrud.browser/link-index ctx)
             (filter (partial link-criteria-match? criterias))
             (mapv second)))

(defn links-in-dimension' [ctx criterias]
  {:post [(not (r/reactive? %))]}
  (let [?element (some-> ctx :hypercrud.browser/element deref)
        ?schema (some-> ctx :hypercrud.browser/schema deref)
        ?pullpath (:hypercrud.browser/pull-path ctx)]
    (if-not (and ?element ?schema ?pullpath)
      @(links-at ctx criterias)
      (let [pull-pattern (get-in ?element [:pattern :value])
            ; if pullpath is [], add fiddle-ident. Or if EAV a is not a keyword.
            ; EAV A may already be the fiddle-ident, or if its an int, use fiddle-ident too.
            ; reachable link locations, not just attrs.
            as (contrib.datomic/reachable-attrs ?schema (contrib.datomic/pull-shape pull-pattern) ?pullpath)
            links (->> as
                       ; Places within reach
                       (mapcat (fn [a]
                                 @(links-at ctx (conj criterias a))))

                       ; This place is where we are now
                       (concat @(links-at ctx criterias)))] ; this causes duplicates, there are bugs here
        (vec (distinct links))                              ; associative by index
        #_(->> links r/sequence (r/fmap vec))))))

(defn links-in-dimension [ctx criterias]
  (r/track links-in-dimension' ctx criterias))

(defn refocus "todo unify with refocus'
  From view, !link(:new-intent-naive :hf/remove) - find the closest ctx with all dependencies satisfied. Accounts for cardinality.
  Caller asserts that the deps are satisfied (right stuff is in scope).
  Logic is relative to where we are now, so find-element is implicit.
  If there is more than one path, is this an error?"
  ; fiddle means eav starts at fiddle level
  [ctx target-attr]
  {:pre [(:hypercrud.browser/element ctx)                   ; even FindScalar has an element, :blank does not need to refocus
         ctx]
   ; If you don't have this stuff why do we need to focus?
   :post [(->> (keys %) (clojure.set/superset? #{:hypercrud.browser/eav
                                                 :hypercrud.browser/element
                                                 :hypercrud.browser/qfind}))]}

  ; are we focusing from blank? Impossible, there's no result to focus
  ; Are we refocusing to blank? That would mean whacking the ctx entirely?
  ; if target-attr = last path, we're already here! How did that happen? Should be harmless, will noop
  (let [{{db :symbol} :source {pull-pattern :value} :pattern} (:hypercrud.browser/element ctx)
        root-pull (contrib.datomic/pull-shape pull-pattern)
        current-path (:hypercrud.browser/pull-path ctx)
        schema (get (summon-schemas-grouped-by-dbname ctx) (str db))

        ; find a reachable path that contains the target-attr in the fewest hops
        ;   me->mother ; me->sister->mother ; closest ctx is selected
        ; What if there is more than one?  me->sister->mother; me->father->mother
        ; Ambiguous, how did we even select this link? Probably need full datascript query language.
        target-path (->> (contrib.datomic/reachable-pullpaths schema root-pull current-path)
                         (filter #(some target-attr %))
                         (map (partial take-while (partial not= target-attr)))
                         (sort-by count)
                         first)

        common-ancestor-path (ancestry-common current-path target-path)
        unwind-offset (- (count current-path) (count common-ancestor-path))
        common-ancestor-ctx ((apply comp (repeat unwind-offset :hypercrud.browser/parent)) ctx)]
    (focus common-ancestor-ctx (ancestry-divergence target-path current-path))))

(defn id->tempid+ [route ctx]
  (let [invert-id (fn [id uri]
                    (if (contrib.datomic/tempid? id)
                      id
                      (let [id->tempid (ctx->id-lookup uri ctx)]
                        (get id->tempid id id))))]
    (try-either (hyperfiddle.route/invert-route (:hypercrud.browser/domain ctx) route invert-id))))

(defn tempid->id+ [route ctx]
  (let [invert-id (fn [temp-id uri]
                    (if (contrib.datomic/tempid? temp-id)
                      (let [tempid->id (-> (ctx->id-lookup uri ctx)
                                           (clojure.set/map-invert))]
                        (get tempid->id temp-id temp-id))
                      temp-id))]
    (try-either (hyperfiddle.route/invert-route (:hypercrud.browser/domain ctx) route invert-id))))

(defn normalize-args [?porps]
  {:post [(vector? %) #_"route args are associative by position"]} ; can be []
  (vec (contrib.data/xorxs ?porps)))

(defn tag-v-with-color "Tag dbids with color, at the last moment before they render into URLs"
  [ctx v]
  (let [[_ a _] @(:hypercrud.browser/eav ctx)
        valueType (if a
                    @(hydrate-attribute ctx a :db/valueType :db/ident)
                    :db.type/ref)]                          ; if there is no a in scope, we must be a new entity
    (cond
      (instance? ThinEntity v) v                            ; backwards compat with old hfhf formulas which return #entity
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
           :let [[_ _ v] @(:hypercrud.browser/eav ctx)]
           args (try-either (formula-fn v))]                ; Documented behavior is v in, tuple out, no colors.
      (return (normalize-args args)))))

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

(defn refocus' "focus a link ctx, accounting for link/formula which occludes the natural eav"
  [ctx link-ref]
  {:pre [(s/assert :hypercrud/context ctx)
         (s/assert r/reactive? link-ref)]
   :post [(s/assert (s/cat :ctx :hypercrud/context :route r/reactive?) %)]}
  (let [[_ a _] @(:hypercrud.browser/eav ctx)
        target-a @(r/fmap (r/comp hyperfiddle.fiddle/read-path :link/path) link-ref)
        ctx (if (and target-a (not= target-a a))            ; backwards compat
              (refocus ctx target-a)
              ctx)
        +args @(r/fmap->> link-ref (build-args+ ctx))
        [v' & vs] (->> +args (contrib.ct/unwrap (constantly nil))) ; EAV sugar is not interested in tuple case, that txfn is way off happy path
        ctx (assoc ctx :hypercrud.browser/eav (r/apply hypercrud.browser.context/stable-eav-v
                                                       [(:hypercrud.browser/eav ctx)
                                                        (r/track identity v')]))
        r+?route (r/fmap->> link-ref (build-route' +args ctx))]
    [ctx r+?route]))

(defn tree-invalid? "For popover buttons (fiddle level)" [ctx]
  (->> (:hypercrud.browser/validation-hints ctx)
       seq boolean))

(defn leaf-invalid? "The thing that styles red" [ctx]
  (->> (:hypercrud.browser/validation-hints ctx)
       (filter (comp nil? first))
       seq boolean))

(defn tempid! "unstable"
  ([ctx]
   (let [dbname (or (dbname ctx) "$")]                      ; (assert dbname "no dbname in ctx")
     ; If you don't like $, specify a :fiddle/pull-database or :fiddle/query
     (tempid! dbname ctx)))
  ([dbname ctx]                                             ; deprecated arity, i think
   @(r/fmap->> (runtime/state (:peer ctx) [::runtime/partitions])
               (hypercrud.util.branch/branch-val (uri dbname ctx) (:branch ctx))
               hash str)))

(defn- fix-param [ctx param]
  (if (instance? ThinEntity param)
    (smart-entity-identifier ctx param)                     ; throws away the dbname
    param))

(defn validate-query-params+ [q args ctx]
  (mlet [query-holes (try-either (hypercrud.browser.q-util/parse-holes q)) #_"normalizes for :in $"
         :let [db-lookup (->> (get-in ctx [:hypercrud.browser/domain :domain/databases])
                              (map (juxt :domain.database/name #(hc/db (:peer ctx) (get-in % [:domain.database/record :database/uri]) (:branch ctx))))
                              (into {}))
               ; Add in named database params that aren't formula params
               [params' unused] (loop [acc []
                                       args args
                                       [x & xs] query-holes]
                                  (let [is-db (clojure.string/starts-with? x "$")
                                        next-arg (if is-db (get db-lookup x)
                                                           (fix-param ctx (first args)))
                                        args (if is-db args (rest args))
                                        acc (conj acc next-arg)]
                                    (if xs
                                      (recur acc args xs)
                                      [acc args])))]]
    ;(assert (= 0 (count (filter nil? params')))) ; datomic will give a data source error
    ; validation. better to show the query and overlay the params or something?
    (cond #_#_(seq unused) (either/left {:message "unused param" :data {:query q :params params' :unused unused}})
      (not= (count params') (count query-holes)) (either/left {:message "missing params" :data {:query q :params params' :unused unused}})
      :else-valid (either/right params'))))

(defn hash-ctx-data [ctx]                                   ; todo there are collisions when two links share the same 'location'
  (let [{r-data :hypercrud.browser/data
         r-qfind :hypercrud.browser/qfind} ctx]
    ; why so defensive here?
    (when (some-> r-qfind deref)                            ; there must be data if there is qfind
      (condp some [(type @r-qfind)]
        #{FindRel FindColl} @(r/fmap->> r-data
                                        (mapv (r/partial stable-relation-key ctx))
                                        (into #{})
                                        hash)
        #{FindTuple FindScalar} @(r/fmap->> r-data (stable-relation-key ctx))))))

(defn tempid "stable" [ctx]
  ; recurse all the way up the path? just data + parent-data is relative not fully qualified, which is not unique
  ; is this just eav?
  (str @(:hypercrud.browser/eav ctx))
  #_(-> (str (:hypercrud.browser/pull-path ctx) "."
             (hash-ctx-data (:hypercrud.browser/parent ctx)) "."
             (hash-ctx-data ctx))
        hash str))
