(ns hypercrud.browser.context
  (:require
    [cats.core :refer [mlet return =<< fmap]]
    [cats.monad.either :as either :refer [left right either?]]
    [contrib.ct :refer [unwrap maybe maybe->either]]
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


; This file is coded in reactive style, which means no closures because they cause unstable references.
; All "closures" must be explicitly closed with a deftype that implements IEquiv, see helpers in contrib.reactive

(s/def :hypercrud/context
  (s/keys :opt [:hypercrud.browser/route
                :hypercrud.browser/schemas
                :hypercrud.browser/link-index
                :hypercrud.browser/result
                :hypercrud.browser/result-enclosure
                :hypercrud.browser/result-path
                :hypercrud.browser/fiddle
                :hypercrud.browser/result-index             ; private, interpreted relative to result-path. never individually
                :hypercrud.browser/qfind
                :hypercrud.browser/qfind-invalid-attrs
                :hypercrud.browser/eav                      ; V is scalar or nil. A is attr or fiddle-ident
                :hypercrud.browser/validation-hints
                :hypercrud.browser/element                  ; not included in EAV
                :hypercrud.browser/schema
                :hypercrud.browser/pull-enclosure
                :hypercrud.browser/pull-path                ; used only by refocus and links-in-dimension
                :hyperfiddle.runtime/branch-aux
                :hyperfiddle.ui.iframe/on-click
                :hypercrud.ui/display-mode
                :hyperfiddle.ui/layout
                :hyperfiddle.ui.sort/sort-col]))

(s/def :hypercrud.browser/eav r/reactive?)
(s/def :hypercrud.browser/result r/reactive?)
(s/def ::result-path-segment (s/or :element int? :attribute keyword? :relation string?
                                   :lookup-ref vector?
                                   :scalar (s/and some? (complement sequential?))
                                   ; nil is legal when entities are sparse
                                   ; https://github.com/hyperfiddle/hyperfiddle/issues/298
                                   :empty nil?))
(s/def :hypercrud.browser/result-path (s/coll-of ::result-path-segment :kind vector?))
(s/def :hypercrud.browser/element r/reactive?)
(s/def :hypercrud.browser/element-index int?)
(s/def :hypercrud.browser/schemas r/reactive?)
(s/def :hypercrud.browser/schema r/reactive?)
(s/def :hypercrud.browser/pull-path (s/coll-of keyword? :kind vector?))

(defn valid? [{:keys [:hypercrud.browser/qfind
                      :hypercrud.browser/fiddle] :as ctx}]
  (let [{:keys [:fiddle/type]} @fiddle]
    (cond
      (= type :blank) true
      (not qfind) (taoensso.timbre/warn "invalid qfind: " (pr-str (:hypercrud.browser/qfind ctx)))
      (:hypercrud.browser/qfind-invalid-attrs ctx) (taoensso.timbre/warn "invalid attrs: " (pr-str (:hypercrud.browser/qfind-invalid-attrs ctx)))
      :else true)))

(defn clean [ctx]
  ; Keeps the :peer which owns the schemas
  (dissoc ctx
          :hypercrud.ui/error
          :hyperfiddle.ui/layout
          :hypercrud.browser/head-sentinel
          :hypercrud.browser/attr-renderers
          :hypercrud.browser/schemas
          :hypercrud.browser/fiddle
          :hypercrud.browser/qfind
          :hypercrud.browser/result
          :hypercrud.browser/result-index
          :hypercrud.browser/result-enclosure
          :hypercrud.browser/result-path
          :hypercrud.browser/link-index
          :hypercrud.browser/eav
          :hypercrud.browser/element
          :hypercrud.browser/schema
          :hypercrud.browser/pull-enclosure
          :hypercrud.browser/pull-path
          :hypercrud.browser/parent
          :hypercrud.browser/route
          :hypercrud.browser/validation-hints))

(declare -infer-implicit-element)

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
  (some->
    ; Guard the implicit-element logic because this is called in tempid paths with an unbuilt context
    ; that i don't understand, this guard preserves old behavior
    ;(if (:hypercrud.browser/qfind ctx)
    ;  (-infer-implicit-element ctx)
    ;  ctx)
    ctx
    :hypercrud.browser/element deref
    :source :symbol str))

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

(defn lookup-ref [schema e-map]
  (if-let [a (contrib.datomic/find-identity-attr schema e-map)]
    [a (a e-map)]))

(defn smart-entity-identifier "key-entity except without the pr-str fallback.
  Generates the best Datomic lookup ref for a given pull. ctx needs :branch and :peer"
  ; flip params for fmap->
  [ctx {:keys [:db/id :db/ident] :as e-map}]                ; v can be a ThinEntity or a pull i guess
  (s/assert map? e-map)                                     ; used to be very loose, track down these cases now.
  ; If we have a color, and a (last path), ensure it is a ref.
  ; If we have a color and [] path, it is definitely a ref.
  ; If we have no color, it is a scalar or aggregate.

  ; If we're a new tempid this branch, the lookups are no good yet, must use the db/id
  ; (NOT the tempid, the db/id will be reversed to the tempid by actions/with)
  (or (if (underlying-tempid ctx id) id)
      ident
      ; Guard tangled tempid logic
      (some-> (:hypercrud.browser/schema ctx) deref (lookup-ref e-map)) ; Entity must already be inferred for this to work
      id
      ;(if-not (coll? e-map) e-map)                        ; id-scalar
      nil                                                   ; This is an entity but you didn't pull identity - error?
      ; Or it could be a relation. Why did this get called?
      ))

(defn summon-schemas-grouped-by-dbname [ctx]
  {:post [(every? #(satisfies? contrib.datomic/SchemaIndexedNormalized %) (vals %))]}
  (-> (->> @(hyperfiddle.runtime/state (:peer ctx) [:hyperfiddle.runtime/partitions (:branch ctx) :schemas])
           (contrib.data/map-keys #(hyperfiddle.domain/uri->dbname % (:hypercrud.browser/domain ctx))))
      (dissoc nil)))

(defn schemas "Dumb setup method, separates context from :peer and helps with tests etc"
  [ctx r-schemas]
  (assoc ctx :hypercrud.browser/schemas r-schemas))

(defn hydrate-attribute [ctx ident & ?more-path]
  (runtime/state (:peer ctx) (concat [::runtime/partitions (:branch ctx) :schemas (uri ctx)] (cons ident ?more-path))))

(defn- set-parent [ctx]
  (assoc ctx :hypercrud.browser/parent ctx))

(defn key-entity [ctx v]
  (if v
    (smart-entity-identifier ctx v)))

(defn reagent-entity-key "Specialized key that works around Datomic issue to stabilize views as we transition from
  empty entity to entity with datoms. Not useful for anything except as internal reagent key."
  [ctx {:keys [:db/id :db/ident] :as v}]
  ; https://github.com/hyperfiddle/hyperfiddle/issues/563 - Regression: Schema editor broken due to smart-id
  ; https://github.com/hyperfiddle/hyperfiddle/issues/345 - Form jank when tempid entity transitions to real entity
  (if v
    (or (underlying-tempid ctx id)                          ; Tempid is stable for entire view lifecycle
        (key-entity ctx v)
        #_(pr-str v))))

(defn key-scalar "For at the top when you don't know"
  [ctx v]
  (condp some [(type @(:hypercrud.browser/element ctx))]
    #{Aggregate Variable} v
    #{Pull} (key-entity ctx v)))

(declare element-head)
(declare spread-elements)
(declare qfind-level?)

(defn row-key "Properly accounts for elements/schema"       ; does it return nil, or a v?
  [{qfind :hypercrud.browser/qfind :as ctx} row]
  ; This keyfn is very tricky, read https://github.com/hyperfiddle/hyperfiddle/issues/341
  #_(let [qfind (contrib.datomic/qfind-collapse-findrel-1 @qfind row)])
  (cond
    (qfind-level? ctx)
    (condp some [(type @qfind)]
      #{FindColl FindScalar}
      (key-scalar (element-head ctx 0) row)
      #{FindRel FindTuple}
      (vec                                                    ; Tupled elements have tupled keys, except FindRel-1.
        (for [[i ctx] (spread-elements ctx element-head)]     ; Data is not focused yet, must sidestep that logic
          (key-scalar ctx (get row i)))))

    :else
    (key-scalar ctx row)))

(def ^:export ^:legacy row-keyfn row-key)
(def ^:export ^:legacy key-row row-key)

(defn stable-eav-v [[?e a _] ?v']
  {:pre [a
         #_?v'                                              ; Never know with ?v, did they pull identity?
         ]}
  [?e a ?v'])

(defn stable-eav-av "v becomes e. In top cases, this is nil->nil; only Pulls have defined E.
  ?v' can be nil - sparse results."
  [[_ _ ?v] a' ?v']
  ;{:pre [a']} ; failurecase can be nil a'
  [?v a' ?v'])

(defn stable-eav-a [[_ _ v] a']
  ; v becomes e
  [v a' nil])

;(defprotocol EAV
;  (eav [this])
;  (e [this])
;  (a [this])
;  (v [this]))

(defn link-identifiers [link]
  (let [idents (-> (set (:link/class link))                 ; for narrowing if same fiddle in several places? legacy?

                   ; generally name links like this
                   (conj (some-> link :link/fiddle :fiddle/ident))

                   ; fast way to name a button without a fiddle
                   (conj (some-> link :link/tx-fn (subs 1) keyword))

                   ; link/a is probably not useful except for narrowing, maybe
                   ; nil path is no longer defined, use fiddle-ident instead
                   (conj (some-> link :link/path hyperfiddle.fiddle/read-path))

                   ; nil #{txfn linkpath fiddleident} is not meaningful
                   (disj nil))]                             ; Preserve set type
    [idents link]))

(defn -indexed-links-at [links]
  (map link-identifiers links))

(defn depth [ctx]
  (count (:hypercrud.browser/result-path ctx)))

(defn pull-depth [ctx]
  (count (:hypercrud.browser/pull-path ctx)))

(defn qfind-level? [ctx]
  (= (pull-depth ctx) 0))

(declare data)

(defn row-key-v [ctx row]
  (or (row-key ctx row) row))

(defn entity-key-v [ctx v]
  (or (key-entity ctx v) v))

(defn index-result "Builds the result index based on qfind. Result index is orthogonal to sorting or order.
  All paths get a result-index, though if there is no collection, then this is just the result as no indexing is needed.
  (data ctx) is no good until after this is done.
  Works at fiddle and attribute level. Currently we do not index at element level (rather it is already indexed by vec index)"
  ([ctx]
   {:pre [(= (depth ctx) 0)]}
   (let [qfind (:hypercrud.browser/qfind ctx)]
     (cond                                                  ; fiddle | nested attr (otherwise already spread)

       (not qfind)
       (assoc ctx :hypercrud.browser/result-index (:hypercrud.browser/result ctx))

       (= (depth ctx) 0)                                    ; fiddle level
       (let [{r-qfind :hypercrud.browser/qfind} ctx
             r-ordered-result (:hypercrud.browser/result ctx)]

         (condp = (type @r-qfind)
           FindRel
           (assoc ctx :hypercrud.browser/result-index
                      (r/fmap->> r-ordered-result (contrib.data/group-by-unique (partial row-key-v ctx))))

           FindColl
           (assoc ctx :hypercrud.browser/result-index
                      (r/fmap->> r-ordered-result (contrib.data/group-by-unique (partial row-key-v ctx))))

           FindTuple
           ; Vectors are already indexed
           (assoc ctx :hypercrud.browser/result-index r-ordered-result)

           FindScalar
           ; Scalar has no index at all
           (assoc ctx :hypercrud.browser/result-index r-ordered-result)))
       :else (assert false)
       )))
  ([ctx a]                                                  ; eav order of init issues, ::eav depends on this in :many
   {:pre [(> (depth ctx) 0)]}
   (assert (not (:hypercrud.browser/head-sentinel ctx)) "this whole flag is trouble, not sure if this assert is strictly necessary")
   (case (contrib.datomic/cardinality @(:hypercrud.browser/schema ctx) a)

     :db.cardinality/one
     ctx

     :db.cardinality/many
     ; Group again by keyfn, we have seq and need lookup
     ; Deep update result in-place, at result-path, to index it. Don't clobber it!
     ; hang onto the set as we index for a future rowkey
     (let [set-ungrouped (r/cursor (:hypercrud.browser/result-index ctx) (:hypercrud.browser/result-path ctx))]
       (assoc ctx :hypercrud.browser/result set-ungrouped   ; replace with refined new tree
                  :hypercrud.browser/result-index
                  (r/fmap-> (:hypercrud.browser/result-index ctx)
                            (update-in (:hypercrud.browser/result-path ctx)
                                       (cond
                                         (contrib.datomic/ref? @(:hypercrud.browser/schema ctx) a)
                                         (partial contrib.data/group-by-unique (partial entity-key-v ctx))

                                         :scalar
                                         ; Sets are an index that evaluate to the key.
                                         set))))))))

(defn data "Works in any context and infers the right stuff"
  [{qfind :hypercrud.browser/qfind :as ctx}]
  {:pre [ctx]}
  ; TODO validate that there is a row and element if required, and throw a spec error if not.
  ;
  ; Data may be responsible for indexing the result, because it depends on the key paths.
  ; They can be db/id, db/ident, lookupref, alt lookupref. It depends what was pulled.
  ; Userland may use a non-canonical lookup ref and it should work.
  ;
  ; Result-index is precomputed to match the expected path,
  ; in some cases it is just the result (no indexing was done)
  ; The user never sees a raw index, it is just for internal lookups.
  (if qfind
    ; check head-sentinel?
    (let [ctx (-infer-implicit-element ctx)
          {:keys [:hypercrud.browser/element
                  :hypercrud.browser/result
                  :hypercrud.browser/result-index
                  :hypercrud.browser/result-path]} ctx]
      (cond
        (qfind-level? ctx)
        (if result-path
          @(r/cursor result-index result-path)
          @result)

        :else
        (let [k (last result-path)
              is-awaiting-rowkey (and (keyword? k)
                                      (= :db.cardinality/many (contrib.datomic/cardinality
                                                                @(:hypercrud.browser/schema ctx) k)))]
          (if is-awaiting-rowkey
            @result                                         ; We hang on to sets as we descend
            @(r/cursor result-index result-path))))
      )))

; It's just easier to end the reaction earlier
(defn v! "returns scalar | identity | lookupref | nil."     ; never a fallback v
  [{:keys [:hypercrud.browser/element] :as ctx}]
  {:pre [element]}                                          ; infer the element above this?
  ; Sparse resultset, v can still be nil, or fully refined to a scalar

  (let [?a (last (:hypercrud.browser/pull-path ctx))
        ?v (data ctx)]
    (cond

      (nil? ?v)
      nil

      (:hypercrud.browser/head-sentinel ctx)                ; do in (data)?
      nil

      ; Attribute level first, makes element level easier
      (> (pull-depth ctx) 0)
      (if (contrib.datomic/ref? @(:hypercrud.browser/schema ctx) ?a)
        (key-entity ctx ?v)                                 ; no fallback, this must be a real identity or nil
        ?v)

      ; Change in behavior below, it was smart-entity-identifier before
      ; which impacts formula vs view

      ; We already have an element and we already know which
      (= (pull-depth ctx) 0)                                ; element level confirmed
      (condp some [(type @(:hypercrud.browser/qfind ctx))]
        #{FindRel FindColl} (if (> (depth ctx) 0)           ; need row
                              (key-scalar ctx ?v))
        #{FindTuple FindScalar} (key-scalar ctx ?v)))))

(defn eav "Not reactive." [ctx]
  {:pre [(s/assert :hypercrud/context ctx)]}
  ; Should you use this or ::eav? Userland renderers call this.
  ; Context internals use ::eav. I think.
  (let [ctx (-infer-implicit-element ctx)]
    @(:hypercrud.browser/eav ctx)))

(defn e [ctx]
  (let [[e _ _] (eav ctx)]
    e))

(defn row "Row does not set E. E is the parent, not the child, and row is analogous to :ref :many child."
  [ctx & [k]]
  {:pre [(s/assert :hypercrud/context ctx)
         #_(not (coll? k))]
   :post [(s/assert :hypercrud/context %)]}
  (s/assert ::result-path-segment k)
  (as-> ctx ctx
        ; What if k is not the canonical key? db/id vs identity, etc
        (update ctx :hypercrud.browser/result-path (fnil conj []) k)
        ; Not pullpath, that is irrespective of the actual data
        ; I don't think row should set v. Only if we infer an element can we set v.
        ; ::eav tells us exactly where we are without optimisim. (eav) infers the rest.

        (if (qfind-level? ctx)
          ; ::eav depends on element which isn't known yet
          ; (eav) can infer this, but keep the "cursor" precise
          ctx

          ; In nested case we do know precisely the eav has changed
          (assoc ctx :hypercrud.browser/eav (r/fmap-> (:hypercrud.browser/eav ctx) (stable-eav-v (v! ctx)))))))

(defn ^:export spread-rows "spread across resultset row-or-rows; returns [k ctx] for your react key.
  Automatically accounts for query dimension - no-op in the case of FindTuple and FindScalar.
  Can be skipped in head case, or can address a row directly if you know the keyfn.
  Accounts for row order and handles client sorting."
  [ctx & [sort-fn]]
  {:pre [(:hypercrud.browser/qfind ctx)]}
  (s/assert :hypercrud/context ctx)
  (let [ctx (assoc ctx :hypercrud.browser/head-sentinel false) ; hack
        sort-fn (or sort-fn identity)]
    (cond                                                   ; fiddle | nested attr (otherwise already spread)
      (= (depth ctx) 0)                                     ; fiddle level
      (let [{r-qfind :hypercrud.browser/qfind} ctx
            ; NOT result-indexed, use the raw one. It's been indexed already, can't use that, order is lost.
            r-ordered-result (:hypercrud.browser/result ctx)]
        (condp = (type @r-qfind)
          FindColl
          (for [[?k k] (->> (sort-fn @r-ordered-result)     ; client side sorting – should happen in backend
                            (map (juxt (partial row-key ctx)
                                       (partial row-key-v ctx))))]
            [?k (row ctx k)])

          FindRel
          (for [[?k k] (->> (sort-fn @r-ordered-result)     ; client side sorting – should happen in backend
                            (map (juxt (partial row-key ctx)
                                       (partial row-key-v ctx))))]
            [?k (row ctx k)])

          FindTuple                                         ; no index
          (let [?k (row-key ctx @r-ordered-result)]
            [[?k ctx]])

          FindScalar                                        ; no index
          (let [?k (row-key ctx @r-ordered-result)]
            [[?k ctx]])))

      ; where is the cardinality test? can elide, assume :many if they called spread-rows down here.
      ; Better also be past qfind level and into a pull
      (> (depth ctx) 0)                                     ; nested attr
      (let [r-ordered-result (data ctx)]                    ; remember row order; this is broken check order
        (for [[?k k] (->> (sort-fn r-ordered-result)
                          (map (juxt (partial row-key ctx)
                                     (partial row-key-v ctx))))]
          [?k (row ctx k)])))))

(defn fiddle "Runtime sets this up, it's not public api.
  Responsible for setting defaults.
  Careful this is highly sensitive to order of initialization."
  [ctx r-fiddle]
  {:pre [r-fiddle
         (:hypercrud.browser/schemas ctx)
         (-> (:hypercrud.browser/schemas ctx) deref count (> 0))]}
  (let [r-fiddle (r/fmap hyperfiddle.fiddle/apply-defaults r-fiddle)
        r-qparsed (r/fmap-> r-fiddle hyperfiddle.fiddle/parse-fiddle-query)
        r-qfind (r/fmap :qfind r-qparsed)
        r-fiddle (r/fmap->> (r/sequence [r-fiddle (:hypercrud.browser/schemas ctx) r-qparsed])
                            (apply hyperfiddle.fiddle/apply-fiddle-links-defaults))
        element-validation-issues (if-let [qfind @r-qfind]
                                    (seq (contrib.datomic/validate-qfind-attrs
                                           @(:hypercrud.browser/schemas ctx) qfind)))]
    (as-> ctx ctx
          (assoc ctx :hypercrud.browser/fiddle r-fiddle)
          (if r-qfind
            (if @r-qfind
              (as-> ctx ctx
                    (assoc ctx :hypercrud.browser/qfind r-qfind
                               :hypercrud.browser/qparsed r-qparsed)
                    (if element-validation-issues
                      (assoc ctx :hypercrud.browser/qfind-invalid-attrs element-validation-issues)
                      ctx))
              ctx)
            ctx)
          (assoc ctx :hypercrud.browser/link-index (r/fmap->> r-fiddle :fiddle/links (map link-identifiers)))
          (assoc ctx :hypercrud.browser/eav (r/apply stable-eav-av
                                                     [(r/pure nil)
                                                      (r/fmap :fiddle/ident r-fiddle)
                                                      (r/pure nil)]))
          ; push this down, it should be nil now
          (assoc ctx :hypercrud.browser/pull-path []))))

(defn result [ctx r-result]
  {:pre [r-result
         (:hypercrud.browser/fiddle ctx)]}
  (as-> ctx ctx
        (assoc ctx :hypercrud.browser/result r-result)      ; can be nil if no qfind
        (if (:hypercrud.browser/qfind ctx)
          (if @(:hypercrud.browser/qfind ctx)
            (assoc ctx :hypercrud.browser/result-enclosure
                       (r/ctxf contrib.datomic/result-enclosure ctx
                               :hypercrud.browser/schemas
                               :hypercrud.browser/qfind
                               :hypercrud.browser/result))
            ctx)
          ctx)
        (assoc ctx :hypercrud.browser/validation-hints
                   (contrib.validation/validate
                     (s/get-spec @(r/fmap-> (:hypercrud.browser/fiddle ctx) :fiddle/ident))
                     @(:hypercrud.browser/result ctx)
                     (partial row-key ctx)))                ; i dont think fallback v

        ; index-result implicitly depends on eav in the tempid reversal stable entity code.
        ; Conceptually, it should be after qfind and before EAV.
        (index-result ctx)                                  ; in row case, now indexed, but path is not aligned yet
        ))

(defn stable-element-schema [schemas element]
  (let [{{db :symbol} :source} element]
    (if db
      (get schemas (str db)))))

; This complects two spread-elements concerns which are separate.
; 1. Setting Schema and element
; 2. Setting result and result related things
(defn element-head [ctx i]
  (let [r-element (r/fmap-> (:hypercrud.browser/qfind ctx) datascript.parser/find-elements (get i))]
    (as-> ctx ctx
          (assoc ctx :hypercrud.browser/element r-element)
          (assoc ctx :hypercrud.browser/schema (r/ctxf stable-element-schema ctx
                                                       :hypercrud.browser/schemas
                                                       :hypercrud.browser/element)))))

(defn element [ctx i]                                       ; [nil :seattle/neighborhoods 1234345]
  {:pre []
   :post [(s/assert r/reactive? (:hypercrud.browser/qfind %)) ; qfind set in base if it is available
          (s/assert r/reactive? (:hypercrud.browser/schema %))]}
  (let [r-pull-enclosure (r/fmap-> (:hypercrud.browser/result-enclosure ctx) (get i))]
    (as-> ctx ctx
          (element-head ctx i)
          (assoc ctx :hypercrud.browser/root-pull-enclosure r-pull-enclosure) ; for refocus
          (assoc ctx :hypercrud.browser/pull-enclosure r-pull-enclosure)
          (condp some [(type @(:hypercrud.browser/qfind ctx))]
            #{FindRel FindTuple} (as-> ctx ctx
                                       (update ctx :hypercrud.browser/result-path (fnil conj []) i)
                                       ; I don't think we index result here, uncertain.
                                       ; Related to collapse FindRel-N to FindColl
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
  (let [qfind @(:hypercrud.browser/qfind ctx)]
    (cond
      (:hypercrud.browser/element ctx)
      ctx

      (#{FindColl FindScalar} (type qfind))
      (element ctx 0)

      ; Can't infer this because the result index is not the right shape
      ;(and (#{FindRel FindTuple} (type qfind))
      ;     (= 1 (count (datascript.parser/find-elements qfind))))
      ;(element ctx 0)

      :else
      ctx)))

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

    ; Schema aliases can crash here https://github.com/hyperfiddle/hyperfiddle.net/issues/182
    #_(if-not (contrib.datomic/cardinality @(:hypercrud.browser/schema ctx) a')
      ctx)
    (as->
      ctx ctx
      (-infer-implicit-element ctx)                         ; ensure pull-enclosure
      (-validate-qfind-element ctx)
      (set-parent ctx)
      (update ctx :hypercrud.browser/pull-path (fnil conj []) a') ; what is the cardinality? are we awaiting a row?
      (if (:hypercrud.browser/head-sentinel ctx)
        ctx                                                 ; no result-path in head, or result
        (as-> ctx ctx
              (update ctx :hypercrud.browser/result-path (fnil conj []) a')
              ; Guaranteed depth >= 1 due to stmt ordering
              (index-result ctx a')))
      ; V is for formulas, E is for security and on-change. V becomes E. E is nil if we don't know identity.
      (assoc ctx :hypercrud.browser/eav                     ; insufficent stability on r-?v? fixme
                 (case (contrib.datomic/cardinality @(:hypercrud.browser/schema ctx) a')
                   :db.cardinality/many (r/fmap-> (:hypercrud.browser/eav ctx) (stable-eav-a a')) ; dont have v yet
                   :db.cardinality/one (r/fmap-> (:hypercrud.browser/eav ctx) (stable-eav-av a' (v! ctx)))
                   ; Gracefully fail but still render.
                   nil (r/fmap-> (:hypercrud.browser/eav ctx) (stable-eav-av nil nil))))

      ; in :hf/new :identity case, E can be nil but we are about to have a tempid in the E
      ; It is not our job here to focus that. I dont think we can set the E to the tempid in that case.
      )))

(defn validation-hints-here [ctx]
  (for [[path hint] (:hypercrud.browser/validation-hints ctx)
        :when (= path (:hypercrud.browser/result-path ctx))
        :let [a (last path)]]
    [a hint]))

(defn tree-invalid? "For popover buttons (fiddle level)" [ctx]
  (->> (validation-hints-here ctx)
       seq boolean))

(defn leaf-invalid? "The thing that styles red" [ctx]
  (->> (validation-hints-here ctx)
       (filter (comp nil? first))
       seq boolean))

(defn pull-enclosure-here "safe to be called anywhere"
  [ctx]
  (when (:hypercrud.browser/element ctx)
    (contrib.datomic/pullshape-get-in
      @(:hypercrud.browser/pull-enclosure ctx)
      (:hypercrud.browser/pull-path ctx))))

(defn focus "Unwind or go deeper, to where we need to be, within same dimension.
    Throws if you focus a higher dimension.
    This is about navigation through pulledtrees which is why it is path-oriented."
  [ctx relative-path]
  {:pre [(s/assert :hypercrud/context ctx)
         (:hypercrud.browser/result ctx)]
   :post [(s/assert :hypercrud/context %)]}
  ; Is this legacy compat?
  (reduce (fn [ctx p]
            (cond
              (int? p) (element ctx p)
              ;(#{:db/id #_:db/ident} p) ctx                 ; v is already properly set
              ; db/id is renderable, address it. The controls will unwind to parent ctx. Tangled
              (keyword? p) (attribute ctx p)
              :else (assert false (str "illegal focus: " p))))
          ctx relative-path))

(defn ^:export spread-result "Guards :fiddle/type :blank to guarantee a qfind.
  Use this with `for` which means reagent needs the key."
  [ctx]
  (let [r-fiddle (:hypercrud.browser/fiddle ctx)]
    ; could also dispatch on qfind. Is fiddle/type unnecessary now?
    (condp some [(:fiddle/type @r-fiddle)]
      #{:blank} []
      #{:query :entity} [[(:fiddle/ident @r-fiddle) ctx]])))

(defn ^:export spread-elements "yields [i ctx] foreach element.
  All query dimensions have at least one element.
  Default arity is for userland.
  Second arity is for keyfns who need elements but not data."
  [ctx & [f]]
  {:pre [(:hypercrud.browser/qfind ctx)
         (not (:hypercrud.browser/element ctx))]
   #_#_:post [(s/assert :hypercrud/context %)]}
  (let [r-qfind (:hypercrud.browser/qfind ctx)]
    ; No unsequence here? What if find elements change? Can we use something other than (range) as keyfn?
    (for [i (range (count (datascript.parser/find-elements @r-qfind)))]
      [i ((or f element) ctx i)])))

(defn spread-attributes "not recursive, just one entity level"
  [ctx]
  (let [{:keys [:hypercrud.browser/element] :as ctx} (-infer-implicit-element ctx)]
    (condp = (type @element)
      Variable []
      Aggregate []
      Pull (for [k (contrib.datomic/pull-level (pull-enclosure-here ctx))]
             [k (attribute ctx k)]))))

; var first, then can always use db/id on row. No not true – collisions! It is the [?e ?f] product which is unique

; keyfn must support entities & scalars (including aggregates)
; scalars may not be unique
; entities might not be unique either, its the combination that is unique
; lift out of current context in this case?
; Make sure to optimize the single-fe case to use the identity/value which will be unique

(defn link-criteria-match? [?corcs [index-key v]]
  (clojure.set/superset? index-key (contrib.data/xorxs ?corcs #{})))

(defn links-at "where criterias is some of #{ident txfn class class2}.
  Links include all links reachable by navigating :ref :one. (Change this by specifying an :ident)
  The index internals are reactive."
  [r-link-index criterias]                                           ; criterias can contain nil, meaning toptop
  (r/fmap->> r-link-index
       (filter (partial link-criteria-match? criterias))
       (mapv second)
       #_(mapv (juxt :db/id identity))))

(defn links-in-dimension' [ctx criterias]
  {:post [(not (r/reactive? %))]}
  (let [?element (some-> ctx :hypercrud.browser/element deref)
        ?schema (some-> ctx :hypercrud.browser/schema deref)
        ?pullpath (:hypercrud.browser/pull-path ctx)]
    (if-not (and ?element ?schema ?pullpath)
      @(links-at (:hypercrud.browser/link-index ctx) criterias)
      (let [pull-pattern (get-in ?element [:pattern :value])
            ; if pullpath is [], add fiddle-ident. Or if EAV a is not a keyword.
            ; EAV A may already be the fiddle-ident, or if its an int, use fiddle-ident too.
            ; reachable link locations, not just attrs.
            as (contrib.datomic/reachable-attrs ?schema (contrib.datomic/pull-shape pull-pattern) ?pullpath)
            links (->> as
                       ; Places within reach
                       (mapcat (fn [a]
                                 @(links-at (:hypercrud.browser/link-index ctx) (conj criterias a))))

                       ; This place is where we are now
                       (concat @(links-at (:hypercrud.browser/link-index ctx) criterias)))] ; this causes duplicates, there are bugs here
        (vec (distinct links))                              ; associative by index
        #_(->> links r/sequence (r/fmap vec))))))

(defn links-in-dimension-r [ctx criterias]
  (r/track links-in-dimension' ctx criterias))

(defn unwind [ctx n]
  ((apply comp (repeat n :hypercrud.browser/parent)) ctx))

(defn refocus-in-element+
  [ctx a]
  {:pre [(:hypercrud.browser/element ctx)]
   :post [(s/assert either? %)
          #_(let [[_ aa _] @(:hypercrud.browser/eav %)] (= a aa))]}
  (cond
    (contrib.datomic/unique? @(:hypercrud.browser/schema ctx) a :db.unique/identity)
    (if (qfind-level? ctx)
      (right ctx)                                           ; [nil :dustingetz.tutorial/blog [:dustingetz.post/slug :hehehe]]
      (right (unwind ctx 1)))

    (let [[_ aa _] @(:hypercrud.browser/eav ctx)] (= aa a))
    (right ctx)

    :else
    (let [current-path (:hypercrud.browser/pull-path ctx)
          ; find a reachable path that contains the target-attr in the fewest hops
          ;   me->mother ; me->sister->mother ; closest ctx is selected
          ; What if there is more than one?  me->sister->mother; me->father->mother
          ; Ambiguous, how did we even select this link? Probably need full datascript query language.
          path-solutions (->> (contrib.datomic/reachable-pullpaths
                                @(:hypercrud.browser/schema ctx)
                                @(:hypercrud.browser/root-pull-enclosure ctx)
                                current-path)
                              ; xs is like '([] [:dustingetz.reg/gender] [:dustingetz.reg/shirt-size])
                              (filter #(some (partial = a) %)))]
      ; In tuple cases (off happy path) there might not be a solution. Can be fixed by tupling all the way up.
      (if (seq path-solutions)                              ; found at least one solution
        ; Warn if more than one solution? Diamonds are fine, but some are parallel solns
        (let [chosen-path (->> path-solutions
                               (map (partial take-while (partial not= a)))
                               (sort-by count)              ; choose the shortest path, if ambiguous could be a problem
                               first)                       ; nil/empty means unwind to nop
              common-ancestor-path (ancestry-common current-path chosen-path)
              unwind-offset (- (count current-path) (count common-ancestor-path)) ; 1 > 0 is fine
              common-ancestor-ctx (unwind ctx unwind-offset)

              foo (if (contrib.datomic/unique? @(:hypercrud.browser/schema ctx) a :db.unique/identity)
                    ; FOR LINK REFOCUS ONLY, the prent entity ctx is correct
                    chosen-path
                    (conj (vec chosen-path) a))]
          (right
            (focus common-ancestor-ctx (ancestry-divergence foo current-path))))

        ; No solution found.
        ; Sparse resultset, or possibly a constraint failure (userland asked for something impossible?)
        (left (str "unable to satisfy link at: " a))))))

(defn refocus+ "From view, find the closest satisfactory ctx accounting for cardinality. e.g. as in
  !link(:new-intent-naive :hf/remove). Caller believes that the deps are satisfied (right stuff is in scope).
  todo unify with refocus'.
  This is ALWAYS focusing a link.
  Handle :identity differently for links – we are not rendering forms, we are generating URLs,
  which means :identity attributes need to name an entity, not a scalar. (This fn is complected with build-args+)

  Notably: (refocus) and (attribute) are pretty much the same thing. This one has element tupling hacks
  and handles :identity differently, now."
  [ctx a']
  {:pre [ctx #_a']                                          ; disable assert for backwards compat until all links have a
   ; nil return is allowed, does this return either?
   :post [(s/assert either? %)]}
  (cond
    (not a')                                                ; backwards compat with Dec 2018 link/path
    (right ctx)

    (= a' (@(:hypercrud.browser/fiddle ctx) :fiddle/ident))
    ; if no element, we already there (depth = 0)
    (right (unwind ctx (pull-depth ctx)))

    (:hypercrud.browser/element ctx)
    (refocus-in-element+ ctx a')

    (let [qfind @(:hypercrud.browser/qfind ctx)]
      (and (#{FindRel FindTuple} (type qfind))
           (= 1 (count (datascript.parser/find-elements qfind)))))
    (refocus-in-element+ (element ctx 0) a')

    :else
    ; This never happens in autogrids, only in custom views.
    ; If the custom view is on a FindRel-1, this does the right thing.
    ; If the custom view is a FindRel-N, then this needs to tuple all the way up.
    ; It is probably not what they want. They should make an element ctx and place
    ; each link individually. This hack probably never needs to get fixed.
    (refocus-in-element+ (element ctx 0) a')))

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

(defn tag-v-with-color' [ctx v]
  (if v
    (->ThinEntity (or (dbname ctx) "$")                     ; busted element level
                  v)))

(defn tag-element-v-with-color [{:keys [:hypercrud.browser/element] :as ctx} v]
  {:pre [element]}
  (condp some [(type @element)]
    #{Pull} (tag-v-with-color' ctx v)
    #{Aggregate Variable} v))

(defn tag-v-with-color "Tag dbids with color, at the last moment before they render into URLs"
  [ctx v]
  (let [[e a _] @(:hypercrud.browser/eav ctx)               ; this v might be a tuple, above v is one item in tuple?
        is-element-level (= (pull-depth ctx) 0)]
    (cond
      (instance? ThinEntity v) v                            ; legacy compat with IDE legacy #entity formulas

      is-element-level                                      ; includes hf/new
      (do
        (assert (:hypercrud.browser/qfind ctx) ":blank fiddle (no qfind) with hf/new is illegal, specify a qfind.")

        ; Includes FindColl and FindScalar inferred above
        ; We can hack in FindRel-1 support here too
        (let [ctx (-infer-implicit-element ctx)             ; don't infer FindRel-1, the result index isn't shaped right
              element (:hypercrud.browser/element ctx)]
          (if element
            (tag-element-v-with-color ctx v)

            ; Qfind tuple case without an element in the ctx, means we have to handle all N elements in parallel.
            ; Example: [:find (pull ?e [:post/slug]) (pull ?f [:post/slug])] should generate both links.
            ; Currently this is an error case. If we push (mapv args) down to this level it can be done.
            (do (timbre/warn "element-level links aren't well defined for tupled qfind: "
                             ; FindRel and FindTuple
                             @(:hypercrud.browser/qfind ctx))
                nil))))

      (contrib.datomic/ref? @(:hypercrud.browser/schema ctx) a)
      (tag-v-with-color' ctx v)

      ; :dustingetz/slack-storm :new-storm exercises this branch
      (contrib.datomic/unique? @(:hypercrud.browser/schema ctx) a :db.unique/identity)
      (if e                                                 ; tempid | lookup-ref | nil (:hf/new)
        (tag-v-with-color' ctx e)
        (tag-v-with-color' ctx v))                          ; :hf/new has nil e, but valid tempid v

      :scalar
      v)))

(let [eval-string!+ (memoize eval/eval-expr-str!+)]
  (defn build-args+ "Params are EAV-typed (uncolored)"
    [ctx                                                    ;There is a ctx per argument if we are element-level tuple.
     {:keys [:link/fiddle :link/tx-fn] :as link}]
    {:post [(s/assert either? %)]}
    ; if at element level, zip with the find-elements, so do this N times.
    ; That assumes the target query is a query of one arg. If it takes N args, we can apply as tuple.
    ; If they misalign thats an error. Return the tuple of args.
    (mlet [formula-ctx-closure (if-let [formula-str (contrib.string/blank->nil (:link/formula link))]
                                 (eval-string!+ (str "(fn [ctx] \n" formula-str "\n)"))
                                 (either/right (constantly (constantly nil))))
           formula-fn (try-either (formula-ctx-closure ctx))

           ; V legacy is tuple, it should be scalar by here (tuple the ctx, not the v)
           :let [[e a v] @(:hypercrud.browser/eav ctx)]

           ; Documented behavior is v in, tuple out, no colors.
           arg (try-either (formula-fn v))]
      ; Don't normalize, must handle tuple dimension properly.
      ; For now assume no tuple.
      (return
        ; !link[⬅︎ Slack Storm](:dustingetz.storm/view)
        (if arg [arg])))))

(defn ^:export build-route+ "There may not be a route! Fiddle is sometimes optional" ; build-route+
  [args ctx {:keys [:link/fiddle :link/tx-fn] :as link}]
  {:pre [ctx]
   :post [(s/assert either? %)]}
  (mlet [fiddle-id (if fiddle
                     (right (:fiddle/ident fiddle))
                     (left {:message ":link/fiddle required" :data {:link link}}))
         ; Why must we reverse into tempids? For the URL, of course.
         :let [colored-args (mapv (partial tag-v-with-color ctx) args)] ; this ctx is refocused to some eav
         route (id->tempid+ (hyperfiddle.route/canonicalize fiddle-id colored-args) ctx)
         route (hyperfiddle.route/validate-route+ route)]
    (return route)))

(defn refocus-to-link+ "focus a link ctx, accounting for link/formula which occludes the natural eav"
  [ctx {:keys [:link/fiddle :link/tx-fn :link/path] :as link}]
  {:pre [(s/assert :hypercrud/context ctx)]
   :post [(s/assert either? %)]
   #_#_:post [(s/assert (s/cat :ctx :hypercrud/context :route r/reactive?) %)]}
  (let [target-a (hyperfiddle.fiddle/read-path path)]
    (mlet [ctx (refocus+ ctx target-a)
           [v' & vs :as args] (build-args+ ctx link)
           ; :hf/remove doesn't have route by default, :hf/new does, both can be customized
           ?route (if (and (not fiddle) tx-fn)
                    (right nil)
                    (build-route+ args ctx link))
           ; EAV sugar is not interested in tuple case, that txfn is way off happy path
           :let [ctx (assoc ctx :hypercrud.browser/eav (r/apply stable-eav-v
                                                                [(:hypercrud.browser/eav ctx)
                                                                 (r/track identity v')]))]]
      (right
        [ctx ?route]))))

(defn- fix-param [ctx param]
  (if (instance? ThinEntity param)
    ; I think it already has the correct identity and tempid is already accounted
    #_(smart-entity-identifier ctx param)
    (.-id param)                                            ; throw away dbname
    param))

(defn validate-query-params+ [q args ctx]
  (mlet [query-holes (try-either (hypercrud.browser.q-util/parse-holes q)) #_"normalizes for :in $"
         :let [db-lookup (->> (get-in ctx [:hypercrud.browser/domain :domain/databases])
                              (map (juxt :domain.database/name
                                         #(hc/db (:peer ctx)
                                                 (get-in % [:domain.database/record :database/uri])
                                                 (:branch ctx))))
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
      (not= (count params') (count query-holes))
      (either/left {:message "missing params" :data {:query q :params params' :unused unused}})

      :else-valid
      (either/right params'))))

(defn tempid "stable" [ctx]
  ; recurse all the way up the path? just data + parent-data is relative not fully qualified, which is not unique
  ; is this just eav?
  (->> (map pr-str (eav ctx))
       (cons "hyperfiddle.tempid")
       (clojure.string/join "-")))

(defn tempid! "unstable"
  ([ctx]
    ; :blank can assume $; otherwise user should specify a qfind
    ; ^ is old comment and can't this be removed now?
   (tempid! (or (dbname ctx) "$") ctx))
  ([dbname ctx]
    ; Use hash of current dbval, which changes with each edit
   @(r/fmap->> (runtime/state (:peer ctx) [::runtime/partitions])
               (hypercrud.util.branch/branch-val (uri dbname ctx) (:branch ctx))
               (str "hyperfiddle.tempid-"))))