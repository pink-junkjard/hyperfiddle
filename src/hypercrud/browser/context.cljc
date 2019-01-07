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


(s/def :hypercrud/context
  (s/keys :opt [:hypercrud.browser/data
                :hypercrud.browser/eav]))

(s/def :hypercrud.browser/eav r/reactive?)
(s/def :hypercrud.browser/data r/reactive?)

(defn clean [ctx]
  (dissoc ctx
          :hypercrud.ui/error
          :hyperfiddle.ui/layout

          :hypercrud.browser/attr-renderers
          :hypercrud.browser/schemas
          :hypercrud.browser/data
          :hypercrud.browser/data-index
          :hypercrud.browser/link-index
          :hypercrud.browser/fiddle
          :hypercrud.browser/qfind
          :hypercrud.browser/element
          :hypercrud.browser/element-index
          :hypercrud.browser/eav


          :hypercrud.browser/parent
          :hypercrud.browser/path
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

(defn target-route [ctx] @(runtime/state (:peer ctx) [::runtime/partitions nil :route]))

(defn dbname [ctx]
  (some-> (:hypercrud.browser/element ctx) deref :source :symbol str))

(defn uri
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

(defn summon-schemas-grouped-by-dbname [ctx]
  {:post [(every? #(satisfies? contrib.datomic/SchemaIndexedNormalized %) (vals %))]}
  (-> (->> @(hyperfiddle.runtime/state (:peer ctx) [:hyperfiddle.runtime/partitions (:branch ctx) :schemas])
           (contrib.data/map-keys #(hyperfiddle.domain/uri->dbname % (:hypercrud.browser/domain ctx))))
      (dissoc nil)))

(defn hydrate-attribute [ctx ident & ?more-path]
  (runtime/state (:peer ctx) (concat [::runtime/partitions (:branch ctx) :schemas (uri ctx)] (cons ident ?more-path))))

(defn- set-parent [ctx]
  (assoc ctx :hypercrud.browser/parent (dissoc ctx :hypercrud.browser/data)))

(defn- set-parent-data [ctx]
  (update ctx :hypercrud.browser/parent (fnil into {}) (select-keys ctx [:hypercrud.browser/data])))

(defn identify [ctx]
  {:post [(not (map? %))]}
  ; When looking at an attr of type ref, figure out it's identity, based on all the ways it can be pulled.
  ; What if we pulled children without identity? Then we can't answer the question (should assert this)
  (if-let [data (:hypercrud.browser/data ctx)]              ; Guard is for txfn popover call site
    (or @(contrib.reactive/cursor data [:db/ident])
        @(contrib.reactive/cursor data [:db/id])
        @data)))

(defn stable-eav-v' [[e a _] v']
  {:pre [e a v']}
  [e a v'])

(defn stable-eav-a [[e _ _] a']
  {:pre [a']}
  [e a' nil])

(defn stable-eav-av [[e _ _] a' ?v']
  {:pre [a']}                                               ; ?v' can be nil - sparse results
  [e a' ?v'])

(defn ^:export fiddle "Fiddle level ctx adds the result to scope"
  [ctx]
  {:post [(s/assert :hypercrud/context %)
          (contains? % :hypercrud.browser/qfind)            ; but it can be nil for :type :blank
          (:hypercrud.browser/link-index %)]}
  ; Deep inspect the elements to compute the enclosing pull shape for each element
  ; Don't infer any further scopes, it is down-scope's job to infer anything it needs (which might be legacy compat at this point)
  (as-> ctx ctx
        (assoc ctx :hypercrud.browser/qfind (r/fmap-> (:hypercrud.browser/fiddle ctx) hypercrud.browser.context/parse-fiddle-data-shape :qfind))
        (assoc ctx :hypercrud.browser/enclosing-pull-shapes (if @(:hypercrud.browser/qfind ctx)
                                                              (r/apply contrib.datomic/enclosing-pull-shapes
                                                                       ((juxt :hypercrud.browser/schemas
                                                                              :hypercrud.browser/qfind
                                                                              :hypercrud.browser/data) ctx))))
        (assoc ctx :hypercrud.browser/validation-hints (contrib.validation/validate
                                                         (s/get-spec @(r/fmap :fiddle/ident (:hypercrud.browser/fiddle ctx)))
                                                         @(:hypercrud.browser/data ctx)
                                                         (partial hypercrud.browser.context/row-keyfn ctx)))
        (assoc ctx :hypercrud.browser/eav (r/apply hypercrud.browser.context/stable-eav-a
                                                   [(:hypercrud.browser/eav ctx) (r/fmap :fiddle/ident (:hypercrud.browser/fiddle ctx))]))))

(defn -infer-implicit-fiddle [ctx]
  (or
    (if-not (:hypercrud.browser/qfind ctx)
      (hypercrud.browser.context/fiddle ctx))
    ctx))

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

(defn attribute [ctx a]
  {:pre [#_(s/assert r/reactive? (:hypercrud.browser/enclosing-pull-shape ctx)) ; can be inferred
         (s/assert :hypercrud/context ctx)
         (s/assert keyword? a)
         #_(do (println "attribute: " a) true)
         #_(do (println "... data pre: " (pr-str (some-> (:hypercrud.browser/data ctx) deref))) true)]
   :post [(s/assert :hypercrud/context %)
          #_(do (println (:hypercrud.browser/enclosing-pull-shape %)) true)
          #_(do (println "... data post: " (pr-str (some-> (:hypercrud.browser/data %) deref))) true)]}
  (as->
    ctx ctx
    (-infer-implicit-fiddle ctx)
    (-infer-implicit-element ctx)
    (-validate-qfind-element ctx)
    (set-parent ctx)
    (update ctx :hypercrud.browser/path conj a)
    (assoc ctx :hypercrud.browser/enclosing-pull-shape (r/fmap->> (:hypercrud.browser/enclosing-pull-shape ctx)
                                                                  (contrib.datomic/pull-shape-refine a)))
    (let [head-or-body (if (:hypercrud.browser/data ctx) :body :head)]
      (case head-or-body
        :head (assoc ctx :hypercrud.browser/eav (r/fmap-> (:hypercrud.browser/eav ctx) (stable-eav-a a)))
        :body (let [r-data (r/fmap a (:hypercrud.browser/data ctx))
                    ; v may be nil - sparse resultset
                    r-?v (r/fmap->> r-data (smart-entity-identifier ctx))] ; flagged
                (-> (set-parent-data ctx)
                    (update :hypercrud.browser/validation-hints #(for [[[p & ps] hint] % :when (= p a)]
                                                                   [ps hint]))
                    (assoc :hypercrud.browser/data r-data)
                    ; insufficent stability on r-?v, fixme
                    (assoc :hypercrud.browser/eav (r/fmap-> (:hypercrud.browser/eav ctx) (stable-eav-av a @r-?v)))))))))

(defn get' "flipped arg order, for reactive partial"
  ([k o] (get o k))
  ([k o not-found] (get o k not-found)))

(defn stable-tupled-v-extractor [i ?r-data]
  {:pre [i #_(r/reactive? ?r-data)]                         ; It's a tuple (already spread row)
   :post [#_(every? some? @%)]}
  (when ?r-data                                             ; headers
    #_(r/fmap->> ?r-data (mapv (r/partial get' i)))
    (r/fmap-> ?r-data (get i))))

(defn element [ctx & [i]]                                   ; [nil :seattle/neighborhoods 1234345]

  {:pre [#_(s/assert r/reactive? (:hypercrud.browser/qfind ctx)) ; can be inferred
         #_(s/assert nil? (:hypercrud.browser/element ctx))
         #_(do (println "element: " i) true)
         #_(do (println "... data pre: " (pr-str (some-> (:hypercrud.browser/data ctx) deref))) true)]
   :post [(s/assert r/reactive? (:hypercrud.browser/qfind %))
          #_(s/assert :hypercrud/context %)
          #_(do (some->> % :hypercrud.browser/data deref pr-str (println "... data post: ")) true)
          #_(every? some? @(:hypercrud.browser/data %))]}   ; bad in header, good in body
  (as-> ctx ctx
        (-infer-implicit-fiddle ctx)
        (assoc ctx :hypercrud.browser/element (r/fmap-> (:hypercrud.browser/qfind ctx) datascript.parser/find-elements (get (or i 0))))
        (assoc ctx :hypercrud.browser/element-index (or i 0)) ; hack, don't drive tables like this, a breaking change
        (assoc ctx :hypercrud.browser/enclosing-pull-shape (r/fmap-> (:hypercrud.browser/enclosing-pull-shapes ctx)
                                                                     (get i)))
        (let [{data :hypercrud.browser/data} ctx]           ; head vs body
          (if data
            (update ctx :hypercrud.browser/data (condp some [(type @(:hypercrud.browser/qfind ctx))]
                                                  #{FindRel FindTuple} (r/partial stable-tupled-v-extractor i)
                                                  #{FindColl FindScalar} identity))
            ctx))))

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
              (keyword? p) (attribute ctx p)
              :else (assert false (str "illegal focus: " p))))
          ctx relative-path))

(defn row "Toggle :many into :one as we spread through the rows.
  No change in :eav."
  [ctx & [k]]
  {:pre [(s/assert :hypercrud/context ctx)]
   :post [(s/assert :hypercrud/context %)
          #_(do (println (:hypercrud.browser/data ctx)) true)]}
  (-> ctx
      (set-parent)
      (set-parent-data)
      (assoc :hypercrud.browser/row-key k)
      (assoc :hypercrud.browser/data (r/fmap-> (:hypercrud.browser/data-index ctx) (apply [k]))) ; no change in eav
      (update :hypercrud.browser/validation-hints #(for [[[p & ps] hint] % :when (= k p)]
                                                     [ps hint]))))

(declare row-keyfn)
; var first, then can always use db/id on row. No not true – collisions! It is the [?e ?f] product which is unique

; keyfn must support entities & scalars (including aggregates)
; scalars may not be unique
; entities might not be unique either, its the combination that is unique
; lift out of current context in this case?
; Make sure to optimize the single-fe case to use the identity/value which will be unique

(defn parse-fiddle-data-shape [{:keys [fiddle/type fiddle/query fiddle/pull fiddle/pull-database]}]
  (->> (case type
         :blank nil
         :entity (->> (contrib.reader/memoized-read-edn-string+ pull)
                      (fmap (fn [pull]
                              (let [source (symbol pull-database)
                                    fake-q `[:find (~'pull ~source ~'?e ~pull) . :where [~'?e]]]
                                (datascript.parser/parse-query fake-q))))
                      (unwrap (constantly nil)))
         :query (->> (contrib.reader/memoized-read-edn-string+ query)
                     (=<< #(try-either (datascript.parser/parse-query %)))
                     (unwrap (constantly nil))))))

(defprotocol FiddleLinksIndex
  #_(links-at [this criterias])
  #_(links-in-dimension [this element pullpath criterias])
  (links-at-r [this criterias])                             ; criterias can contain nil, meaning toptop
  (links-in-dimension-r [this element pullpath criterias]))

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

(defn index-links "#{criteria} -> [link]
  where criterias is some of #{ident txfn class class2}.
  Links include all links reachable by navigating :ref :one. (Change this by specifying an :ident)
  The index internals are reactive."
  [r-schemas r-fiddle]
  {:pre [#_(not-empty r-schemas)
         (every? #(satisfies? contrib.datomic/SchemaIndexedNormalized %) (vals @r-schemas))]}
  (let [indexed-links-at (r/fmap -indexed-links-at r-fiddle)]
    (reify FiddleLinksIndex
      (links-at-r [this criterias]
        (r/fmap->> indexed-links-at
                   (filter (partial link-criteria-match? criterias))
                   (map second)))                           ; drop the index, just the links

      (links-in-dimension-r [this ?element ?pullpath criterias] ; hidden deref
        (if-not (and ?element @?element ?pullpath)
          (links-at-r this criterias)
          (let [{{db :symbol} :source {pull-pattern :value} :pattern} @?element
                schema (get @r-schemas (str db))]             ; track schema in ctx too
            (->> (when schema
                   (contrib.datomic/reachable-attrs schema (contrib.datomic/pull-shape pull-pattern) ?pullpath))
                 (mapcat (fn [a]
                           (links-at-r this (conj criterias a))))
                 r/sequence)))))))

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
        current-path (drop-while int? (:hypercrud.browser/path ctx))
        schema (get (summon-schemas-grouped-by-dbname ctx) (str db))

        ; find a reachable path that contains the target-attr in the fewest hops
        ;   me->mother ; me->sister->mother ; closest ctx is selected
        ; What if there is more than one?  me->sister->mother; me->father->mother
        ; Ambiguous, how did we even select this link? Probably need full datascript query language.
        target-path (->> (contrib.datomic/reachable-paths schema root-pull current-path)
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
  (let [[e a v] @(:hypercrud.browser/eav ctx)
        target-a @(r/fmap (r/comp hyperfiddle.fiddle/read-path :link/path) link-ref)
        ctx (if (and target-a (not= target-a a))            ; backwards compat
              (refocus ctx target-a)
              ctx)
        +args @(r/fmap->> link-ref (build-args+ ctx))
        [v' & vs] (->> +args (contrib.ct/unwrap (constantly nil))) ; EAV sugar is not interested in tuple case, that txfn is way off happy path
        ctx (assoc ctx :hypercrud.browser/eav (r/track identity [e a v'])
                    #_(r/partial r/fmap (r/partial r/flip stable-eav-v' v')))
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
  (-> (str (:hypercrud.browser/path ctx) "."
           (hash-ctx-data (:hypercrud.browser/parent ctx)) "."
           (hash-ctx-data ctx))
      hash str))
