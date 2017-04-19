(ns hypercrud.browser.links
  (:require [cljs.reader :as reader]
            [clojure.set :as set]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx]
            [hypercrud.compile.eval :refer [eval-str]]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.util :as util]
            [promesa.core :as p]
            [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.browser.link-util :as link-util]
            [reagent.core :as r]))


(defn auto-formula [anchor]                                 ; what about long-coersion?
  ; Future improvement:
  ; we already have this info in the runtime param-ctx, so we could delay until formula runtime
  ; and not look at the anchor at all and bypass the formula read-eval.

  ; this is a 3x3 matrix - repeating, entity, attribute. attribute depends on entity
  (let [{r :anchor/repeating? e :anchor/find-element a :anchor/attribute} anchor]
    (cond

      ; attr edit
      (and r true #_e a)                                    ; legacy links may not set entity here
      (pr-str {:entity `(fn [ctx#]
                          (if (= :db.cardinality/many (-> (get (:schema ctx#) (-> ctx# :attribute :attribute/ident)) :attribute/cardinality :db/ident))
                            (mapv :db/id (get ctx# :value))
                            (get-in ctx# [:value :db/id])))})

      ; attr create (managed, see auto-txfn)
      (and (not r) e a)
      (pr-str {:entity `(fn [ctx#]                          ; create ignores cardinality
                          (hc/*temp-id!* ~(-> e :find-element/connection :db/id :id)))})

      ; entity edit
      (and r e (not a))
      (pr-str {:entity `(fn [ctx#]                          ; find-elements don't have cardinality
                          (get-in ctx# [:entity :db/id]))})

      ; entity create
      (and (not r) e (not a))
      (pr-str {:entity `(fn [ctx#]
                          (hc/*temp-id!* ~(-> e :find-element/connection :db/id :id)))})

      ; naked
      (and (not r) (not e) (not a)) nil

      ; relation edit (this is not really a thing)
      ; If this is a thing, it probably is a query with named params and a custom formula.
      (and r (not e) (not a)) nil

      :else (assert false (str "auto-formula matrix - missing pattern: " (util/pprint-str [r e a]))))))

(declare build-link-props)

(defn auto-txfn [anchor]
  (let [{r :anchor/repeating? e :anchor/find-element a :anchor/attribute} anchor]
    (cond

      ; legacy links don't have e
      (and (not r) true #_e a)                              ; attr create
      (let [anchor (tx/walk-pulled-tree nil #(dissoc % :db/id) anchor)] ; hacks because we can't read #DbId at "compile time" which is dumb
        (pr-str `(fn [ctx# show-popover!#]
                   (let [parent# (:entity ctx#)
                         new-dbid# (hc/*temp-id!* (-> parent# :db/id :conn-id))]
                     (-> (show-popover!#)
                         (p/then (fn [tx-from-modal#]
                                   {:tx
                                    (concat
                                      (let [attr-ident# (-> ctx# :attribute :attribute/ident)
                                            rets# (some-> parent# attr-ident# :db/id vector)
                                            adds# [new-dbid#]]
                                        (tx/edit-entity (:db/id parent#) attr-ident# rets# adds#))
                                      tx-from-modal#)})))))))
      :else nil)))

(defn build-url-params-map!
  ([domain project link-dbid formula-str param-ctx]
   {:domain domain
    :project project
    :link-dbid link-dbid #_:id
    :query-params (try                                      ; todo return monad
                    (->> (q-util/read-eval-formulas formula-str)
                         (util/map-values #(q-util/run-formula! % param-ctx)))
                    (catch js/Error e {}))})
  ([link formula-str param-ctx]
   (build-url-params-map!
     (-> link :hypercrud/owner :database/domain)
     (-> link :hypercrud/owner :database/ident)
     (-> link :db/id)
     formula-str
     param-ctx))
  ([anchor param-ctx]
   (let [formula-str (:anchor/formula anchor)
         formula-str (if (empty? formula-str)
                       (auto-formula anchor)
                       formula-str)]
     (build-url-params-map! (:anchor/link anchor) formula-str param-ctx)))
  #_(case (link-type (:anchor/link anchor))
      :link-query {:link-dbid (-> anchor :anchor/link :db/id)
                   :query-params (->> (q-util/read-eval-formulas (:anchor/formula anchor))
                                      (util/map-values #(q-util/run-formula! % param-ctx)))}
      :link-entity {:link-dbid (-> anchor :anchor/link :db/id)
                    :query-params (->> (q-util/read-eval-formulas (:anchor/formula anchor))
                                       (util/map-values #(q-util/run-formula! % param-ctx)))
                    #_(let [find-element-name nil
                            attr (-> anchor :anchor/attribute :attribute/ident)]
                        (get-in param-ctx [:result find-element-name attr :db/id]))}))


(defn holes-filled? [hole-names query-params-map]
  (set/subset? (set hole-names) (set (keys (into {} (remove (comp nil? val) query-params-map))))))

(defn anchor-valid? [link url-params]                       ; could return monad to say why
  ; We specifically hydrate this deep just so we can validate anchors like this.
  (case (link-util/link-type link)
    :link-query (some-> link :link/request :link-query/value
                        reader/read-string q-util/parse-param-holes
                        (holes-filled? (:query-params url-params)))
    :link-entity (not= nil (-> url-params :query-params :entity))
    true #_"no query, probably, like hyperfiddle admin"))

(defn anchor-tooltip [link url-params param-ctx]
  (case (:display-mode param-ctx)
    :xray (if (anchor-valid? link url-params)
            [nil (pr-str (:query-params url-params))]
            [:warning (pr-str (:query-params url-params))])
    nil))

(defn build-link-props-raw [route link param-ctx]           ; param-ctx is for display-mode
  ; doesn't handle tx-fn - meant for the self-link. Weird and prob bad.
  {:route route
   :style {:color (connection-color/connection-color (-> link :hypercrud/owner :db/id :id))}
   :tooltip (anchor-tooltip link route param-ctx)
   :class (if-not (anchor-valid? link route) "invalid")})



; if this is driven by anchor, and not route, it needs memoized.
; the route is a fn of the formulas and the formulas can have effects
; which have to be run only once.
(defn build-link-props [anchor param-ctx]
  ; auto-tx-fn in addition to auto-formula
  (let [param-ctx (assoc param-ctx :link-owner (-> anchor :anchor/link :hypercrud/owner)) ; tx-fn may need this
        tx-fn-str (let [tx-fn-str (:anchor/tx-fn anchor)]
                    (if (empty? tx-fn-str)
                      (auto-txfn anchor)
                      tx-fn-str))
        tx-fn (if tx-fn-str
                (let [{value :value error :error} (eval-str tx-fn-str)]
                  ;; non-fatal error, report it here so user can fix it
                  (if error (js/alert (str "cljs eval error: " error))) ; return monad so tooltip can draw the error
                  value))
        route (if (:anchor/link anchor) (build-url-params-map! anchor param-ctx)) #_"return monad so tooltip can draw the error?"]
    (doall
      (merge
        (if tx-fn
          ; do we need to hydrate any dependencies in this chain?
          {:txfn #(let [result (tx-fn param-ctx %)]         ; tx-fn may be sync or async
                    (-> (if-not (p/promise? result) (p/resolved result) result)
                        (p/branch (:user-swap! param-ctx)
                                  (fn cancelled [why] nil))))})

        (if (and tx-fn (:anchor/link anchor))               ; default case is a syslink
          ; this is a special case where due to the txfn the embed goes in a popover
          {:popover (fn [state]
                      ; assumes everything is hydrated
                      [hypercrud.browser.core/safe-ui       ; cycle
                       route
                       (assoc param-ctx :user-swap!
                                        (fn [{:keys [tx route]}]
                                          (assert (not route) "popups not allowed to route")
                                          (swap! state tx/into-tx tx)))])})

        (if route (build-link-props-raw route (:anchor/link anchor) param-ctx))))))

(defn link-visible? [anchor param-ctx]
  (let [visible-src (:anchor/visible? anchor)
        visible-fn (if-not (empty? visible-src)
                     (let [{:keys [value error]} (eval-str visible-src)]
                       (if error (js/alert (str "cljs eval error: " error)))
                       value)
                     (constantly true))]
    (visible-fn param-ctx)))
