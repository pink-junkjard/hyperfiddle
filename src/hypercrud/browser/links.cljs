(ns hypercrud.browser.links
  (:require [cljs.reader :as reader]
            [clojure.set :as set]
            [hypercrud.client.core :as hc]
            [hypercrud.compile.eval :refer [eval-str]]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.util :as util]
            [promesa.core :as p]
            [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.browser.link-util :as link-util]))


(defn auto-formula [anchor]                                 ; what about long-coersion?
  ; Future improvement:
  ; we already have this info in the runtime param-ctx, so we could delay until formula runtime
  ; and not look at the anchor at all and bypass the formula read-eval.

  ; this is a 3x3 matrix - repeating, entity, attribute. attribute depends on entity
  (let [{r :anchor/repeating? e :anchor/find-element a :anchor/attribute} anchor]
    (cond

      ; attr edit
      (and r true #_e a)                                    ; legacy links may not set entity here
      (pr-str {:entity `(fn [~'ctx]
                          ; i am seeing cljs compiler bugs here? introducing let forms breaks it.
                          (if (= :db.cardinality/many (-> (get (:schema ~'ctx) (-> ~'ctx :attribute :attribute/ident)) :attribute/cardinality :db/ident))
                            (mapv :db/id (get ~'ctx :value))
                            (get-in ~'ctx [:value :db/id])))})

      ; entity edit
      (and r e (not a))
      (pr-str {:entity `(fn [~'ctx]
                          ; find-elements don't have cardinality
                          (get-in ~'ctx [:entity :db/id]))})

      ; attr create
      (and (not r) e a) nil

      ; entity create
      (and (not r) e (not a))
      (pr-str {:entity `(fn [~'ctx]
                          (hc/*temp-id!* ~(-> e :find-element/connection :db/id :id)))})

      ; naked
      (and (not r) (not e) (not a)) nil

      ; relation edit (this is not really a thing)
      ; If this is a thing, it probably is a query with named params and a custom formula.
      (and r (not e) (not a)) nil

      :else (assert false (str "auto-formula matrix - missing pattern: " (util/pprint-str [r e a]))))))

(defn build-url-params-map
  ([domain project link-dbid formula param-ctx]
   {:domain domain
    :project project
    :link-dbid link-dbid #_:id
    :query-params (try                                      ; todo return monad
                    (->> (q-util/read-eval-formulas formula)
                         (util/map-values #(q-util/run-formula % param-ctx)))
                    (catch js/Error e {}))})
  ([link formula param-ctx]
   (build-url-params-map
     (-> link :hypercrud/owner :database/domain)
     (-> link :hypercrud/owner :database/ident)
     (-> link :db/id)
     formula
     param-ctx))
  ([anchor param-ctx]
   (let [formula (:anchor/formula anchor)
         formula (if (empty? formula)
                   (auto-formula anchor)
                   formula)
         ]
     (build-url-params-map (:anchor/link anchor) formula param-ctx)))
  #_(case (link-type (:anchor/link anchor))
      :link-query {:link-dbid (-> anchor :anchor/link :db/id)
                   :query-params (->> (q-util/read-eval-formulas (:anchor/formula anchor))
                                      (util/map-values #(q-util/run-formula % param-ctx)))}
      :link-entity {:link-dbid (-> anchor :anchor/link :db/id)
                    :query-params (->> (q-util/read-eval-formulas (:anchor/formula anchor))
                                       (util/map-values #(q-util/run-formula % param-ctx)))
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

(defn build-link-props [anchor param-ctx]
  (let [param-ctx (assoc param-ctx :link-owner (-> anchor :anchor/link :hypercrud/owner)) ; tx-fn may need this
        tx-fn (if-let [tx-fn (:anchor/tx-fn anchor)]
                (let [{value :value error :error} (eval-str tx-fn)]
                  ;; non-fatal error, report it here so user can fix it
                  (if error (js/alert (str "cljs eval error: " error))) ; return monad so tooltip can draw the error
                  value))]
    (if tx-fn
      {:on-click #(let [result (tx-fn param-ctx)]           ; tx-fn may be sync or async
                    (-> (if-not (p/promise? result) (p/resolved result) result)
                        (p/then (:user-swap! param-ctx))))}
      (let [route (build-url-params-map anchor param-ctx) #_"return monad so tooltip can draw the error?"]
        (build-link-props-raw route (:anchor/link anchor) param-ctx)))))

(defn link-visible? [anchor param-ctx]
  (let [visible-src (:anchor/visible? anchor)
        visible-fn (if-not (empty? visible-src)
                     (let [{:keys [value error]} (eval-str visible-src)]
                       (if error (js/alert (str "cljs eval error: " error)))
                       value)
                     (constantly true))]
    (visible-fn param-ctx)))
