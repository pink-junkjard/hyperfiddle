(ns hypercrud.browser.browser-request
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either :refer-macros [try-either]]
            [datascript.parser :as parser]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.schema :as schema-util]))


(declare request-from-route)
(declare request-from-anchor)

(defn recurse-request [anchor ctx]
  (if (:anchor/managed? anchor)
    (let [route' (routing/build-route' anchor ctx)
          ctx (context/anchor-branch ctx anchor)]
      (if true #_(get-in (-> ctx :peer .-state-atom deref) [:popovers (:branch ctx)])
        ; if the anchor IS a popover, we need to run the same logic as anchor/build-anchor-props
        ; the ctx needs to be updated (branched, etc), but NOT BEFORE determining the route
        ; that MUST happen in the parent context
        (let [ctx (-> ctx
                      (context/clean)
                      (update :debug #(str % ">popover-link[" (:db/id anchor) ":" (or (:anchor/ident anchor) (:anchor/prompt anchor)) "]")))]
          (either/branch route'
                         (constantly nil)
                         #(request-from-route % ctx)))))
    ; if the anchor IS NOT a popover, this should be the same logic as widget/render-inline-anchors
    (let [ctx (update ctx :debug #(str % ">inline-link[" (:db/id anchor) ":" (or (:anchor/ident anchor) (:anchor/prompt anchor)) "]"))]
      (request-from-anchor anchor ctx))))

(defn cell-dependent-requests [cell fe fe-anchors-lookup ctx]
  (let [ctx (-> ctx
                (context/find-element fe)
                (context/cell-data cell))]
    (concat
      (->> (get fe-anchors-lookup :links)
           (filter :anchor/repeating?)
           (mapcat #(recurse-request % ctx)))
      (->> (:fields fe)
           (mapcat (fn [field]
                     (let [ctx (-> (context/attribute ctx (:attribute field))
                                   (context/value ((:cell-data->value field) (:cell-data ctx))))]
                       (->> (get-in fe-anchors-lookup [(:attribute field) :links])
                            (filter :anchor/repeating?)
                            (mapcat #(recurse-request % ctx))))))))))

(defn relation-dependent-requests [relation ordered-fes anchors-lookup ctx]
  (->> (map vector relation ordered-fes)
       (map-indexed (fn [pos [cell fe]]
                      (cell-dependent-requests cell fe (get anchors-lookup pos) ctx)))
       (apply concat)))

(defn link-dependent-requests [result ordered-fes anchors ctx]
  (let [anchors-lookup (->> anchors
                            (filter :anchor/render-inline?) ; at this point we only care about inline anchors
                            (base/build-pathed-anchors-lookup))]
    (concat
      (->> (mapcat #(recurse-request % ctx) (->> (get anchors-lookup :links)
                                                 (remove :anchor/repeating?))))
      (->> ordered-fes                                      ; might have empty results
           (map-indexed (fn [fe-pos fe]
                          (let [ctx (context/find-element ctx fe)
                                fe-anchors-lookup (get anchors-lookup fe-pos)]
                            (concat
                              (->> (get fe-anchors-lookup :links)
                                   (remove :anchor/repeating?)
                                   (mapcat #(recurse-request % ctx)))
                              (->> (:fields fe)
                                   (mapcat (fn [{:keys [attribute]}]
                                             (let [ctx (context/attribute ctx attribute)]
                                               (->> (get-in fe-anchors-lookup [attribute :links])
                                                    (remove :anchor/repeating?)
                                                    (mapcat #(recurse-request % ctx)))))))))))
           (apply concat))
      (case (get-in ctx [:fiddle :request/type])
        :entity (if-let [a (get-in ctx [:request :a])]
                  (either/branch
                    (try-either (.-dbname (get-in ctx [:route :request-params :entity])))
                    (constantly nil)
                    (fn [source-symbol]
                      (case (get-in ctx [:schemas (str source-symbol) a :db/cardinality :db/ident])
                        :db.cardinality/one
                        (cell-dependent-requests result (first ordered-fes) (get anchors-lookup 0) ctx)

                        :db.cardinality/many
                        (mapcat #(cell-dependent-requests % (first ordered-fes) (get anchors-lookup 0) ctx) result))))
                  (cell-dependent-requests result (first ordered-fes) (get anchors-lookup 0) ctx))

        :query (either/branch
                 (try-either (parser/parse-query (get-in ctx [:request :query])))
                 (constantly nil)
                 (fn [{:keys [qfind]}]
                   (condp = (type qfind)
                     datascript.parser.FindRel
                     (mapcat #(relation-dependent-requests % ordered-fes anchors-lookup ctx) result)

                     datascript.parser.FindColl
                     (mapcat #(cell-dependent-requests % (first ordered-fes) (get anchors-lookup 0) ctx) result)

                     datascript.parser.FindTuple
                     (relation-dependent-requests result ordered-fes anchors-lookup ctx)

                     datascript.parser.FindScalar
                     (cell-dependent-requests result (first ordered-fes) (get anchors-lookup 0) ctx))))

        :blank nil

        nil))))

(defn f-mode-config []
  {:from-ctx :user-request
   :from-link :fiddle/request
   :with-user-fn (fn [user-fn]
                   (fn [result ordered-fes anchors ctx]
                     ; todo report invocation errors back to the user
                     (try (->> (user-fn result ordered-fes anchors ctx)
                               ; user-fn HAS to return a seqable value, we want to throw right here if it doesn't
                               seq)
                          (catch :default e
                            (js/console.error (pr-str e))
                            nil))))
   :default link-dependent-requests})

(defn process-data [{:keys [result ordered-fes anchors ctx]}]
  (mlet [request-fn (base/fn-from-mode (f-mode-config) (:fiddle ctx) ctx)]
    (cats/return (request-fn result ordered-fes anchors ctx))))

(defn request-from-route [route ctx]
  (let [ctx (context/route ctx route)
        {:keys [meta-link-req' link']} (base/hydrate-link ctx)]
    (concat (if-let [meta-link-req (-> meta-link-req'
                                       (cats/mplus (either/right nil))
                                       (cats/extract))]
              [meta-link-req])
            (-> (mlet [link link'
                       link-request (base/request-for-link link ctx)]
                  (cats/return
                    (concat
                      (if link-request [link-request])
                      (schema-util/schema-requests-for-link ctx)
                      (-> (base/process-results link link-request ctx)
                          (cats/bind process-data)
                          (cats/mplus (either/right nil))
                          (cats/extract)))))
                (cats/mplus (either/right nil))
                (cats/extract)))))

(defn request-from-anchor [anchor ctx]
  (-> (base/from-anchor anchor ctx (fn [route ctx]
                                     (either/right (request-from-route route ctx))))
      (cats/mplus (either/right nil))
      (cats/extract)))
