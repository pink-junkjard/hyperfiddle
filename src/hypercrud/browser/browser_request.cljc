(ns hypercrud.browser.browser-request
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either #?(:clj :refer :cljs :refer-macros) [try-either]]
            [datascript.parser :as parser]
            [hypercrud.browser.anchor :as link]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.schema :as schema-util]
            [taoensso.timbre :as timbre]))


(declare request-from-route)
(declare request-from-anchor)

(defn recurse-request [link ctx]
  (if (:link/managed? link)
    (let [route' (routing/build-route' link ctx)
          ctx (context/anchor-branch ctx link)]
      (if true #_(get-in (-> ctx :peer .-state-atom deref) [:popovers (:branch ctx)])
        ; if the anchor IS a popover, we need to run the same logic as anchor/build-anchor-props
        ; the ctx needs to be updated (branched, etc), but NOT BEFORE determining the route
        ; that MUST happen in the parent context
        (let [ctx (-> ctx
                      (context/clean)
                      (update :debug #(str % ">popover-link[" (:db/id link) ":" (or (:link/rel link) (:anchor/prompt link)) "]")))]
          (either/branch route'
                         (constantly nil)
                         #(request-from-route % ctx)))))
    ; if the anchor IS NOT a popover, this should be the same logic as widget/render-inline-anchors
    (let [ctx (update ctx :debug #(str % ">inline-link[" (:db/id link) ":" (or (:link/rel link) (:anchor/prompt link)) "]"))]
      (request-from-anchor link ctx))))

(defn cell-dependent-requests [cell fe links ctx]
  (let [ctx (-> ctx
                (context/find-element fe (:fe-pos ctx))
                (context/cell-data cell))]
    (concat
      (->> (link/links-lookup links [(:fe-pos ctx)])
           (filter :link/dependent?)
           (mapcat #(recurse-request % ctx)))
      (->> (:fields fe)
           (mapcat (fn [field]
                     (let [ctx (-> (context/attribute ctx (:attribute field))
                                   (context/value ((:cell-data->value field) (:cell-data ctx))))]
                       (->> (link/links-lookup links [(:fe-pos ctx) (-> ctx :attribute :db/ident)])
                            (filter :link/dependent?)
                            (mapcat #(recurse-request % ctx))))))))))

(defn relation-dependent-requests [relation ordered-fes links ctx]
  (->> (map vector relation ordered-fes)
       (map-indexed (fn [fe-pos [cell fe]]
                      (cell-dependent-requests cell fe links (assoc ctx :fe-pos fe-pos))))
       (apply concat)))

(defn fiddle-dependent-requests [result ordered-fes links ctx]
  ; reconcile this with the anchor.cljs logic
  (let [links (filter :link/render-inline? links)]      ; at this point we only care about inline links
    (concat
      (->> (mapcat #(recurse-request % ctx) (->> (link/links-lookup links [])
                                                 (remove :link/dependent?))))
      (->> ordered-fes                                      ; might have empty results
           (map-indexed (fn [fe-pos fe]
                          (let [ctx (context/find-element ctx fe fe-pos)]
                            (concat
                              (->> (link/links-lookup links [fe-pos])
                                   (remove :link/dependent?)
                                   (mapcat #(recurse-request % ctx)))
                              (->> (:fields fe)
                                   (mapcat (fn [{:keys [attribute]}]
                                             (let [ctx (context/attribute ctx attribute)]
                                               (->> (link/links-lookup links [fe-pos (-> ctx :attribute :db/ident)])
                                                    (remove :link/dependent?)
                                                    (mapcat #(recurse-request % ctx)))))))))))
           (apply concat))
      (case (get-in ctx [:fiddle :fiddle/type])
        :entity (if-let [a (get-in ctx [:request :a])]
                  (either/branch
                    (try-either (.-dbname (get-in ctx [:route :request-params :entity])))
                    (constantly nil)
                    (fn [source-symbol]
                      (case (get-in ctx [:schemas (str source-symbol) a :db/cardinality :db/ident])
                        :db.cardinality/one
                        (cell-dependent-requests result (first ordered-fes) links (assoc ctx :fe-pos 0))

                        :db.cardinality/many
                        (mapcat #(cell-dependent-requests % (first ordered-fes) links (assoc ctx :fe-pos 0)) result))))
                  (cell-dependent-requests result (first ordered-fes) links (assoc ctx :fe-pos 0)))

        :query (either/branch
                 (try-either (parser/parse-query (get-in ctx [:request :query])))
                 (constantly nil)
                 (fn [{:keys [qfind]}]
                   (condp = (type qfind)
                     datascript.parser.FindRel
                     (mapcat #(relation-dependent-requests % ordered-fes links ctx) result)

                     datascript.parser.FindColl
                     (mapcat #(cell-dependent-requests % (first ordered-fes) links (assoc ctx :fe-pos 0)) result)

                     datascript.parser.FindTuple
                     (relation-dependent-requests result ordered-fes links ctx)

                     datascript.parser.FindScalar
                     (cell-dependent-requests result (first ordered-fes) links (assoc ctx :fe-pos 0)))))

        :blank nil

        nil))))

(defn f-mode-config []
  {:from-ctx :user-request
   :from-link :fiddle/request
   :with-user-fn (fn [user-fn]
                   (fn [result ordered-fes links ctx]
                     ; todo report invocation errors back to the user
                     (try (->> (user-fn result ordered-fes links ctx)
                               ; user-fn HAS to return a seqable value, we want to throw right here if it doesn't
                               seq)
                          (catch :default e
                            (timbre/error e)
                            nil))))
   :default fiddle-dependent-requests})

(defn process-data [{:keys [result ordered-fes anchors ctx]}]
  (mlet [request-fn (base/fn-from-mode (f-mode-config) (:fiddle ctx) ctx)]
    (cats/return (request-fn result ordered-fes anchors ctx))))

(defn request-from-route [route ctx]
  (let [ctx (context/route ctx route)
        {:keys [meta-fiddle-req' fiddle']} (base/hydrate-fiddle ctx)]
    (concat (if-let [meta-fiddle-req (-> meta-fiddle-req'
                                         (cats/mplus (either/right nil))
                                         (cats/extract))]
              [meta-fiddle-req])
            (-> (mlet [fiddle fiddle'
                       fiddle-request (base/request-for-fiddle fiddle ctx)]
                  (cats/return
                    (concat
                      (if fiddle-request [fiddle-request])
                      (schema-util/schema-requests-for-link ctx)
                      (-> (base/process-results fiddle fiddle-request ctx)
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
