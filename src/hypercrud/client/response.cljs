(ns hypercrud.client.response
  (:require [cats.monad.exception :as exception]
            [hypercrud.client.core :as hc]
            [hypercrud.types :as types :refer [->DbVal]]
            [cljs.reader :as reader]))

(defn human-error [e req]
  (let [unfilled-holes (->> (filter (comp nil? val) (.-params req)) (map key))]
    (if-not (empty? unfilled-holes)
      [:div "Query "
       [:pre (pr-str (.-query req))]
       "has unfilled holes"
       [:pre (pr-str unfilled-holes)]
       "datomic reported"
       [:pre (.-msg e)]]
      (.-msg e))))

(deftype Response [requests pulled-trees-map stage-val]         ; can this be called cache?
  hc/Response
  (hydrate [this request]
    ; (exception/try-or-recover  (constantly (exception/failure (str unfilled-holes))))
    (if (contains? pulled-trees-map request)
      (let [resultset-or-error (get pulled-trees-map request)]
        (if (instance? types/DbError resultset-or-error)
          (exception/failure (human-error resultset-or-error request))
          (exception/success resultset-or-error)))
      (do
        (let [error (js/Error. (str "Unhydrated request:\n" (pr-str request)))]
          #_(js/console.log error)                          ; happens a lot during query fns - would need to silence this log during query phase.
          (exception/failure error)))))

  (db [this conn-id branch]
    (->DbVal conn-id branch))

  (tx [this db]
    (get-in stage-val [(.-conn-id db) (.-branch db)] []))

  IHash
  (-hash [this]
    ; requests have a dbval which has a hash of the stage
    (hash requests))

  IEquiv
  (-equiv [this other]
    (= (hash this) (hash other)))

  ; This looks like edn but is not edn - its really just for debugging.
  Object (toString [this] (str "#Response" (pr-str [(count requests) (hash this)])))
  IPrintWithWriter (-pr-writer [o writer _] (-write writer (.toString o))))


(deftype ResponseTransitHandler []
  Object
  (tag [this v] "Response")

  ; is it true that the requests are just (set (keys pulled-trees-map)) ?
  (rep [this v] [(.-requests v) (.-pulled-trees-map v) (.-stage v)])
  (stringRep [this v] nil))

(def read-Response #(apply ->Response %))
(reader/register-tag-parser! 'Response read-Response)
