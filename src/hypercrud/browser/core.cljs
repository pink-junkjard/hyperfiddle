(ns hypercrud.browser.core
  (:require [cljs.pprint :as pprint]
            [clojure.string :as string]
            [hypercrud.browser.base-64-url-safe :as base64]
            [hypercrud.browser.pages.entity :as entity]
            [hypercrud.browser.pages.field :as field]
            [hypercrud.browser.pages.index :as index]
            [hypercrud.browser.pages.query :as query]))


(defn ui [cur transact! graph forms queries page-rel-path navigate!]
  [:div
   [:div.hc-node-view
    (let [path-params (string/split page-rel-path "/")]
      (cond
        (and (= (second path-params) "query") (= 3 (count path-params)))
        (let [[form-id _ q] path-params]
          (query/ui cur transact! graph forms (js/parseInt form-id 10)))

        (and (= (second path-params) "entity"))
        (condp = (count path-params)
          3 (let [[form-id _ eid] path-params
                  form-id (js/parseInt form-id 10)
                  eid (js/parseInt eid 10)]
              (entity/ui cur transact! graph eid forms form-id navigate!))
          4 (let [[form-id _ eid field-ident] path-params
                  form-id (js/parseInt form-id 10)
                  eid (js/parseInt eid 10)
                  field-ident (keyword (subs (base64/decode field-ident) 1))
                  form (get forms form-id)
                  field (first (filter #(= (:ident %) field-ident) form))]
              (field/ui field cur transact! graph eid forms)))

        (and (= (first path-params) "") (= 1 (count path-params)))
        (index/ui queries)
        :else [:div "no route for: " page-rel-path]
        ))]
   [:hr]
   [:pre (with-out-str (pprint/pprint @cur))]])


(defn query [forms state page-rel-path]
  (let [path-params (string/split page-rel-path "/")]
    (cond
      (and (= (second path-params) "query") (= 3 (count path-params)))
      (let [[form-id _ q] path-params]
        (query/query state (base64/decode q) forms (js/parseInt form-id 10)))

      (and (= (second path-params) "entity"))
      (condp = (count path-params)
        3 (let [[form-id _ eid] path-params
                form-id (js/parseInt form-id 10)
                eid (js/parseInt eid 10)]
            (entity/query state eid forms form-id))
        4 (let [[form-id _ eid field-ident] path-params
                form-id (js/parseInt form-id 10)
                eid (js/parseInt eid 10)
                field-ident (keyword (subs (base64/decode field-ident) 1))]
            (field/query state eid forms form-id)))

      (and (= (first path-params) "") (= 1 (count path-params)))
      (index/query)

      :else {}
      )))
