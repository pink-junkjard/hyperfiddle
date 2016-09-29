(ns hypercrud.browser.core
  (:require [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [clojure.string :as string]
            [hypercrud.browser.base-64-url-safe :as base64]
            [hypercrud.browser.pages.entity :as entity]
            [hypercrud.browser.pages.field :as field]
            [hypercrud.browser.pages.index :as index]
            [hypercrud.browser.pages.query :as query]))


(defn ui [cur transact! graph forms queries page-rel-path navigate! navigate-cmp]
  [:div
   [:div.hc-node-view
    (let [path-params (string/split page-rel-path "/")]
      (cond
        (and (= (second path-params) "query") (= 3 (count path-params)))
        (let [[form-id _ query-blob] path-params
              form-id (js/parseInt form-id 10)
              query-blob (reader/read-string (base64/decode query-blob))]
          (query/ui cur transact! graph forms queries form-id query-blob navigate-cmp))

        (and (= (second path-params) "entity"))
        (condp = (count path-params)
          3 (let [[form-id _ eid] path-params
                  form-id (js/parseInt form-id 10)
                  eid (js/parseInt eid 10)]
              (entity/ui cur transact! graph eid forms queries form-id navigate! navigate-cmp))
          4 (let [[form-id _ eid field-ident] path-params
                  form-id (js/parseInt form-id 10)
                  eid (js/parseInt eid 10)
                  field-ident (keyword (subs (base64/decode field-ident) 1))]
              (field/ui cur transact! graph eid forms queries form-id field-ident navigate-cmp)))

        (and (= (first path-params) "") (= 1 (count path-params)))
        (index/ui queries navigate-cmp)
        :else [:div "no route for: " page-rel-path]
        ))]
   [:hr]
   [:pre (with-out-str (pprint/pprint @cur))]])


(defn query [forms queries state page-rel-path param-ctx]
  (let [path-params (string/split page-rel-path "/")]
    (cond
      (and (= (second path-params) "query") (= 3 (count path-params)))
      (let [[form-id _ query-blob] path-params
            form-id (js/parseInt form-id 10)
            query-blob (reader/read-string (base64/decode query-blob))]
        (query/query state forms queries query-blob form-id param-ctx))

      (and (= (second path-params) "entity"))
      (condp = (count path-params)
        3 (let [[form-id _ eid] path-params
                form-id (js/parseInt form-id 10)
                eid (js/parseInt eid 10)]
            (entity/query state eid forms queries form-id param-ctx))
        4 (let [[form-id _ eid field-ident] path-params
                form-id (js/parseInt form-id 10)
                eid (js/parseInt eid 10)
                field-ident (keyword (subs (base64/decode field-ident) 1))]
            (field/query state eid forms queries form-id field-ident param-ctx)))

      (and (= (first path-params) "") (= 1 (count path-params)))
      (index/query)

      :else {}
      )))
