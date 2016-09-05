(ns hypercrud.browser.core
  (:require [cljs.pprint :as pprint]
            [clojure.string :as string]
            [hypercrud.browser.base-64-url-safe :as base64]
            [hypercrud.browser.pages.entity :as entity]
            [hypercrud.browser.pages.index :as index]
            [hypercrud.browser.pages.query :as query]))


(defn ui [cur transact! graph forms query->form queries page-rel-path navigate!]
  [:div
   [:div.hc-node-view
    (let [path-params (string/split page-rel-path "/")]
      (cond
        (and (= (second path-params) "query") (= 3 (count path-params)))
        (let [[form-name _ q] path-params]
          (query/ui graph forms (keyword form-name)))

        (and (= (second path-params) "entity") (= 3 (count path-params)))
        (let [[form-name _ eid] path-params]
          (entity/ui cur transact! graph (keyword form-name) forms (js/parseInt eid 10) navigate!))

        (and (= (first path-params) "") (= 1 (count path-params)))
        (index/ui query->form queries)
        :else [:div "no route for: " page-rel-path]
        ))]
   [:hr]
   [:pre (with-out-str (pprint/pprint @cur))]])


(defn query [forms state page-rel-path]
  (let [path-params (string/split page-rel-path "/")]
    (cond
      (and (= (second path-params) "query") (= 3 (count path-params)))
      (let [[form-name _ q] path-params]
        (query/query ((keyword form-name) forms) (base64/decode q)))

      (and (= (second path-params) "entity") (= 3 (count path-params)))
      (let [[form-name _ eid] path-params]
        (entity/query (js/parseInt eid 10) state (keyword form-name) forms))

      (and (= (first path-params) "") (= 1 (count path-params)))
      (index/query)

      :else {}
      )))
