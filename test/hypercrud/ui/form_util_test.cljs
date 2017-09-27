(ns hypercrud.ui.form-util-test
  (:require-macros [cljs.test :refer [deftest is]])
  (:require [cljs.test]
            [hypercrud.types.DbId :refer [->DbId]]
            [hypercrud.ui.form-util :refer [determine-colspec]]))


;(def foo #DbId [1 2])

(def fe1 [{:find-element/name :entity,
           :find-element/form {:db/id (->DbId 17592186045552 17592186045422),
                               :hypercrud/owner {:db/id (->DbId 17592186045423 17592186045422)},
                               :form/field [{:db/id (->DbId 17592186045553 17592186045422),
                                             :field/attribute {:db/id (->DbId 17592186045451 17592186045422),
                                                               :attribute/ident :link/renderer,
                                                               :attribute/valueType {:db/id (->DbId 23 17592186045422), :db/ident :db.type/string},
                                                               :attribute/cardinality {:db/id (->DbId 35 17592186045422), :db/ident :db.cardinality/one}},
                                             :field/prompt "Renderer",
                                             :field/order 0}]}}])

(def resultset1
  [{:entity {:db/id (->DbId -16 17592186045422),
             :link/renderer "(fn [resultset link param-ctx]\n  [:div\n   [:ul\n    (map (fn [result]\n           (let [param-ctx (assoc param-ctx :result result)]\n             [:li {:key (hash result)}\n              (let [post (get-in result [\"?blog\"])\n                    iso-date-str #(some-> % .toISOString (.slice 0 10))]\n                [:span\n                 (iso-date-str (:post/date post))\n                 \" \"\n                 ((:link-fn param-ctx) :edit (:post/title post) param-ctx)])]))\n         resultset)]])",
             :hypercrud/owner {:db/id (->DbId -20 17592186045422)},
             :link/name "index"}}])

(def fe2
  [{:db/id (->DbId 17592186045923 17592186045422),
    :find-element/name "?e",
    :find-element/connection
    {:db/id (->DbId 17592186045910 17592186045422),
     :domain/ident "seattle"}}])
(def resultset2
  [{"?e" {:db/id (->DbId 17592186045443 17592186045910),
          :district/name "Southwest",
          :district/region {:db/id (->DbId 17592186045435 17592186045910)}}}
   {"?e" {:db/id (->DbId 17592186045508 17592186045910),
          :district/name "Delridge",
          :district/region {:db/id (->DbId 17592186045435 17592186045910)}}}
   {"?e" {:db/id (->DbId 17592186045542 17592186045910),
          :district/name "Magnolia/Queen Anne",
          :district/region {:db/id (->DbId 17592186045436 17592186045910)}}}])


(deftest test-determine-colspec []
  ; there's a form but strip it for raw mode
  (is (= (determine-colspec resultset2 fe2 {:display-mode :root})
         ["?e" :db/id nil "?e" :district/name nil "?e" :district/region nil]))

  ; no form hydrated
  (is (= (determine-colspec resultset2 fe2 {:display-mode :xray})
         ["?e" :db/id nil "?e" :district/name nil "?e" :district/region nil]))

  ; there's a form and strip it
  (is (= (determine-colspec resultset1 fe1 {:display-mode :root})
         [:entity :db/id nil :entity :hypercrud/owner nil :entity :link/name nil :entity :link/renderer nil]))

  ; there's a form and use it
  (is (= (determine-colspec resultset1 fe1 {:display-mode :xray})
         [:entity
          :link/renderer
          {:db/id (->DbId 17592186045553 17592186045422),
           :field/attribute {:db/id (->DbId 17592186045451 17592186045422),
                             :attribute/ident :link/renderer,
                             :attribute/valueType {:db/id (->DbId 23 17592186045422), :db/ident :db.type/string},
                             :attribute/cardinality {:db/id (->DbId 35 17592186045422), :db/ident :db.cardinality/one}},
           :field/prompt "Renderer",
           :field/order 0}])))
