(ns hypercrud.browser.system-links
  (:require [hypercrud.browser.link-util :as link-util]
            [hypercrud.client.core :as hc]
            [hypercrud.types :refer [->DbId ->DbVal ->EntityRequest]]
            [hypercrud.ui.form-util :as form-util]))


(defn system-link? [link-dbid]
  (map? (:id link-dbid)))

(defn system-edit-link-dbid [parent-link fe]
  (->DbId {:ident :system-edit
           :parent-link (:db/id parent-link)
           :find-element (:db/id fe)}
          (-> parent-link :db/id :conn-id)))


(defn system-edit-attr-link-dbid [parent-link fe attr]
  (->DbId {:ident :system-edit-attr
           :parent-link (:db/id parent-link)
           :find-element (:db/id fe)
           :attribute (:db/id attr)}
          (-> parent-link :db/id :conn-id)))


(defn system-edit-link [parent-link fe]
  (assert parent-link)
  (assert fe)
  {:db/id (system-edit-link-dbid parent-link fe)
   :hypercrud/owner (:hypercrud/owner parent-link)
   :link/name (str "edit ")                                 ; hmm
   :link/request {:link-entity/connection (:find-element/connection fe)}})


(defn system-edit-attr-link [parent-link fe attr]
  (assert parent-link)
  (assert fe)
  (assert attr)
  {:db/id (system-edit-attr-link-dbid parent-link fe attr)
   :link/name (str "edit " (:find-element/name fe) " " (:attribute/ident attr))
   :hypercrud/owner (:hypercrud/owner parent-link)
   :link/request {:link-entity/connection (:find-element/connection fe)}})


(defn system-anchors
  "All sys links are :anchor/ident :sys, so they can be matched and merged with user-anchors.
  Matching is determined by [repeat? entity attribute ident]"
  [parent-link result param-ctx]
  (let [colspec (form-util/determine-colspec result parent-link param-ctx) ; colspec can be empty if result is empty and no form.
        find-elements (form-util/find-elements-by-name (:link/request parent-link))]
    (case (link-util/link-type parent-link)
      :link-query
      (let [edit-links (->> find-elements
                            (mapv (fn [[fe-name fe]]
                                    {:anchor/prompt (str fe-name)
                                     :anchor/ident :sys
                                     :anchor/link (system-edit-link parent-link fe)
                                     :anchor/repeating? true
                                     :anchor/find-element fe})))

            edit-attr-links (->> (partition 4 colspec)      ; driven by colspec, not find elements, because what matters is what's there.
                                 (mapcat (fn [[conn fe-name ident maybe-field]]
                                           (let [fe (get find-elements fe-name)
                                                 attr ((:schema param-ctx) ident)]
                                             (case (-> attr :attribute/valueType :db/ident)
                                               :db.type/ref [{:anchor/prompt (str "edit") ; conserve space in label
                                                              :anchor/ident :sys
                                                              :anchor/link (system-edit-attr-link parent-link fe attr)
                                                              :anchor/repeating? true
                                                              :anchor/find-element fe
                                                              :anchor/attribute attr}]
                                               nil))))
                                 doall)
            create-links (->> find-elements
                              ; Really we want distinct connections - but that causes problems.
                              ; 1. we want to auto-generate the formulas, not macro it here.
                              ; 2. In the link merge step they need to match differently and the conn is not a field to match on.
                              (mapv (fn [[fe-name fe]]
                                      {:anchor/ident :sys
                                       :anchor/find-element fe
                                       :anchor/prompt (str "create in " fe-name)
                                       :anchor/link (system-edit-link parent-link fe)
                                       :anchor/repeating? false})))]
        (concat create-links edit-links edit-attr-links))

      :link-entity []                                       ; No system links yet for entity links. What will there be?
      ; entities get edit-attr-links
      [])))


(defn request-for-system-link [system-link-idmap]
  (assert (:parent-link system-link-idmap))
  ; both need parent-link
  ; both need fe
  ; one needs attr
  ; all this is in the parent-link, we can just overhydrate and then prune away what we don't need.
  (->EntityRequest (:parent-link system-link-idmap)
                   (->DbVal hc/*root-conn-id* nil)
                   (let [form-pull-exp ['* {:form/field
                                            ['*
                                             {:field/attribute ['*
                                                                {:attribute/valueType [:db/id :db/ident]}
                                                                {:attribute/cardinality [:db/id :db/ident]}
                                                                {:attribute/unique [:db/id :db/ident]}]}]}]]
                     [:db/id
                      {:hypercrud/owner ['*]
                       :link/request
                       [:db/id
                        {:link-query/find-element [:db/id
                                                   :find-element/name
                                                   :find-element/connection
                                                   {:find-element/form form-pull-exp}]}]}])))





(defn generate-system-link [system-link-idmap parent-link]
  (let [fe (first (filter #(= (:db/id %) (:find-element system-link-idmap))
                          (-> parent-link :link/request :link-query/find-element)))
        attr (first (filter #(= (-> % :field/attribute :db/id)  (:attribute system-link-idmap))
                            (-> fe :find-element/form :form/field)))]
    (case (:ident system-link-idmap)
      :system-edit (system-edit-link parent-link fe)
      :system-edit-attr (system-edit-attr-link parent-link fe attr))))
