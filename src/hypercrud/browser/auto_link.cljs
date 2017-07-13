(ns hypercrud.browser.auto-link
  (:require [hypercrud.browser.link-util :as link-util]
            [hypercrud.client.core :as hc]
            [hypercrud.types.DbId :refer [->DbId]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.ui.form-util :as form-util]))


(defn system-link? [link-dbid]
  (map? (:id link-dbid)))

(defn link-query-system-edit [conn owner e]                 ; these need to be thick/hydrated params bc we are manufacturing a pulled tree here.
  {:pre [conn owner e]}
  {:db/id (->DbId {:ident :system-edit
                   :owner (-> owner :db/id :id)
                   :conn (-> conn :db/id :id)
                   :e (-> e :db/id :id)}
                  (-> conn :db/id :id))                     ; this should be root-conn, bug
   :hypercrud/owner owner
   :link/name (str "system-" (:find-element/name e))
   :request/type :entity
   :link-query/find-element [{:find-element/name "entity"
                              :find-element/connection conn}]})

(defn link-entity-system-edit [conn owner]
  {:pre [conn owner]}
  {:db/id (->DbId {:ident :system-edit
                   :owner (-> owner :db/id :id)
                   :conn (-> conn :db/id :id)}
                  (-> conn :db/id :id))
   :hypercrud/owner owner
   :link/name (str "system-entity")
   :request/type :entity
   :link-query/find-element [{:find-element/name "entity"
                              :find-element/connection conn}]})

(defn link-query-system-edit-attr [conn owner e a]
  {:pre [conn owner e a]}
  {:db/id (->DbId {:ident :system-edit-attr
                   :owner (-> owner :db/id :id)
                   :conn (-> conn :db/id :id)
                   :e (-> e :db/id :id)
                   :a (-> a :db/id :id)}
                  (-> conn :db/id :id))
   :link/name (str "system-" (:find-element/name e) "-" (:attribute/ident a))
   :hypercrud/owner owner
   :request/type :entity
   :link-query/find-element [{:find-element/name "entity"
                              :find-element/connection conn}]})

(defn link-entity-system-edit-attr [conn owner a]
  {:pre [conn owner a]}
  {:db/id (->DbId {:ident :system-edit-attr
                   :owner (-> owner :db/id :id)
                   :conn (-> conn :db/id :id)
                   :a (-> a :db/id :id)}
                  (-> conn :db/id :id))
   :link/name (str "system-entity-" (:attribute/ident a))
   :hypercrud/owner owner
   :request/type :entity
   :link-query/find-element [{:find-element/name "entity"
                              :find-element/connection conn}]})

(defn link-blank-system-remove [owner e a]
  {:pre [owner]}
  {:db/id (->DbId {:ident :sys-remove
                   :e (-> e :db/id :id)
                   :a (-> a :db/id :id)} nil)
   :link/name "sys-remove"
   :hypercrud/owner owner
   :link/renderer (pr-str `(fn [result# colspec# anchors# param-ctx#]
                             [:p "Retract entity?"]))})

; todo this belongs in auto-anchor namespace
(defn system-anchors
  "All sys links are :anchor/ident :sys, so they can be matched and merged with user-anchors.
  Matching is determined by [repeat? entity attribute ident]"
  [parent-link colspec param-ctx]
  (let [type (link-util/link-type parent-link)

        find-elements (case type
                        :link-query (->> (form-util/get-ordered-find-elements parent-link param-ctx)
                                         (mapv (juxt :find-element/name identity))
                                         (into {}))
                        :link-entity nil
                        :link-blank nil)

        ; entity links for link-query (have fe)
        ; or, entity links for link-entity
        entity-links (case type
                       :link-query
                       (->> find-elements
                            (mapcat (fn [[fe-name fe]]
                                      [{:anchor/prompt (str "sys-edit-" fe-name)
                                        :anchor/ident (keyword (str "sys-edit-" fe-name))
                                        :anchor/link (link-query-system-edit (:find-element/connection fe) (:hypercrud/owner parent-link) fe)
                                        :anchor/repeating? true
                                        :anchor/managed? false
                                        :anchor/find-element fe}
                                       ; create links mirror edit links but repeating false, see auto-formula.
                                       ; This is because the connection comes from the find-element, and when merging
                                       ; sys links we match on the find-element.
                                       {:anchor/prompt (str "sys-new-" fe-name)
                                        :anchor/ident (keyword (str "sys-new-" fe-name))
                                        :anchor/link (link-query-system-edit (:find-element/connection fe) (:hypercrud/owner parent-link) fe)
                                        :anchor/repeating? false
                                        :anchor/find-element fe
                                        :anchor/managed? true
                                        :anchor/create? true
                                        :anchor/render-inline? false}
                                       {:anchor/prompt (str "sys-remove-" fe-name)
                                        :anchor/ident (keyword (str "sys-remove-" fe-name))
                                        :anchor/link (link-blank-system-remove (:hypercrud/owner parent-link) fe nil)
                                        :anchor/repeating? true
                                        :anchor/find-element fe
                                        :anchor/managed? true
                                        :anchor/render-inline? true
                                        :anchor/tx-fn (pr-str `(fn [ctx# get-tx-from-modal#]
                                                                 {:tx (concat (get-tx-from-modal#)
                                                                              [[:db.fn/retractEntity (-> ctx# :entity :db/id)]])}))}]))
                            doall)

                       :link-entity
                       (let [conn-id (or
                                       (-> parent-link :link-query/find-element
                                           (->> (filter #(= (:find-element/name %) "entity"))
                                                first)
                                           :find-element/connection :db/id :id)
                                       (-> param-ctx :query-params :entity :conn-id) #_"cardinality many parent conn-id is correct")
                             conn {:db/id (->DbId conn-id hc/*root-conn-id*)}]
                         [#_{:anchor/prompt "edit"
                             :anchor/ident :sys
                             :anchor/link (link-entity-system-edit parent-link conn-id)
                             :anchor/repeating? true
                             :anchor/managed? false
                             :anchor/find-element nil}
                          {:anchor/prompt "sys-remove-entity"
                           :anchor/ident (keyword "sys-remove-entity")
                           :anchor/link (link-blank-system-remove (:hypercrud/owner parent-link) nil nil) ; "entity"
                           :anchor/repeating? true
                           :anchor/find-element nil         ; "entity"
                           :anchor/managed? true
                           :anchor/render-inline? true
                           :anchor/tx-fn (pr-str `(fn [ctx# get-tx-from-modal#]
                                                    {:tx (concat (get-tx-from-modal#)
                                                                 [[:db.fn/retractEntity (-> ctx# :entity :db/id)]])}))}])

                       :link-blank [])


        attr-links (case type
                     :link-query
                     (->> (partition 4 colspec)             ; driven by colspec, not find elements, because what matters is what's there.
                          (mapcat (fn [[db fe attr maybe-field]]
                                    (let [fe-name (-> fe :find-element/name)
                                          ident (-> attr :attribute/ident)
                                          conn {:db/id (->DbId (.-conn-id db) hc/*root-conn-id*)}
                                          fe (get find-elements fe-name)
                                          _ (assert fe)]
                                      (case (-> attr :attribute/valueType :db/ident)
                                        :db.type/ref
                                        [{:anchor/prompt (str "sys-edit-" fe-name "-" ident) ; conserve space in label
                                          :anchor/ident (keyword (str "sys-edit-" fe-name "-" ident))
                                          :anchor/repeating? true
                                          :anchor/find-element fe
                                          :anchor/attribute attr
                                          :anchor/managed? false
                                          :anchor/link (link-query-system-edit-attr conn (:hypercrud/owner parent-link) fe attr)}
                                         {:anchor/prompt (str "sys-new-" fe-name "-" ident) ; conserve space in label
                                          :anchor/ident (keyword (str "sys-new-" fe-name "-" ident))
                                          :anchor/repeating? false
                                          :anchor/find-element fe
                                          :anchor/attribute attr
                                          :anchor/managed? true
                                          :anchor/create? true
                                          :anchor/render-inline? true
                                          :anchor/link (link-query-system-edit-attr conn (:hypercrud/owner parent-link) fe attr)}
                                         {:anchor/prompt (str "sys-remove-" fe-name "-" ident)
                                          :anchor/ident (keyword (str "sys-remove-" fe-name "-" ident))
                                          :anchor/link (link-blank-system-remove (:hypercrud/owner parent-link) fe attr)
                                          :anchor/find-element nil
                                          :anchor/attribute attr
                                          :anchor/repeating? true
                                          :anchor/managed? true
                                          :anchor/render-inline? true
                                          :anchor/tx-fn (pr-str (if (= :db.cardinality/one (-> attr :attribute/cardinality :db/ident))
                                                                  `(fn [ctx# get-tx-from-modal#]
                                                                     {:tx (concat (get-tx-from-modal#)
                                                                                  [[:db.fn/retractEntity (-> ctx# :value :db/id)]])})
                                                                  `(fn [ctx# get-tx-from-modal#]
                                                                     {:tx (concat (get-tx-from-modal#)
                                                                                  (->> (:value ctx#)
                                                                                       (mapv (fn [e#] [:db.fn/retractEntity (:db/id e#)]))))})))}]

                                        nil))))
                          doall)

                     :link-entity
                     (->> (partition 4 colspec)
                          (mapcat (fn [[db fe attr maybe-field]]
                                    (let [fe-name (-> fe :find-element/name) ; "entity" ?
                                          ident (-> attr :attribute/ident)
                                          conn {:db/id (->DbId (.-conn-id db) hc/*root-conn-id*)}]
                                      (case (-> attr :attribute/valueType :db/ident)
                                        :db.type/ref
                                        [{:anchor/prompt (str "sys-edit-" fe-name "-" ident) ; conserve space in label
                                          :anchor/ident (keyword (str "sys-edit-" fe-name "-" ident))
                                          :anchor/repeating? true
                                          :anchor/managed? false
                                          :anchor/find-element nil
                                          :anchor/attribute attr
                                          :anchor/link (link-entity-system-edit-attr conn (:hypercrud/owner parent-link) attr)}
                                         {:anchor/prompt (str "sys-new-" fe-name "-" ident) ; conserve space in label
                                          :anchor/ident (keyword (str "sys-new-" fe-name "-" ident))
                                          :anchor/repeating? true
                                          :anchor/find-element nil
                                          :anchor/attribute attr
                                          :anchor/managed? true
                                          :anchor/create? true
                                          :anchor/render-inline? true
                                          :anchor/link (link-entity-system-edit-attr conn (:hypercrud/owner parent-link) attr)}
                                         {:anchor/prompt (str "sys-remove-" fe-name "-" ident)
                                          :anchor/ident (keyword (str "sys-remove-" fe-name "-" ident))
                                          :anchor/link (link-blank-system-remove (:hypercrud/owner parent-link) fe attr)
                                          :anchor/find-element nil
                                          :anchor/attribute attr
                                          :anchor/repeating? true
                                          :anchor/managed? true
                                          :anchor/render-inline? true
                                          :anchor/tx-fn (pr-str
                                                          (if (= :db.cardinality/one (-> attr :attribute/cardinality :db/ident))
                                                            `(fn [ctx# get-tx-from-modal#]
                                                               {:tx (concat (get-tx-from-modal#)
                                                                            [[:db.fn/retractEntity (-> ctx# :value :db/id)]])})
                                                            `(fn [ctx# get-tx-from-modal#]
                                                               {:tx (concat (get-tx-from-modal#)
                                                                            (->> (:value ctx#)
                                                                                 (mapv (fn [e#] [:db.fn/retractEntity (:db/id e#)]))))})))}]
                                        nil))))
                          doall)

                     :link-blank [])]
    (concat entity-links attr-links)))


(defn request-for-system-link [root-db system-link-idmap]   ; always latest
  ; connection, owner, fe, attr.

  ; all this is in the parent-link, we can just overhydrate and then prune away what we don't need.
  ; That was the plan, but it doesn't work in second layer deep sys links, the parent is a sys link. So we
  ; need to just hydrate what we need.

  [(->EntityRequest (->DbId (:owner system-link-idmap) hc/*root-conn-id*) nil root-db ['*])
   (if-let [conn (:conn system-link-idmap)] (->EntityRequest (->DbId conn hc/*root-conn-id*) nil root-db ['*]))
   (if-let [e (:e system-link-idmap)] (->EntityRequest (->DbId e hc/*root-conn-id*) nil root-db
                                                       [:db/id
                                                        :find-element/name
                                                        :find-element/connection
                                                        {:find-element/form ['* {:form/field
                                                                                 ['*
                                                                                  {:field/attribute ['*
                                                                                                     {:attribute/valueType [:db/id :db/ident]}
                                                                                                     {:attribute/cardinality [:db/id :db/ident]}
                                                                                                     {:attribute/unique [:db/id :db/ident]}]}]}]}]))
   (if-let [a (:a system-link-idmap)] (->EntityRequest (->DbId a hc/*root-conn-id*) nil root-db
                                                       ['*
                                                        {:attribute/valueType [:db/id :db/ident]
                                                         :attribute/cardinality [:db/id :db/ident]
                                                         :attribute/unique [:db/id :db/ident]}]))])

(defn hydrate-system-link [system-link-idmap [owner conn e a] param-ctx]
  (let [owner-id (:owner system-link-idmap)                 ; hydrate this?
        conn-id (:conn system-link-idmap)
        fe-id (:e system-link-idmap)
        attr-id (:a system-link-idmap)]

    (case (:ident system-link-idmap)

      :system-edit
      (if fe-id
        (link-query-system-edit conn owner e)
        (link-entity-system-edit conn owner))

      :system-edit-attr
      (if fe-id
        (link-query-system-edit-attr conn owner e a)
        (link-entity-system-edit-attr conn owner a))

      :sys-remove
      (link-blank-system-remove owner e a))))
