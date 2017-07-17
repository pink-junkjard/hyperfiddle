(ns hypercrud.browser.auto-link
  (:require [hypercrud.browser.link-util :as link-util]
            [hypercrud.client.core :as hc]
            [hypercrud.types.DbId :refer [->DbId]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.ui.form-util :as form-util]))


(defn system-link? [link-dbid]
  (map? (:id link-dbid)))

(defn link-system-edit [conn owner fe]                      ; these need to be thick/hydrated params bc we are manufacturing a pulled tree here.
  {:pre [conn owner fe]}
  {:db/id (->DbId {:ident :system-edit
                   :owner (-> owner :db/id :id)
                   :conn (-> conn :db/id :id)
                   :fe (-> fe :db/id :id)}
                  (-> conn :db/id :id))                     ; this should be root-conn, bug
   :hypercrud/owner owner
   :link/name (str "system-" (:find-element/name fe))
   :request/type :entity
   :link-query/find-element [{:find-element/name "entity"
                              :find-element/connection conn}]})

(defn link-system-edit-attr [conn owner fe a]
  {:pre [conn owner fe a]}
  {:db/id (->DbId {:ident :system-edit-attr
                   :owner (-> owner :db/id :id)
                   :conn (-> conn :db/id :id)
                   :fe (-> fe :db/id :id)
                   :a (-> a :db/id :id)}
                  (-> conn :db/id :id))
   :link/name (str "system-" (:find-element/name fe) "-" (:attribute/ident a))
   :hypercrud/owner owner
   :request/type :entity
   :link-query/find-element [{:find-element/name "entity"
                              :find-element/connection conn}]})

(defn link-blank-system-remove [owner fe a]
  {:pre [owner]}
  {:db/id (->DbId {:ident :sys-remove
                   :fe (-> fe :db/id :id)
                   :a (-> a :db/id :id)} nil)
   :link/name "sys-remove"
   :hypercrud/owner owner
   :request/type :blank
   :link/renderer (pr-str `(fn [result# colspec# anchors# param-ctx#]
                             [:p "Retract entity?"]))})

; todo this belongs in auto-anchor namespace
(defn system-anchors
  "All sys links are :anchor/ident :sys, so they can be matched and merged with user-anchors.
  Matching is determined by [repeat? entity attribute ident]"
  [parent-link colspec param-ctx]
  (let [find-elements (if (not= :blank (:request/type parent-link))
                        (->> (form-util/get-ordered-find-elements parent-link param-ctx)
                             (mapv (juxt :find-element/name identity))
                             (into {})))

        ; entity links for link-query (have fe)
        ; or, entity links for link-entity
        entity-links (->> find-elements
                          (mapcat (fn [[fe-name fe]]
                                    (let [edit {:anchor/prompt (str "sys-edit-" fe-name)
                                                :anchor/ident (keyword (str "sys-edit-" fe-name))
                                                :anchor/link (link-system-edit (:find-element/connection fe) (:hypercrud/owner parent-link) fe)
                                                :anchor/repeating? true
                                                :anchor/managed? false
                                                :anchor/find-element fe}
                                          ; create links mirror edit links but repeating false, see auto-formula.
                                          ; This is because the connection comes from the find-element, and when merging
                                          ; sys links we match on the find-element.
                                          new {:anchor/prompt (str "sys-new-" fe-name)
                                               :anchor/ident (keyword (str "sys-new-" fe-name))
                                               :anchor/link (link-system-edit (:find-element/connection fe) (:hypercrud/owner parent-link) fe)
                                               :anchor/repeating? false ; not managed, no parent-child ref
                                               :anchor/find-element fe
                                               :anchor/managed? true
                                               :anchor/create? true
                                               :anchor/render-inline? true}
                                          remove {:anchor/prompt (str "sys-remove-" fe-name)
                                                  :anchor/ident (keyword (str "sys-remove-" fe-name))
                                                  :anchor/link (link-blank-system-remove (:hypercrud/owner parent-link) fe nil)
                                                  :anchor/repeating? true
                                                  :anchor/find-element fe
                                                  :anchor/managed? true
                                                  :anchor/render-inline? true
                                                  :anchor/tx-fn (pr-str `(fn [ctx# get-tx-from-modal#]
                                                                           {:tx (concat (get-tx-from-modal#)
                                                                                        [[:db.fn/retractEntity (-> ctx# :entity :db/id)]])}))}]
                                      (case (:request/type parent-link)
                                        :entity [remove]

                                        :query [edit new remove]

                                        :blank []))))
                          doall)

        attr-links (if (not= :blank (:request/type parent-link))
                     (->> (partition 4 colspec)             ; driven by colspec, not find elements, because what matters is what's there.
                          (mapcat (fn [[db fe attr maybe-field]]
                                    (let [fe-name (-> fe :find-element/name)
                                          ident (-> attr :attribute/ident)
                                          conn {:db/id (->DbId (.-conn-id db) hc/*root-conn-id*)}
                                          fe (get find-elements fe-name)
                                          _ (assert fe)]
                                      (if (and (not= ident :db/id) (= :db.type/ref (-> attr :attribute/valueType :db/ident)))
                                        [{:anchor/prompt (str "sys-edit-" fe-name "-" ident) ; conserve space in label
                                          :anchor/ident (keyword (str "sys-edit-" fe-name "-" ident))
                                          :anchor/repeating? true
                                          :anchor/find-element fe
                                          :anchor/attribute attr
                                          :anchor/managed? false
                                          :anchor/link (link-system-edit-attr conn (:hypercrud/owner parent-link) fe attr)}
                                         {:anchor/prompt (str "sys-new-" fe-name "-" ident) ; conserve space in label
                                          :anchor/ident (keyword (str "sys-new-" fe-name "-" ident))
                                          :anchor/repeating? true ; manged - need parent-child ref
                                          :anchor/find-element fe
                                          :anchor/attribute attr
                                          :anchor/managed? true
                                          :anchor/create? true
                                          :anchor/render-inline? true
                                          :anchor/link (link-system-edit-attr conn (:hypercrud/owner parent-link) fe attr)}
                                         {:anchor/prompt (str "sys-remove-" fe-name "-" ident)
                                          :anchor/ident (keyword (str "sys-remove-" fe-name "-" ident))
                                          :anchor/link (link-blank-system-remove (:hypercrud/owner parent-link) fe attr)
                                          :anchor/find-element fe
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
                                                                                       (mapv (fn [e#] [:db.fn/retractEntity (:db/id e#)]))))})))}]))))
                          doall))]
    (concat entity-links attr-links)))


(defn request-for-system-link [root-db system-link-idmap]   ; always latest
  ; connection, owner, fe, attr.

  ; all this is in the parent-link, we can just overhydrate and then prune away what we don't need.
  ; That was the plan, but it doesn't work in second layer deep sys links, the parent is a sys link. So we
  ; need to just hydrate what we need.

  [(->EntityRequest (->DbId (:owner system-link-idmap) hc/*root-conn-id*) nil root-db ['*])
   (if-let [conn (:conn system-link-idmap)] (->EntityRequest (->DbId conn hc/*root-conn-id*) nil root-db ['*]))
   (if-let [fe (:fe system-link-idmap)] (->EntityRequest (->DbId fe hc/*root-conn-id*) nil root-db
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

(defn hydrate-system-link [system-link-idmap [owner conn fe a] param-ctx]
  (let [owner-id (:owner system-link-idmap)                 ; hydrate this?
        conn-id (:conn system-link-idmap)
        fe-id (:fe system-link-idmap)
        attr-id (:a system-link-idmap)]

    (case (:ident system-link-idmap)
      :system-edit (link-system-edit conn owner fe)
      :system-edit-attr (link-system-edit-attr conn owner fe a)
      :sys-remove (link-blank-system-remove owner fe a))))
