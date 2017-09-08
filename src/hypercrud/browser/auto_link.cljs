(ns hypercrud.browser.auto-link
  (:require-macros [hypercrud.util.template :as template])
  (:require [hypercrud.client.core :as hc]
            [hypercrud.types.DbId :refer [->DbId]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.util.vedn :as vedn]))


(defn system-link? [link-dbid]
  (map? (:id link-dbid)))

(defn link-system-edit [conn owner fe]                      ; these need to be thick/hydrated params bc we are manufacturing a pulled tree here.
  {:pre [conn fe]}
  {:db/id (->DbId {:ident :system-edit
                   :owner (-> owner :db/id :id)
                   :conn (-> conn :db/id :id)
                   :fe (-> fe :db/id :id)}
                  (-> conn :db/id :id))                     ; this should be root-conn, bug
   ;:hypercrud/owner owner
   :link/name (str "system-" (:find-element/name fe))
   :request/type :entity
   :link-query/find-element [{:find-element/name "entity"
                              :find-element/connection conn}]})

(defn link-system-edit-attr [conn owner fe a]
  {:pre [conn fe a]}
  {:db/id (->DbId {:ident :system-edit-attr
                   :owner (-> owner :db/id :id)
                   :conn (-> conn :db/id :id)
                   :fe (-> fe :db/id :id)
                   :a a}
                  (-> conn :db/id :id))
   :link/name (str "system-" (:find-element/name fe) "-" a)
   ;:hypercrud/owner owner
   :request/type :entity
   :link-query/find-element [{:find-element/name "entity"
                              :find-element/connection conn}]})

(defn link-blank-system-remove [owner fe a]
  {:pre []}
  {:db/id (->DbId {:ident :sys-remove
                   :fe (-> fe :db/id :id)
                   :a a} nil)
   :link/name "sys-remove"
   ;:hypercrud/owner owner
   :request/type :blank
   :link/renderer (pr-str `(fn [result# colspec# anchors# param-ctx#]
                             [:p "Retract entity?"]))})

(def auto-link-txfn-lookup
  (->> (template/load-resource "auto-link/tx-fns.vedn")
       (vedn/read-string)))

; todo this belongs in auto-anchor namespace
(defn system-anchors
  "All sys links are :anchor/ident :sys, so they can be matched and merged with user-anchors.
  Matching is determined by [repeat? entity attribute ident]

  This function recurses in unexpected abstract way and impacts performance highly
  "
  [parent-link colspec param-ctx]
  (let [entity-links (->> (partition 4 colspec)
                          (map (fn [[_ fe _ _]] fe))
                          (distinct)
                          (mapcat (fn [fe]
                                    (let [fe-name (:find-element/name fe)
                                          edit {:db/id (->DbId {:ident :system-anchor-edit
                                                                :fe (-> fe :db/id :id)}
                                                               hc/*root-conn-id*)
                                                :anchor/prompt (str "edit-" fe-name)
                                                :anchor/ident (keyword (str "sys-edit-" fe-name))
                                                :anchor/link (link-system-edit (:find-element/connection fe) (:hypercrud/owner parent-link) fe)
                                                :anchor/repeating? true
                                                :anchor/managed? false
                                                :anchor/find-element fe}
                                          ; create links mirror edit links but repeating false, see auto-formula.
                                          ; This is because the connection comes from the find-element, and when merging
                                          ; sys links we match on the find-element.
                                          new {:db/id (->DbId {:ident :system-anchor-new
                                                               :fe (-> fe :db/id :id)}
                                                              hc/*root-conn-id*)
                                               :anchor/prompt (str "new-" fe-name)
                                               :anchor/ident (keyword (str "sys-new-" fe-name))
                                               :anchor/link (link-system-edit (:find-element/connection fe) (:hypercrud/owner parent-link) fe)
                                               :anchor/repeating? false ; not managed, no parent-child ref
                                               :anchor/find-element fe
                                               :anchor/managed? true
                                               :anchor/create? true
                                               :anchor/render-inline? true}
                                          remove {:db/id (->DbId {:ident :system-anchor-remove
                                                                  :fe (-> fe :db/id :id)}
                                                                 hc/*root-conn-id*)
                                                  :anchor/prompt (str "remove-" fe-name)
                                                  :anchor/ident (keyword (str "sys-remove-" fe-name))
                                                  :anchor/link (link-blank-system-remove (:hypercrud/owner parent-link) fe nil)
                                                  :anchor/repeating? true
                                                  :anchor/find-element fe
                                                  :anchor/managed? true
                                                  :anchor/render-inline? true
                                                  :anchor/tx-fn (:entity-remove auto-link-txfn-lookup)}]
                                      (case (:request/type parent-link)
                                        :entity [remove]

                                        :query [edit new remove]

                                        :blank []))))
                          doall)

        attr-links (if (not= :blank (:request/type parent-link))
                     (->> (partition 4 colspec)             ; driven by colspec, not find elements, because what matters is what's there.
                          (mapcat (fn [[db fe attr maybe-field]]
                                    (let [fe-name (-> fe :find-element/name)
                                          ident (-> attr :db/ident)
                                          conn {:db/id (->DbId (.-conn-id db) hc/*root-conn-id*)}]
                                      (if (and (not= ident :db/id) (= :db.type/ref (-> attr :db/valueType :db/ident)))
                                        [{:db/id (->DbId {:ident :system-anchor-edit-attr
                                                          :fe (-> fe :db/id :id)
                                                          :a ident}
                                                         hc/*root-conn-id*)
                                          :anchor/prompt (str "edit") ; conserve space in label
                                          :anchor/ident (keyword (str "sys-edit-" fe-name "-" ident))
                                          :anchor/repeating? true
                                          :anchor/find-element fe
                                          :anchor/attribute ident
                                          :anchor/managed? false
                                          :anchor/link (link-system-edit-attr conn (:hypercrud/owner parent-link) fe attr)}
                                         {:db/id (->DbId {:ident :system-anchor-new-attr
                                                          :fe (-> fe :db/id :id)
                                                          :a ident}
                                                         hc/*root-conn-id*)
                                          :anchor/prompt (str "new") ; conserve space in label
                                          :anchor/ident (keyword (str "sys-new-" fe-name "-" ident))
                                          :anchor/repeating? true ; manged - need parent-child ref
                                          :anchor/find-element fe
                                          :anchor/attribute ident
                                          :anchor/managed? true
                                          :anchor/create? true
                                          :anchor/render-inline? true
                                          :anchor/link (link-system-edit-attr conn (:hypercrud/owner parent-link) fe attr)}
                                         {:db/id (->DbId {:ident :system-anchor-remove-attr
                                                          :fe (-> fe :db/id :id)
                                                          :a ident}
                                                         hc/*root-conn-id*)
                                          :anchor/prompt (str "remove")
                                          :anchor/ident (keyword (str "sys-remove-" fe-name "-" ident))
                                          :anchor/link (link-blank-system-remove (:hypercrud/owner parent-link) fe attr)
                                          :anchor/find-element fe
                                          :anchor/attribute ident
                                          :anchor/repeating? true
                                          :anchor/managed? true
                                          :anchor/render-inline? true
                                          :anchor/tx-fn (if (= :db.cardinality/one (-> attr :db/cardinality :db/ident))
                                                          (:value-remove-one auto-link-txfn-lookup)
                                                          (:value-remove-many auto-link-txfn-lookup))}]))))
                          doall))]
    (concat entity-links attr-links)))


(defn request-for-system-link [system-link-idmap param-ctx]
  ; connection, owner, fe, attr.

  ; all this is in the parent-link, we can just overhydrate and then prune away what we don't need.
  ; That was the plan, but it doesn't work in second layer deep sys links, the parent is a sys link. So we
  ; need to just hydrate what we need.
  (let [dbval (hc/db (:peer param-ctx) hc/*root-conn-id* (:branch param-ctx))]
    [(->EntityRequest (->DbId (:owner system-link-idmap) hc/*root-conn-id*) nil dbval ['*])
     (if-let [conn (:conn system-link-idmap)] (->EntityRequest (->DbId conn hc/*root-conn-id*) nil dbval ['*]))
     (if-let [fe (:fe system-link-idmap)] (->EntityRequest (->DbId fe hc/*root-conn-id*) nil dbval [:db/id :find-element/name]))]))

(defn hydrate-system-link [system-link-idmap [owner conn fe] param-ctx]
  (let [owner-id (:owner system-link-idmap)
        conn-id (:conn system-link-idmap)
        fe-id (:fe system-link-idmap)
        a (:a system-link-idmap)]

    (case (:ident system-link-idmap)
      :system-edit (link-system-edit conn owner fe)
      :system-edit-attr (link-system-edit-attr conn owner fe a)
      :sys-remove (link-blank-system-remove owner fe a))))
