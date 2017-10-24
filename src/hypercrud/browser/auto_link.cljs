(ns hypercrud.browser.auto-link)


(defn system-link? [link-id]
  (map? link-id))

(defn link-system-edit [fe-name fe-conn]                    ; these need to be thick/hydrated params bc we are manufacturing a pulled tree here.
  {:pre [fe-name fe-conn]}
  {:db/id {:ident :system-edit
           :fe-name fe-name
           :fe-conn fe-conn}
   :link/name (str "system-" fe-name)
   :request/type :entity
   :link-query/find-element [{:find-element/name "entity"
                              :find-element/connection fe-conn}]})

(defn link-system-edit-attr [fe-name fe-conn a]
  {:pre [fe-name fe-conn a]}
  {:db/id {:ident :system-edit-attr
           :fe-name fe-name
           :fe-conn fe-conn
           :a a}
   :link/name (str "system-" fe-name "-" a)
   :request/type :entity
   :link-query/find-element [{:find-element/name "entity"
                              :find-element/connection fe-conn}]})

(defn link-blank-system-remove [fe-name a]
  {:pre []}
  {:db/id {:ident :sys-remove
           :fe-name fe-name
           :a a}
   :link/name "sys-remove"
   :request/type :blank
   :link/renderer (pr-str `(fn [result# ordered-fes# anchors# param-ctx#]
                             [:p "Retract entity?"]))})

(defn hydrate-system-link [{:keys [fe-name fe-conn a ident]} param-ctx]
  (case ident
    :system-edit (link-system-edit fe-name fe-conn)
    :system-edit-attr (link-system-edit-attr fe-name fe-conn a)
    :sys-remove (link-blank-system-remove fe-name a)))
