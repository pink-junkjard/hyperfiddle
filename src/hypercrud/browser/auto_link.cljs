(ns hypercrud.browser.auto-link
  (:require [cats.monad.either :refer-macros [try-either]]))


(defn system-link? [fiddle-id]
  (map? fiddle-id))

(defn link-system-edit [fe-name]                            ; these need to be thick/hydrated params bc we are manufacturing a pulled tree here.
  {:pre [fe-name]}
  {:db/id {:ident :system-edit
           :fe-name fe-name}
   :fiddle/name (str "system-" fe-name)
   :fiddle/type :entity})

(defn link-system-edit-attr [fe-name a]
  {:pre [fe-name a]}
  {:db/id {:ident :system-edit-attr
           :fe-name fe-name
           :a a}
   :fiddle/name (str "system-" fe-name "-" a)
   :fiddle/type :entity})

(defn link-blank-system-remove [fe-name a]
  {:db/id {:ident :sys-remove
           :fe-name fe-name
           :a a}
   :fiddle/name "sys-remove"
   :fiddle/type :blank
   :fiddle/renderer (pr-str `(fn [result# ordered-fes# anchors# ctx#]
                               [:p "Retract entity?"]))})

(defn hydrate-system-link [{:keys [fe-name a ident]} ctx]
  (try-either
    ; catch all the pre assertions
    (case ident
      :system-edit (link-system-edit fe-name)
      :system-edit-attr (link-system-edit-attr fe-name a)
      :sys-remove (link-blank-system-remove fe-name a))))
