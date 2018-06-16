(ns hyperfiddle.ui.form
  (:require
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [contrib.ui.input :refer [keyword-input* edn-input*]]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.system-link :refer [system-link?]]
    [hypercrud.ui.connection-color :refer [connection-color]]
    [hyperfiddle.ui.controls :as controls]
    [cuerdas.core :as str]))


(defn border-color [ctx]
  (let [shadow-link @(r/fmap system-link? (r/fmap :db/id (context/entity ctx)))]
    (if-not shadow-link (connection-color ctx))))

(defn ui-block-border-wrap [ctx class & children]
  [:div {:class class :style {:border-color (border-color ctx)}}
   (apply fragment children)])

(defn magic-new-head [field ctx props]
  (let [entity (context/entity ctx)
        ;read-only (r/fmap (comp not controls/writable-entity?) entity) -- don't check this, '* always has a dbid and is writable
        state (r/cursor (::state ctx) [::magic-new-a])]
    (println (str/format "magic-new-head: %s , %s , %s" @state (pr-str @entity)))
    [keyword-input* @state (r/partial reset! state)
     (merge props {:placeholder ":task/title"})]))


(letfn [(change! [ctx entity state v]
          ((:user-with! ctx) [[:db/add @(r/fmap :db/id entity) @state v]]))]
  (defn magic-new-body [value ctx props]
    (let [entity (context/entity ctx)
          read-only (r/fmap (comp not controls/writable-entity?) entity)
          state (r/cursor (::state ctx) [::magic-new-a])]
      (println (str/format "magic-new-body: %s , %s , %s" @state @read-only (pr-str @entity)))
      ; Uncontrolled widget on purpose i think
      ; Cardinality many is not needed, because as soon as we assoc one value,
      ; we dispatch through a proper typed control
      [edn-input* nil (r/partial change! ctx entity state)
       (merge props {:read-only (let [_ [@state @read-only]] ; force reactions
                                  (or (nil? @state) @read-only))
                     :placeholder (pr-str "mow the lawn")})])))
