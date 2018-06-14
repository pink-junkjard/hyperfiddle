(ns hyperfiddle.ui.form
  (:require
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [contrib.ui.input :as input]
    [hypercrud.browser.system-link :refer [system-link?]]
    [hypercrud.ui.connection-color :refer [connection-color]]
    [hypercrud.browser.context :as context]))


(defn border-color [ctx]
  (let [shadow-link @(r/fmap system-link? (r/fmap :db/id (context/entity ctx)))]
    (if-not shadow-link (connection-color ctx))))

(defn ui-block-border-wrap [ctx class & children]
  [:div {:class class :style {:border-color (connection-color ctx)}} ; should call border-color?
   (apply fragment children)])

(defn new-field-state-container [ctx]
  (let [attr-ident (r/atom nil)]
    (fn [ctx]
      (let [entity (context/entity ctx)
            missing-dbid (nil? @(r/fmap :db/id entity))]
        (fragment
          [:div (let [on-change! #(reset! attr-ident %)]
                  [input/keyword-input* @attr-ident on-change! {:read-only missing-dbid
                                                                :placeholder ":task/title"}])]
          ; Cardinality many is not needed, because as soon as we assoc one value, we go through hyper-control
          (let [on-change! #((:user-with! ctx) [[:db/add @(r/fmap :db/id entity) @attr-ident %]])
                props {:read-only (or missing-dbid (nil? @attr-ident))
                       :placeholder (pr-str "mow the lawn")}]
            [input/edn-input* nil on-change! props]))))))

(defn new-field [ctx]
  ^{:key (hash @(r/fmap keys (context/entity ctx)))}
  [new-field-state-container ctx])
