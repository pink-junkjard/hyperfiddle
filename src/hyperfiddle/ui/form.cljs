(ns hyperfiddle.ui.form
  (:require
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [contrib.ui.input :as input]
    [hypercrud.browser.system-link :refer [system-link?]]
    [hypercrud.ui.connection-color :refer [connection-color]]))


(defn border-color [ctx]
  (let [shadow-link @(r/fmap system-link? (r/cursor (:cell-data ctx) [:db/id]))]
    (if-not shadow-link (connection-color ctx))))

(defn ui-block-border-wrap [ctx class & children]
  [:div {:class class :style {:border-color (connection-color ctx)}} ; should call border-color?
   (apply fragment children)])

(defn new-field-state-container [ctx]
  (let [attr-ident (r/atom nil)]
    (fn [ctx]
      (let [missing-dbid (nil? @(r/cursor (:cell-data ctx) [:db/id]))]
        (fragment
          [:div (let [on-change! #(reset! attr-ident %)]
                  [input/keyword-input* @attr-ident on-change! {:read-only missing-dbid
                                                                :placeholder ":task/title"}])]
          (let [on-change! #(let [id @(r/cursor (:cell-data ctx) [:db/id])]
                              ; todo cardinality many
                              ((:user-with! ctx) [[:db/add id @attr-ident %]]))
                props {:read-only (or missing-dbid (nil? @attr-ident))
                       :placeholder (pr-str "mow the lawn")}]
            [input/edn-input* nil on-change! props]))))))

(defn new-field [ctx]
  ^{:key (hash (keys @(:cell-data ctx)))}
  [new-field-state-container ctx])
