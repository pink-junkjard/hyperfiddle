(ns hypercrud.ui.auto-control-default
  (:require [hypercrud.client.core :as hc]
            [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.ui.code-editor :as code-editor]
            [hypercrud.ui.widget :as widget]))


(defn code-iframe [fieldinfo graph forms value expanded-cur change! transact! tempid!]
  [:div.code-parent.code-iframe
   [code-editor/code-editor* value change!]
   [:iframe {:key (hc/t graph) :src "http://www.hypercrud.com/" #_(:iframe-url fieldinfo)}]])


(defn widget-for-fieldinfo [{:keys [datatype cardinality component name]}]
  (cond
    (= name :project/code) code-iframe

    (and (= datatype :string) (= cardinality :one)) widget/input
    (and (= datatype :code) (= cardinality :one)) widget/code-editor
    (and (= datatype :ref) (= cardinality :one) component) widget/select-ref-component
    (and (= datatype :ref) (= cardinality :many) component) widget/multi-select-ref-component
    (and (= datatype :ref) (= cardinality :one)) widget/select-ref
    (and (= datatype :ref) (= cardinality :many)) widget/multi-select-ref
    :else widget/default))


(defmethod auto-control/auto-control :default
  [fieldinfo graph forms value expanded-cur change! transact! tempid!]
  [(widget-for-fieldinfo fieldinfo) fieldinfo graph forms value expanded-cur change! transact! tempid!])
