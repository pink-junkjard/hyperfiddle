(ns hypercrud.ui.attribute
  (:require [hypercrud.ui.auto-control :refer [auto-control auto-table-cell]]
            [hypercrud.ui.field :as field]))


; What even is this
(defn attribute-props [field links ctx]
  ; why does this need the field - it needs the ident for readonly in "Edit Anchors"
  ; todo clean this interface up
  {:read-only ((get ctx :read-only) (:attribute ctx) ctx)})

(defn Attribute [field links props ctx]
  (let [control (case (:layout ctx) :block auto-control
                                    :inline-block auto-table-cell
                                    :table auto-table-cell)
        props (attribute-props field links ctx)
        display-mode @(:display-mode ctx)
        ; What is the user-field allowed to change? The ctx. Can it change links or anchors? no.
        control (case display-mode :xray control :user (get ctx :control control))]
    ; todo control can have access to repeating contextual values (color, owner, result, entity, value, etc) but field should NOT
    ; this leads to inconsistent location formulas between non-repeating links in tables vs forms
    ((field/with-field control) field links props ctx)))
