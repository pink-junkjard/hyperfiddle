(ns hypercrud.ui.navigate-cmp
  (:require [contrib.css :refer [classes]]
            [contrib.keypress :refer [with-keychord]]
            [contrib.ui.tooltip :refer [tooltip]]
            [re-com.core :as re-com]))


(defn dissoc-non-native-props [hypercrud-props]
  (dissoc hypercrud-props :route :tooltip :popover :hidden))

(defn anchor-cmp [route-encode hypercrud-props label]
  {:pre [(not (:on-click hypercrud-props))]}
  (let [anchor-props (-> hypercrud-props
                         (dissoc-non-native-props)
                         (assoc :href (if (:route hypercrud-props)
                                        (route-encode (:route hypercrud-props))
                                        nil #_"javascript:void 0;")))]
    [:a anchor-props label]))

(defn popover-cmp [hypercrud-props label]
  (let [{:keys [showing? body open! close!]} (:popover hypercrud-props)]
    [with-keychord
     "esc" #(do (js/console.warn "esc") (close!))
     [re-com/popover-anchor-wrapper
      :showing? showing?
      :position :below-center
      :anchor (let [btn-props (-> hypercrud-props
                                  (dissoc-non-native-props)
                                  (assoc :on-click open!)
                                  ; use twbs btn coloring but not "btn" itself
                                  (update :class #(classes % "btn-default")))]
                [:button btn-props [:span (str label "▾")]])
      :popover [re-com/popover-content-wrapper
                :no-clip? true
                :body body]]]))

; props = {
;   :route    [fiddle args]
;   :tooltip  [Keyword Hiccup]
;   :popover  {:showing?  Atom
;              :body      Component
;              :open!     () => ()
;              :cancel!   () => ()}
;   :hidden   Boolean
;
;   ; any other standard HTML anchor props, e.g.
;   :class    String
;   :style    Map[CSS-Key, CSS-Value]
;   :on-click (e) => ()
; }
; todo all HF prop values should be monads and
; tooltip can be generated within navigate-cmp by mapping over them
(defn navigate-cmp* [route-encode hypercrud-props label & [class]]
  ; why doesn't this just take anchor? - ctx should not leak up this high.
  ; props (links/build-link-props anchor ctx)
  ; and because nested router. Is that even needed now?
  (if-not (:hidden hypercrud-props)
    (let [hypercrud-props (update hypercrud-props :class #(classes % class "hf-auto-nav"))]
      (if (some-> hypercrud-props :popover :showing? deref)

        ; this means popover AND popover-showing - so omit the formula tooltip
        [popover-cmp hypercrud-props label]

        ; no popover showing - so can draw tooltip
        [tooltip
         (let [tooltip-config (:tooltip hypercrud-props)
               [status label] (if (string? tooltip-config)
                                [:info tooltip-config]
                                [(first tooltip-config) (second tooltip-config)])]
           {:status status :label label})
         (if (:popover hypercrud-props)
           [popover-cmp hypercrud-props label]
           [anchor-cmp route-encode hypercrud-props label])]))))

; act like a appfn down-stack
(defn navigate-cmp [route-encode props label & [class]]
  [navigate-cmp* route-encode props label class])
