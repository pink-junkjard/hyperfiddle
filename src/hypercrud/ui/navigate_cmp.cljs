(ns hypercrud.ui.navigate-cmp
  (:require [hypercrud.browser.routing :as routing]
            [hypercrud.platform.native-event-listener :refer [native-listener]]
            [hypercrud.ui.tooltip :as tooltip]
            [hypercrud.util.core :as util]
            [promesa.core :as p]
            [reagent.core :as r]
            [re-com.core :as re-com]))


; props = {
;   :route    {:keys [domain project link-dbid query-params]}
;   :tooltip  [Keyword Hiccup]
;   :txfns    {:stage () => Promise[Nil]
;              :cancel () => ()}
;   :popover  () => Hiccup
;   :hidden   Boolean
;
;   ; any other standard HTML anchor props, e.g.
;   :class    String
;   :style    Map[CSS-Key, CSS-Value]
;   :on-click (e) => ()
; }
; todo all HF prop values should be monads and
; tooltip can be generated within navigate-cmp by mapping over them
(defn navigate-cmp* [hypercrud-props label]
  ; why doesn't this just take anchor? - param-ctx should not leak up this high.
  ; props (links/build-link-props anchor param-ctx)
  ; and because nested router. Is that even needed now?
  (if-not (:hidden hypercrud-props)
    (let [anchor-props (-> hypercrud-props
                           (dissoc :route :tooltip :txfns :popover :hidden :show-popover?)
                           (assoc :href (if (:route hypercrud-props)
                                          (routing/encode (:route hypercrud-props))
                                          nil #_"javascript:void 0;")
                                  :class (str (:class hypercrud-props) " hf-auto-nav")))]
      [:span.nav-link.hf-nav-link
       [tooltip/fast-hover-tooltip-managed
        (let [tooltip-config (:tooltip hypercrud-props)
              [status label] (if (string? tooltip-config)
                               [:info tooltip-config]
                               [(first tooltip-config) (second tooltip-config)])]
          {:status status :label label})
        [re-com/popover-anchor-wrapper
         :showing? (or (:show-popover? hypercrud-props) (atom false))
         :position :below-center
         :anchor (if (:popover hypercrud-props)
                   (let [btn-props (assoc anchor-props :on-click #(when-not (:disabled anchor-props)
                                                                    ((get-in hypercrud-props [:txfns :open]))))]
                     [:button btn-props [:span label]])

                   ; Why would an anchor have an on-click? Is this historical.
                   ; If legit it needs to respect disabled.
                   [native-listener (select-keys anchor-props [:on-click])
                    [:a (dissoc anchor-props :on-click)
                     [:span label]]])
         :popover (let [{cancel! :cancel stage! :stage :or {stage! #() cancel! #()}} (:txfns hypercrud-props)]
                    [re-com/popover-content-wrapper
                     :on-cancel (fn []
                                  ; todo catch exceptions - ... hypercrud bugs, right? Not user errors
                                  (cancel!))
                     :no-clip? true
                     :body (if (:popover hypercrud-props)
                             [:div
                              ((:popover hypercrud-props))
                              [:button {:on-click (fn []
                                                    ; todo something better with these exceptions ... hypercrud bugs, right? Not user errors
                                                    (p/catch (stage!) #(-> % util/pprint-str js/alert)))}
                               "stage"]])])]]])))

; act like a function down-stack
(defn navigate-cmp [props label]
  [navigate-cmp* props label])
