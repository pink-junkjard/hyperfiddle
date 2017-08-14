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
(defn navigate-cmp* [props label]
  ; why doesn't this just take anchor? - param-ctx should not leak up this high.
  ; props (links/build-link-props anchor param-ctx)
  ; and because nested router. Is that even needed now?
  (if-not (:hidden props)
    (let [state-popover (r/atom false)]
      (fn [props label]
        (let [txfns (:txfns props)
              anchor-props (-> props
                               (dissoc :route :tooltip :txfns :popover :hidden)
                               (assoc :href (if (:route props)
                                              (routing/encode (:route props))
                                              nil #_"javascript:void 0;")
                                      :class (str (:class props) " hf-auto-nav")))]
          [:span.nav-link.hf-nav-link
           [tooltip/hover-tooltip-managed
            {:label (-> props :tooltip second)
             :status (-> props :tooltip first)}
            [re-com/popover-anchor-wrapper
             :showing? state-popover
             :position :below-center
             :anchor
             (if txfns
               [:button {:href (:href anchor-props)
                         :style (:style anchor-props)
                         :class (:class anchor-props)
                         :on-click #(reset! state-popover true)}
                [:span label]]
               [native-listener (select-keys anchor-props [:on-click])
                [:a (dissoc anchor-props :on-click)
                 [:span label]]])
             :popover [re-com/popover-content-wrapper
                       :on-cancel (fn []
                                    (reset! state-popover false)
                                    ; todo catch exceptions
                                    ((:cancel txfns)))
                       :no-clip? true
                       :body (if (:popover props)
                               [:div
                                ((:popover props))
                                [:button {:on-click (fn []
                                                      (reset! state-popover false)
                                                      ; todo something better with these exceptions
                                                      (p/catch ((:stage txfns)) #(-> % util/pprint-str js/alert)))}
                                 "stage"]])]]]])))))

; act like a function down-stack
(defn navigate-cmp [props label]
  [navigate-cmp* props label])
