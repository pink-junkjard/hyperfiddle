(ns hypercrud.ui.tooltip
  (:require [re-com.core :as re-com]
            [reagent.core :as r]))


;(defn tooltip [& {:as props}]                               ; re-com style props
;  ; adapt standard props to re-com
;  (let [props (-> props
;                  (dissoc :body)                            ; leave :status and other props which don't need adaption
;                  (merge {:label (:body props)}))]
;    (apply re-com/popover-tooltip (apply concat props))))

(defn popover-anchor-wrapper* [& args]
  ; re-com has some weird "sugar" which inhibits dynamic props so turn it into a fn
  (apply vector re-com/popover-anchor-wrapper args))

;(defn popover [& {:as props}]
;  (let [props (-> props
;                  (dissoc :body)                            ; leave :status and other props which don't need adaption
;                  (merge {:popover
;                          [re-com/popover-content-wrapper
;                           :on-cancel #(do (reset! state false) nil)
;                           :no-clip? true
;                           :body (:body props)]
;
;                          }))]
;    (apply popover-anchor-wrapper* (apply concat props))))

(defn hover-tooltip-managed [& args]
  ; if there is no tooltip, the hovers are such that it will never be shown
  (let [state (r/atom false)]
    (fn [t-props anchor]
      (apply
        re-com/popover-tooltip
        (apply
          concat
          (merge
            ; Position parameter is very tricky.
            ; Our table layout is not compatible with :above-* ; the point div will push our anchor down.
            ; Recom will detect clipping, and depending on which quadrant of the screen were in, it may override
            ; to :above-* which causes the jank.
            ;
            ; If we set to below-left, the only person who might flip upwards is someone in the bottom half of a page.
            ; Since the devtools are often there, this doesn't happen a lot.
            ;
            ; This css is helpful when udnerstanding:
            ;    .rc-popover-point { position: absolute !important; }
            ;    a.hf-auto-nav {position: absolute; }
            ;
             {:position :below-left
              :showing? state}
             t-props                                        ; label, status
             {:label (or (:label t-props) "")               ; required
              :anchor [:span {:key 0
                              :on-mouse-over #(do (if (:label t-props) (reset! state true)) nil)
                              :on-mouse-out #(do (if (:label t-props) (reset! state false)) nil)}
                       anchor]}))))))

(defn fast-hover-tooltip-managed [t-props anchor]
  ; if there is no tooltip, the hovers are such that it will never be shown
  (if (:label t-props)
    [hover-tooltip-managed t-props anchor]
    anchor))

(defn click-popover* [state t-props anchor]
  (apply
    popover-anchor-wrapper*
    (apply
      concat
      (merge
        {:position :below-left
         :showing? state}
        (dissoc t-props :body)
        {:anchor [:span {:on-click #(do (swap! state not) nil)}
                  anchor]
         :popover [re-com/popover-content-wrapper
                   :on-cancel #(do (reset! state false) nil)
                   :no-clip? true
                   :body (:body t-props)]}))))

(defn click-popover-managed* [& args]
  (let [state (r/atom false)]
    (fn [& args]
      (apply click-popover* state args))))

(defn- hover-popover* [state t-props anchor]                ; accept :label instead of :body to standardize
  (apply
    popover-anchor-wrapper*
    (apply
      concat
      (merge
        {:position :below-left
         :showing? state}
        (dissoc t-props :label :status)                     ; just ignore status, todo fix

        ; Delay hide and cancel delay if we enter again.
        ; Track enter and leave on both anchor and content.
        ; https://github.com/react-bootstrap/react-bootstrap/pull/1557/files

        {:anchor [:span {:on-mouse-enter #(do (if (:label t-props) (reset! state true)) nil)
                         :on-mouse-leave #(do (if (:label t-props) (reset! state false)) nil)}
                  anchor]
         :popover [re-com/popover-content-wrapper
                   :no-clip? true
                   :body (:label t-props)]}))))

(defn- hover-popover-managed* [& args]
  (let [state (r/atom false)]
    (fn [& args]
      (apply hover-popover* state args))))


; public interface is functions - why was this important again?
; was it for teasing apart this file into smaller pieces?

(defn click-popover-managed [& args]
  (apply vector click-popover-managed* args))

(defn click-popover [& args]
  (apply vector click-popover* args))

(defn hover-popover-managed [& args]
  (apply vector hover-popover-managed* args))

(defn hover-popover [& args]
  (apply vector hover-popover* args))
