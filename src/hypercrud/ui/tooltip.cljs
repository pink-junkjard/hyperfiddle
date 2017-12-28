(ns hypercrud.ui.tooltip
  (:require [hypercrud.util.reactive :as reactive]
            [re-com.core :as re-com]))


; Tooltips are popovers, and implemented in terms of popovers


;(defn tooltip [& {:as props}]                               ; re-com style props
;  ; adapt standard props to re-com
;  (let [props (-> props
;                  (dissoc :body)                            ; leave :status and other props which don't need adaption
;                  (merge {:label (:body props)}))]
;    (apply re-com/popover-tooltip (apply concat props))))

(defn tooltip [t-props anchor]
  ; if there is no tooltip, the hovers are such that it will never be shown
  (if-not (:label t-props)
    anchor
    (let [state (reactive/atom false)]
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
              t-props                                       ; label, status
              {:label (or (:label t-props) "")              ; required
               :anchor [:span {:key 0
                               :on-mouse-over #(do (if (:label t-props) (reset! state true)) nil)
                               :on-mouse-out #(do (if (:label t-props) (reset! state false)) nil)}
                        anchor]})))))))

