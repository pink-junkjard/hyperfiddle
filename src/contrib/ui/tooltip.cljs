(ns contrib.ui.tooltip
  (:require [contrib.reactive :as r]
            [contrib.ui.popover :as popover]
            [re-com.core :as re-com]))


; Tooltips are popovers, and implemented in terms of popovers


;(defn tooltip [& {:as props}]                               ; re-com style props
;  ; adapt standard props to re-com
;  (let [props (-> props
;                  (dissoc :body)                            ; leave :status and other props which don't need adaption
;                  (merge {:label (:body props)}))]
;    (apply re-com/popover-tooltip (apply concat props))))

(defn tooltip* [rstate t-props anchor]
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
        ;    a.hyperfiddle {position: absolute; }
        ;
        {:position :below-left :showing? rstate}
        t-props                                             ; label, status
        {:label (or (:label t-props) "")                    ; required
         :anchor [:span {:key 0
                         :on-mouse-over #(do (reset! rstate true) nil)
                         :on-mouse-out #(do (reset! rstate false) nil)}
                  anchor]}))))

(defn tooltip-thick* [rstate body anchor]
  (apply
    popover/anchor-wrapper*
    (apply concat {:position :below-right                   ; link tooltips prefer left; field labels prefer right
                   :showing? rstate
                   :popover [re-com/popover-content-wrapper
                             :body body
                             :no-clip? true
                             :tooltip-style? true
                             :popover-color "#333"
                             :padding "3px 8px"
                             :arrow-length 6
                             :arrow-width 12
                             :arrow-gap 4]
                   :anchor [:span {:key 0
                                   :on-mouse-over #(do (reset! rstate true) nil)
                                   :on-mouse-out #(do (reset! rstate false) nil)}
                            anchor]})))

(defn tooltip [t-props anchor]
  (if-not (:label t-props)
    anchor
    (r/partial tooltip* (r/atom false))))

(defn tooltip-thick [body anchor]
  (if-not body
    anchor
    (r/partial tooltip-thick* (r/atom false))))

(defn tooltip-props [tooltip-config]
  (let [[status label] (if (string? tooltip-config)
                         [:info tooltip-config]
                         [(first tooltip-config) (second tooltip-config)])]
    {:status status :label label}))
