[:div {:class class}
 (let [[_ [inner-fiddle & inner-args]] (:route ctx)]
   [hyperfiddle.ui/iframe
    (assoc ctx :user-renderer (hyperfiddle.ide.hf-live/hf-live))
    {:route [inner-fiddle inner-args]}])]
