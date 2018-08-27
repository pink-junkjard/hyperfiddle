[:div props
 (let [[_ [inner-fiddle & inner-args]] (:route ctx)]
   [hyperfiddle.ui/iframe ctx
    {:route [inner-fiddle inner-args]
     :user-renderer (hyperfiddle.ide.hf-live/hf-live)}])]
