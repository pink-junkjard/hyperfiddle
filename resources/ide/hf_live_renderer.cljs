[:div props
 (let [[_ [inner-fiddle & inner-args]] @(:hypercrud.browser/route ctx)]
   [hyperfiddle.ide.hf-live/iframe ctx {:route [inner-fiddle inner-args]}])]
