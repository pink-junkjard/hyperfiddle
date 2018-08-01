(let [{:keys [:hypercrud.browser/fiddle
              :hypercrud.browser/result]} ctx]
  [:div {:class class}
   (let [[_ [inner-fiddle & inner-args]] (:route ctx)]
     [hypercrud.browser.core/ui-from-route
      [inner-fiddle inner-args]
      (assoc ctx :user-renderer (hyperfiddle.ide.hf-live/hf-live))])])