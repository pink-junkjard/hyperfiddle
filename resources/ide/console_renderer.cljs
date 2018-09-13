(let [{:keys [:hypercrud.browser/fiddle] :as ctx} (hyperfiddle.ide.console-links/inject-console-links ctx)]
  [:div props
   [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]
   [hyperfiddle.ui/result val ctx {}]])