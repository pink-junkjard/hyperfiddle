(ns hyperfiddle.ide.fiddles.main
  (:require [hypercrud.browser.browser-ui :as browser-ui]))


; this is the ide-main-fiddle :fiddle/renderer
(defn ^:export renderer [result fes links ctx]
  (let [ctx (assoc ctx :ui-error browser-ui/ui-error-inline)]
    ; This ought to lift a layer, topnav could be the entry point.
    [(:browse ctx) :topnav ctx :class "topnav hidden-print"]))
