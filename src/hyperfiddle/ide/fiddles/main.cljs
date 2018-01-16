(ns hyperfiddle.ide.fiddles.main
  (:require [hypercrud.browser.browser-ui :as browser-ui]
            [hypercrud.browser.core :as browser]
            [hyperfiddle.appval.domain.core :as hyperfiddle]
            [hyperfiddle.ide]))



; this is the ide-main-fiddle :fiddle/renderer
(defn ^:export renderer [result fes links ctx] ; Already an ide-page ctx
  (let [hide-ide (hyperfiddle/alias? (hyperfiddle/hostname->hf-domain-name (:hostname ctx) (:hyperfiddle-hostname ctx)))]
    [:div.hyperfiddle-ide

     (if-not hide-ide
       (let [ctx (assoc ctx :ui-error browser-ui/ui-error-inline
                            :peer (:peer-ide ctx))]
         [(:browse ctx) :topnav ctx :class "topnav hidden-print"]))

     (let [ctx (hyperfiddle.ide/target-ui-context ctx)]
       ; This is different than foo=user because it is special css at root attach point
       [browser/ui-from-route (:target-route ctx) ctx "app-browser"])]))
