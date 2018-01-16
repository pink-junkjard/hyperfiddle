(ns hyperfiddle.ide.fiddles.main
  (:require [hypercrud.browser.browser-ui :as browser-ui]
            [hypercrud.browser.core :as browser]
            [hyperfiddle.appval.domain.core :as hyperfiddle]
            [hyperfiddle.ide]))


(defn renderer [result ordered-fes anchors ctx]
  [:div.hyperfiddle-ide
   (if-not (hyperfiddle/alias? (hyperfiddle/hostname->hf-domain-name (:hostname ctx) (:hyperfiddle-hostname ctx)))
     (let [ctx (assoc ctx :ui-error browser-ui/ui-error-inline)]
       [(:browse ctx) :topnav ctx :class "topnav hidden-print"]))
   (let [ctx (hyperfiddle.ide/target-ui-context ctx)]
     [browser/ui-from-route (:target-route ctx) ctx "app-browser"])])
