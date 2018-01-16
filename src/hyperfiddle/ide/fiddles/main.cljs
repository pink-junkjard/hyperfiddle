(ns hyperfiddle.ide.fiddles.main
  (:require [hypercrud.browser.browser-ui :as browser-ui]
            [hypercrud.browser.core :as browser]
            [hyperfiddle.appval.domain.core :as hyperfiddle]
            [hyperfiddle.ide]))

(defn hide-ide? [ctx]
  (hyperfiddle/alias? (hyperfiddle/hostname->hf-domain-name (:hostname ctx) (:hyperfiddle-hostname ctx))))

(defn view [result ordered-fes anchors ctx]
  [:div.hyperfiddle-ide

   (if-not (hide-ide? ctx)
     ; Why is this already an IDE ctx?
     (let [ctx (assoc ctx :ui-error browser-ui/ui-error-inline
                          :peer (:peer-ide ctx))]
       [(:browse ctx) :topnav ctx :class "topnav hidden-print"]))

   (let [ctx (-> (hyperfiddle.ide/target-ui-context ctx)
                 (assoc :peer (:peer-user ctx)))]
     [browser/ui-from-route (:target-route ctx) ctx "app-browser"])])

(def ^:export renderer view)                                ; compat
