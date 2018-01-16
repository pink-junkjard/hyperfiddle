(ns hyperfiddle.appval.domain.foundation-view
  (:require [cats.core :refer [mlet]]
            [cats.monad.either :as either]
            [cljs.pprint :as pprint]
            [clojure.string :as string]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.core :as hc]
            [hypercrud.compile.reader :as reader]
            [hypercrud.state.actions.core :as actions]
            [hypercrud.state.actions.util :as actions-util]
            [hypercrud.ui.control.code :refer [code*]]
            [hypercrud.ui.navigate-cmp :as navigate-cmp]
            [hypercrud.ui.stale :as stale]
            [hypercrud.util.non-fatal :refer [try-either]]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.util.string :as hc-string]
            [hyperfiddle.appval.domain.foundation :as foundation]
            [hyperfiddle.appval.domain.core :as hf]
            [hyperfiddle.appval.domain.error :as error]))


(defn staging [peer dispatch!]
  (let [stage-val @(reactive/cursor (.-state-atom peer) [:stage])
        edn (binding [pprint/*print-miser-width* nil
                      pprint/*print-right-margin* 200]
              (with-out-str (pprint/pprint stage-val)))]
    ; todo this can throw
    [code* edn #(dispatch! (actions/reset-stage peer (reader/read-edn-string %)))]))

; Users can't bypass the foundation.
(defn view [user-view-fn ctx]
  (let [state-atom (.-state-atom (:peer ctx))
        either-v (or (some-> @(reactive/cursor state-atom [:error]) either/left)
                     (let [state-route @(reactive/cursor state-atom [:encoded-route])]
                       (mlet [maybe-decoded-route (if state-route
                                                    (try-either (routing/decode state-route))
                                                    (either/right nil))
                              target-domain (let [hf-domain-name (hf/hostname->hf-domain-name (:hostname ctx) (:hyperfiddle-hostname ctx))]
                                              (hc/hydrate (:peer ctx) (hf/domain-request hf-domain-name (:peer ctx))))
                              hf-domain (hc/hydrate (:peer ctx) (hf/domain-request "hyperfiddle" (:peer ctx)))
                              maybe-decoded-route (or (some-> maybe-decoded-route either/right)
                                                      (hc-string/safe-read-edn-string (:domain/home-route target-domain)))]
                         (if maybe-decoded-route
                           (either/right [target-domain hf-domain maybe-decoded-route])
                           (either/left "Please set a home route")))))
        ui-props {:class (string/join " " @(reactive/cursor state-atom [:pressed-keys]))}]
    [stale/loading (stale/can-be-loading? ctx) either-v
     (fn [e]
       [:div.hyperfiddle.hyperfiddle-foundation ui-props
        [error/error-cmp e]
        [staging (:peer ctx) (:dispatch! ctx)]])
     (fn [[target-domain hf-domain decoded-route]]
       [:div.hyperfiddle.hyperfiddle-foundation ui-props
        (user-view-fn target-domain hf-domain decoded-route ctx)
        (if @(reactive/cursor state-atom [:staging-open])
          [staging (:peer ctx) (:dispatch! ctx)])])]))
