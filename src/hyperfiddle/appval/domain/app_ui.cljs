(ns hyperfiddle.appval.domain.app-ui
  (:require [cats.core :refer [mlet]]
            [cats.monad.either :as either]
            [cljs.pprint :as pprint]
            [clojure.string :as string]
            [hypercrud.browser.core :as browser]
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
            [hyperfiddle.appval.domain.app :as app]
            [hyperfiddle.appval.domain.core :as hf]
            [hyperfiddle.appval.domain.error :as error]))


(defn staging [peer dispatch!]
  (let [stage-val @(reactive/cursor (.-state-atom peer) [:stage])
        edn (binding [pprint/*print-miser-width* nil
                      pprint/*print-right-margin* 200]
              (with-out-str (pprint/pprint stage-val)))]
    ; todo this can throw
    [code* edn #(dispatch! (actions/reset-stage peer (reader/read-edn-string %)))]))

(let [page-on-click (fn [ctx target-domain route event]
                      (when (and route (.-altKey event))
                        (let [can-soft-nav? (->> (:domain/code-databases target-domain)
                                                 ; only if the user domain has the root code-database
                                                 (filter #(and (= (:dbhole/name %) "root")
                                                               (= (:dbhole/uri %) hf/root-uri)))
                                                 (empty?)
                                                 not)]
                          (if can-soft-nav?
                            ((:dispatch! ctx) (fn [dispatch! get-state]
                                                (let [encoded-route (routing/encode route)]
                                                  (when (actions-util/navigable? encoded-route (get-state))
                                                    (actions/set-route (:peer ctx) encoded-route dispatch! get-state)))))
                            (let [encoded-route (routing/encode route (str "hyperfiddle." (:hyperfiddle-hostname ctx)))]
                              ; todo push this window.location set up to the appfn atom watcher
                              (aset js/window "location" encoded-route)))
                          (.stopPropagation event))))]
  (defn hf-ui-context [ctx hf-domain target-domain target-route user-profile]
    (-> (app/hf-context ctx hf-domain target-domain target-route user-profile)
        (assoc :navigate-cmp navigate-cmp/navigate-cmp
               :page-on-click (reactive/partial page-on-click ctx target-domain)))))

(defn target-ui-context [ctx]
  (-> (app/target-context ctx (:target-domain ctx) (:target-route ctx) (:user-profile ctx))
      (dissoc :page-on-click)))

(defn ui [ctx]
  (let [state-atom (.-state-atom (:peer ctx))
        either-v (or (some-> @(reactive/cursor state-atom [:error]) either/left)
                     (let [state-route @(reactive/cursor state-atom [:encoded-route])]
                       (mlet [maybe-decoded-route (if state-route
                                                    (try-either (routing/decode state-route))
                                                    (either/right nil))
                              target-domain (let [hf-domain-name (hf/hostname->hf-domain-name (:hostname ctx) (:hyperfiddle-hostname ctx))]
                                              @(hc/hydrate (:peer ctx) (hf/domain-request hf-domain-name (:peer ctx))))
                              hf-domain @(hc/hydrate (:peer ctx) (hf/domain-request "hyperfiddle" (:peer ctx)))
                              maybe-decoded-route (or (some-> maybe-decoded-route either/right)
                                                      (hc-string/safe-read-edn-string (:domain/home-route target-domain)))]
                         (if maybe-decoded-route
                           (either/right [target-domain hf-domain maybe-decoded-route])
                           (either/left "Please set a home route")))))
        ui-props {:class (string/join " " @(reactive/cursor state-atom [:pressed-keys]))}]
    [stale/loading (stale/can-be-loading? ctx) either-v
     (fn [e]
       [:div.hyperfiddle ui-props
        [error/error-cmp e]
        [staging (:peer ctx) (:dispatch! ctx)]])
     (fn [[target-domain hf-domain decoded-route]]
       (let [ctx (-> (hf-ui-context ctx hf-domain target-domain decoded-route @(reactive/cursor state-atom [:user-profile]))
                     (update :debug str "-v"))]
         [:div.hyperfiddle ui-props
          [browser/ui-from-route (app/main-route ctx) ctx]
          (if @(reactive/cursor state-atom [:staging-open])
            [staging (:peer ctx) (:dispatch! ctx)])]))]))
