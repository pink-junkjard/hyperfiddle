(ns hyperfiddle.appval.domain.foundation
  (:require [cats.core :refer [mlet]]
            [cljs.pprint :as pprint]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hypercrud.compile.reader :as reader]
            [hypercrud.state.actions.core :as actions]
            [hypercrud.util.core :refer [unwrap]]
            [hypercrud.util.exception :refer [->Exception]]
            [hypercrud.util.non-fatal :refer [try-either]]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.util.string :as hc-string]

            [hyperfiddle.appval.domain.core :as foundation2]
    #?(:cljs [hypercrud.ui.stale :as stale])
    #?(:cljs [hypercrud.ui.control.code :refer [code*]])
    #?(:cljs [hypercrud.ui.css :refer [classes]])
    #?(:cljs [hyperfiddle.appval.domain.error :as error])))



; Can be removed once domain/databases are flattened up.
(defn process-domain-legacy [domain]
  (-> (into {} domain)
      (update :domain/code-databases
              (fn [repos]
                (->> repos
                     (map (fn [repo]
                            (-> (into {} repo)
                                ; todo this can throw
                                (update :repository/environment reader/read-string)))))))))

(defn local-basis [foo user-local-basis global-basis route]
  (let [domain nil #_"unused but theoretically allowed?"]
    (concat
      (:domain global-basis)
      (user-local-basis global-basis domain route))))

(defn canonical-route [route domain]
  (or (routing/decode' route)
      (unwrap (hc-string/safe-read-edn-string (:domain/home-route domain)))))

(defn api [foo user-api-fn hyperfiddle-hostname hostname route branch state-val]
  (let [ctx {:branch branch
             :dispatch! #(throw (->Exception "dispatch! not supported in hydrate-route"))
             :hostname hostname
             :hyperfiddle-hostname hyperfiddle-hostname
             ; just blast the peer everytime
             :peer (peer/->Peer (reactive/atom state-val))}
        domain-api (foundation2/domain-request (foundation2/hostname->hf-domain-name hostname hyperfiddle-hostname) (:peer ctx))
        domain (hc/hydrate-api (:peer ctx) domain-api)]
    (concat [domain-api]
            (if domain
              (user-api-fn domain (canonical-route route domain) state-val ctx)))))

#?(:cljs
   (defn staging [peer dispatch!]
     (let [stage-val @(reactive/cursor (.-state-atom peer) [:stage])
           edn (binding [pprint/*print-miser-width* nil
                         pprint/*print-right-margin* 200]
                 (with-out-str (pprint/pprint stage-val)))]
       ; todo this can throw
       [code* edn #(dispatch! (actions/reset-stage peer (reader/read-edn-string %)))])))

#?(:cljs
   (defn leaf-view [user-view-fn hyperfiddle-hostname hostname route ctx]
     (let [domain (let [user-domain (foundation2/hostname->hf-domain-name hostname hyperfiddle-hostname)]
                    (hc/hydrate-api (:peer ctx) (foundation2/domain-request user-domain (:peer ctx))))]
       ; A malformed stage can break bootstrap hydrates, but the root-page is bust, so ignore here
       ; Fix this by branching userland so bootstrap is sheltered from staging area? (There are chickens and eggs)
       (user-view-fn domain route ctx))))

#?(:cljs
   (defn page-view [user-view-fn hyperfiddle-hostname hostname route ctx]
     (let [domain' (hc/hydrate (:peer ctx) (foundation2/domain-request (foundation2/hostname->hf-domain-name hostname hyperfiddle-hostname) (:peer ctx)))]
       [stale/loading (stale/can-be-loading? ctx) domain'
        (fn [e]
          [:div.hyperfiddle.hyperfiddle-foundation
           [error/error-cmp e]
           [staging (:peer ctx) (:dispatch! ctx)]])
        (fn [domain]
          [:div {:class (apply classes "hyperfiddle-foundation" "hyperfiddle"
                               @(reactive/cursor (.-state-atom (:peer ctx)) [:pressed-keys]))}
           (user-view-fn domain route ctx)
           (if @(reactive/cursor (.-state-atom (:peer ctx)) [:staging-open])
             [staging (:peer ctx) (:dispatch! ctx)])])])))

#(:cljs
   (defn view [foo user-view-fn hyperfiddle-hostname hostname route ctx]
     (case foo
       ; The foundation comes with special root markup which means the foundation/view knows about page/user (not ide)
       ; Can't ide/user (not page) be part of the userland route?
       "page" (partial page-view user-view-fn hyperfiddle-hostname hostname route ctx)
       "ide" (partial leaf-view user-view-fn hyperfiddle-hostname hostname route ctx)
       "user" (partial leaf-view user-view-fn hyperfiddle-hostname hostname route ctx))))
