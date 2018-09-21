(ns hyperfiddle.actions
  (:require [cats.core :refer [mlet]]
            [cats.labs.promise]
            [cats.monad.either :as either]
            [contrib.ct :refer [unwrap]]
            [contrib.data :refer [map-values map-keys]]
            [contrib.datomic-tx :as tx]
            [contrib.uri :refer [->URI]]
            [hypercrud.browser.router :as router]
            [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hypercrud.client.schema :as schema]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.Err :as Err]
            [hypercrud.util.branch :as branch]
            [hyperfiddle.io.hydrate-requests :refer [hydrate-all-or-nothing!]]
            [hyperfiddle.io.util :refer [v-not-nil?]]
            [hyperfiddle.domain :as domain]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.security.client :as security]
            [promesa.core :as p]
            [taoensso.timbre :as timbre]))


(defn set-display-mode [display-mode]
  [:set-display-mode display-mode])

; batch doesn't make sense with thunks (can be sync or async dispatches in a thunk),
; user beware
(defn batch [& action-list] (cons :batch action-list))

(defn hydrate-partition [rt branch on-start dispatch! get-state]
  (dispatch! (apply batch (conj on-start [:hydrate!-start branch])))
  (let [{:keys [route local-basis hydrate-id ::runtime/branch-aux]} (get-in (get-state) [::runtime/partitions branch])]
    (assert route)
    (assert (not (string? route)))
    (-> (runtime/hydrate-route rt local-basis route branch branch-aux (map-values :stage (::runtime/partitions (get-state))))
        (p/then (fn [{:keys [ptm tempid-lookups]}]
                  (if (= hydrate-id (get-in (get-state) [::runtime/partitions branch :hydrate-id]))
                    (dispatch! [:hydrate!-success branch ptm tempid-lookups])
                    (timbre/info (str "Ignoring response for " hydrate-id)))))
        (p/catch (fn [error]
                   (if (= hydrate-id (get-in (get-state) [::runtime/partitions branch :hydrate-id]))
                     (dispatch! [:partition-error branch error])
                     (timbre/info (str "Ignoring response for " hydrate-id)))
                   (throw error))))))

(defn refresh-global-basis [rt dispatch! get-state]
  (-> (runtime/global-basis rt)
      (p/then (fn [global-basis]
                (dispatch! [:set-global-basis global-basis])))
      (p/catch (fn [error]
                 (dispatch! [:set-error error])
                 (throw error)))))

(defn refresh-partition-basis [rt branch dispatch! get-state]
  (let [{:keys [::runtime/global-basis]} (get-state)
        {:keys [route ::runtime/branch-aux]} (get-in (get-state) [::runtime/partitions branch])]
    (-> (runtime/local-basis rt global-basis route branch branch-aux)
        (p/then (fn [local-basis]
                  (dispatch! [:partition-basis branch local-basis])))
        (p/catch (fn [error]
                   (dispatch! [:partition-error branch error])
                   (throw error))))))

(defn refresh-domain [rt dispatch! get-state]
  (-> (runtime/domain rt)
      (p/then (fn [domain]
                (dispatch! [::runtime/set-domain domain])))
      (p/catch (fn [error]
                 (dispatch! [:set-error error])
                 (throw error)))))

(defn refresh-user [rt dispatch! get-state]
  (let [users-uri (->URI "datomic:free://datomic:4334/hyperfiddle-users") ; todo inject
        beta-uri (->URI "datomic:free://datomic:4334/~dustin.getz@hyperfiddle.net+beta") ; todo inject
        basis (->> (if-let [global-basis @(runtime/state rt [::runtime/global-basis])]
                     (:ide global-basis)
                     (->> @(runtime/state rt [::runtime/partitions])
                          (some (fn [[_ partition]]
                                  (->> (:local-basis partition)
                                       (filter (fn [[k _]] (#{users-uri beta-uri} k)))
                                       seq)))))
                   (filter (comp #{users-uri beta-uri} first))
                   (into {}))
        stage nil
        requests (let [user-id @(runtime/state rt [::runtime/user-id])
                       $users (hc/db rt users-uri nil)]
                   (cond-> [(->EntityRequest [:user/user-id user-id] $users [:hyperfiddle.ide/parinfer])]
                     ; todo this should be modeled on the domain/project
                     (= "tank" @(runtime/state rt [::runtime/domain :domain/ident]))
                     (conj
                       (let [$beta (hc/db rt beta-uri nil)]
                         (->EntityRequest [:user/user-id user-id] $beta [:hfnet.beta/accepted-on :hfnet.beta/archived])))))]
    (-> (hydrate-all-or-nothing! rt basis stage requests)
        (p/then (fn [responses]
                  (dispatch! [:set-user (apply merge {} responses)])))
        (p/catch (fn [error]
                   (dispatch! [:set-error error])
                   (throw error))))))

(defn add-partition [rt route branch branch-aux & on-start]
  (fn [dispatch! get-state]
    (if-let [e (router/invalid-route? route)]
      (do
        (dispatch! [:partition-error e])
        (p/rejected e))
      (do
        (dispatch! (apply batch (conj (vec on-start) [:add-partition branch route branch-aux])))
        (-> (refresh-partition-basis rt branch dispatch! get-state)
            (p/then #(hydrate-partition rt branch nil dispatch! get-state)))))))

(defn discard-partition [branch]
  [:discard-partition branch])

(defn close-popover [branch popover-id]
  [:close-popover branch popover-id])

(defn set-route [rt route branch keep-popovers? force dispatch! get-state]
  (assert (nil? branch) "Non-nil branches currently unsupported")
  (let [current-route (get-in (get-state) [::runtime/partitions branch :route])]
    (if (and (not force) (router/compare-routes route current-route) (not= route current-route))
      (dispatch! [:partition-route branch route])           ; just update state without re-hydrating
      (if-let [e (router/invalid-route? route)]
        (do (dispatch! [:set-error e])
            (p/rejected e))
        ; currently branches only have relationships to parents, need to be able to find all children from a parent
        ; this would allow us to discard/close ourself and our children
        ; for now we are always nil branch, so blast everything
        (let [actions (->> (::runtime/partitions (get-state))
                           (mapcat (fn [[ident partition]]
                                     (conj (if (and (= branch ident) keep-popovers?)
                                             (vector)
                                             (mapv (partial close-popover ident) (:popovers partition)))
                                           (if (= branch ident)
                                             [:partition-route ident route] ; dont blast nil stage
                                             (discard-partition ident))))))]
          (dispatch! (apply batch actions))
          ; should just call foundation/bootstrap-data
          (-> (refresh-partition-basis rt branch dispatch! get-state)
              (p/then #(hydrate-partition rt branch nil dispatch! get-state))))))))

(defn update-to-tempids [get-state branch uri tx]
  (let [{:keys [tempid-lookups ptm]} (get-in (get-state) [::runtime/partitions branch])
        dbval (->DbVal uri branch)
        schema (let [schema-request (schema/schema-request dbval)]
                 (-> (peer/hydrate-val+ schema-request ptm)
                     (either/branch (fn [e] (throw e)) identity)))
        id->tempid (get tempid-lookups uri)]
    (map (partial tx/stmt-id->tempid id->tempid schema) tx)))

(defn transact! [rt invert-route tx-groups dispatch! get-state & {:keys [route post-tx]}]
  (dispatch! [:transact!-start])
  (let [tx-groups (map-values (partial filter v-not-nil?)   ; hack because the ui still generates some garbage tx
                              tx-groups)]
    (-> (runtime/transact! rt tx-groups)
        (p/catch (fn [e]
                   #?(:cljs
                      (let [message (cond
                                      (string? e) e
                                      (Err/Err? e) (:msg e)
                                      (map? e) (:message e)
                                      :else (ex-message e))]
                        (js/alert message)))
                   (dispatch! [:transact!-failure e])
                   (throw e)))
        (p/then (fn [{:keys [tempid->id]}]
                  (dispatch! (apply batch [:transact!-success (keys tx-groups)] post-tx))
                  ; todo should just call foundation/bootstrap-data
                  (mlet [_ (refresh-global-basis rt dispatch! get-state)
                         _ (refresh-domain rt dispatch! get-state)
                         _ (refresh-user rt dispatch! get-state)
                         :let [invert-id (fn [temp-id uri]
                                           (get-in tempid->id [uri temp-id] temp-id))
                               current-route (get-in (get-state) [::runtime/partitions nil :route])
                               route' (-> (or route current-route)
                                          (invert-route invert-id))
                               keep-popovers? (or (nil? route) (router/compare-routes route current-route))]]
                    ; todo we want to overwrite our current browser location with this new url
                    ; currently this new route breaks the back button
                    (set-route rt route' nil keep-popovers? true dispatch! get-state)))))))

(defn should-transact!? [uri get-state]
  (and (get-in (get-state) [::runtime/auto-transact uri])
       (let [hf-db (domain/uri->hfdb uri (::runtime/domain (get-state))) ; todo this needs sourced from context domain for topnav
             user-id (get-in (get-state) [::runtime/user-id])
             user (get-in (get-state) [::runtime/user])]
         (either/branch
           (security/subject-can-transact? hf-db user-id user)
           #(do
              (timbre/error %)
              false)
           identity))))

(defn with-groups [rt invert-route branch tx-groups & {:keys [route post-tx]}]
  {:pre [(not-any? nil? (keys tx-groups))]}
  (fn [dispatch! get-state]
    (let [tx-groups (->> tx-groups
                         (remove (fn [[uri tx]] (empty? tx)))
                         (map (fn [[uri tx]] [uri (update-to-tempids get-state branch uri tx)]))
                         (into {}))
          transact-uris (->> (keys tx-groups)
                             (filter (fn [uri] (and (nil? branch) (should-transact!? uri get-state)))))
          transact-groups (select-keys tx-groups transact-uris)
          with-actions (->> (apply dissoc tx-groups transact-uris)
                            (mapv (fn [[uri tx]] [:with branch uri tx])))]
      (if (not (empty? transact-groups))
        ; todo what if transact throws?
        (transact! rt invert-route transact-groups dispatch! get-state
                   :post-tx (let [clear-uris (->> (keys transact-groups)
                                                  (map (fn [uri] [:reset-stage-uri branch uri nil]))
                                                  vec)]
                              (concat clear-uris            ; clear the uris that were transacted
                                      with-actions))
                   :route route)
        (if-let [e (some-> route router/invalid-route?)]
          (dispatch! (apply batch (conj with-actions [:set-error e])))
          (let [actions (cond-> with-actions
                          ; what about local-basis? why not specify branch?
                          route (conj [:partition-route nil route]))]
            (hydrate-partition rt branch actions dispatch! get-state)))))))

(defn open-popover [branch popover-id]
  [:open-popover branch popover-id])

(defn stage-popover [rt invert-route branch swap-fn-async & on-start] ; todo rewrite in terms of with-groups
  (fn [dispatch! get-state]
    (p/then (swap-fn-async (get-in (get-state) [::runtime/partitions branch :stage] {}))
            (fn [{:keys [tx app-route]}]
              (let [with-actions (mapv (fn [[uri tx]]
                                         (let [tx (update-to-tempids get-state branch uri tx)]
                                           [:with branch uri tx]))
                                       tx)
                    parent-branch (branch/decode-parent-branch branch)]
                ; should the tx fn not be withd? if the transact! fails, do we want to run it again?
                (dispatch! (apply batch (concat with-actions on-start)))
                ;(with-groups rt invert-route parent-branch tx-groups :route app-route :post-tx nil)
                (let [tx-groups (->> (get-in (get-state) [::runtime/partitions branch :stage])
                                     (filter (fn [[uri tx]] (and (should-transact!? uri get-state) (not (empty? tx)))))
                                     (into {}))]
                  (if (and (nil? parent-branch) (not (empty? tx-groups)))
                    ; todo what if transact throws?
                    (transact! rt invert-route tx-groups dispatch! get-state
                               :post-tx (let [clear-uris (->> (keys tx-groups)
                                                              (map (fn [uri] [:reset-stage-uri branch uri nil]))
                                                              vec)]
                                          (concat clear-uris ; clear the uris that were transacted
                                                  [[:merge branch] ; merge the untransacted uris up
                                                   (discard-partition branch)])) ; clean up the partition
                               :route app-route)
                    (let [e (some-> app-route router/invalid-route?)
                          actions [[:merge branch]
                                   (cond
                                     e [:set-error e]
                                     app-route [:partition-route nil app-route] ; what about local-basis? why not specify branch?
                                     :else nil)
                                   (discard-partition branch)]]
                      (if e
                        (dispatch! (apply batch actions))
                        (hydrate-partition rt parent-branch actions dispatch! get-state))))))))))

(defn reset-stage-uri [rt branch uri tx]
  (fn [dispatch! get-state]
    ; check if auto-tx is OFF first?
    (when (not= tx (get-in (get-state) [::runtime/partitions branch :stage uri]))
      (hydrate-partition rt nil [[:reset-stage-uri branch uri tx]] dispatch! get-state))))

(defn manual-transact-uri! [peer invert-route nil-branch-aux uri]
  (fn [dispatch! get-state]
    ; todo do something when child branches exist and are not nil: hyperfiddle/hyperfiddle#99
    ; can only transact one branch
    (let [tx-groups (-> (get-in (get-state) [::runtime/partitions nil :stage])
                        (select-keys [uri]))]
      (transact! peer invert-route tx-groups dispatch! get-state :post-tx [[:reset-stage-uri nil uri]]))))
