(ns hyperfiddle.ui.staging
  (:require
    [cats.monad.either :as either]
    [contrib.pprint :refer [pprint-datoms-str]]
    [contrib.reactive :as r]
    [contrib.reader :refer [read-edn-string!]]
    [contrib.ui :refer [code debounced easy-checkbox validated-cmp]]
    [contrib.ui.tooltip :refer [tooltip]]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.security.client :as security]
    [re-com.tabs]))


(letfn [(toggle-auto-transact! [ctx selected-dbname]
          (runtime/dispatch! (:peer ctx) [:toggle-auto-transact @selected-dbname]))]
  (defn ^:export stage-ui-buttons [domain selected-dbname stage ctx]
    (let [writes-allowed?+ (let [hf-db (domain/database domain @selected-dbname)
                                 subject @(runtime/state (:peer ctx) [::runtime/user-id])]
                             (security/subject-can-transact? hf-db subject))
          anonymous? (nil? @(runtime/state (:peer ctx) [::runtime/user-id]))]
      [:<>
       [tooltip (either/branch
                  writes-allowed?+
                  (fn [e] {:status :warning :label "Misconfigured db security"})
                  (fn [writes-allowed?]
                    (cond (and (not writes-allowed?) anonymous?) {:status :warning :label "Please login"}
                          (not writes-allowed?) {:status :warning :label "Writes restricted"}
                          (empty? @stage) {:status :warning :label "no changes"})))
        (let [disabled? (either/branch
                          writes-allowed?+
                          (constantly true)
                          (fn [writes-allowed?] (or (not writes-allowed?) (empty? @stage))))]
          [:button {:disabled disabled?
                    :style (if disabled? {:pointer-events "none"})
                    :on-click (fn []
                                (let [action (actions/manual-transact-db! (:peer ctx) @selected-dbname)]
                                  (runtime/dispatch! (:peer ctx) action)))}
           "transact!"])]
       " "
       [tooltip (either/branch
                  writes-allowed?+
                  (fn [e] {:status :warning :label "Misconfigured db security"})
                  (fn [writes-allowed?]
                    (cond (and anonymous? (not writes-allowed?)) {:status :warning :label "Please login"}
                          (not writes-allowed?) {:status :warning :label "Writes restricted"}
                          (not (empty? @stage)) {:status :warning :label "please transact! all changes first"})))
        (let [is-disabled (either/branch
                            writes-allowed?+
                            (constantly true)
                            (fn [writes-allowed?]
                              (or (not writes-allowed?) (not (empty? @stage)))))
              is-auto-transact @(runtime/state (:peer ctx) [::runtime/auto-transact @selected-dbname])]
          [easy-checkbox {:disabled is-disabled
                          :style (if is-disabled {:pointer-events "none"})
                          :checked (boolean is-auto-transact)
                          :on-change (r/partial toggle-auto-transact! ctx selected-dbname)}
           "auto-transact"])]])))

(let [parse-string (fn [s]
                     (let [v (read-edn-string! s)]
                       (assert (and (or (nil? v) (vector? v) (seq? v))
                                    (every? (fn [v] (or (map? v) (vector? v) (seq? v))) v)))
                       v))
      to-string pprint-datoms-str
      on-change (fn [peer branch dbname-ref o n]
                  (runtime/dispatch! peer (actions/reset-stage-db peer branch @dbname-ref n)))]
  (defn staging-control [ctx dbname-ref]
    (let [props {:value @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :stage @dbname-ref])
                 :readOnly @(runtime/state (:peer ctx) [::runtime/auto-transact @dbname-ref])
                 :on-change (r/partial on-change (:peer ctx) (:branch ctx) dbname-ref)}]
      ^{:key (str @dbname-ref)}
      [debounced props validated-cmp parse-string to-string code])))

(defn- default-tab-model [selected-dbname tab-ids]
  (if (contains? (set tab-ids) selected-dbname)
    selected-dbname
    (first tab-ids)))

(defn ^:export cmp [domain selected-dbname ctx & [child]]
  (let [dirty-dbs (->> @(runtime/state (:peer ctx) [::runtime/partitions nil :stage])
                       (remove (comp empty? second))
                       (map first)
                       set)
        tabs-definition (->> (domain/databases domain)
                             keys
                             sort
                             (mapv (fn [dbname]
                                     {:id dbname
                                      :label [:span {:class (when (contains? dirty-dbs dbname) "stage-dirty")} dbname]})))
        stage (runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :stage @selected-dbname])]
    [:<>
     [re-com.tabs/horizontal-tabs
      :model (r/fmap-> selected-dbname (default-tab-model (mapv :id tabs-definition)))
      :tabs tabs-definition
      :on-change (r/partial reset! selected-dbname)]
     [staging-control ctx selected-dbname]
     (when child
       [child domain selected-dbname stage ctx])]))
