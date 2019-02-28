(ns hyperfiddle.ui.staging
  (:require
    [cats.monad.either :as either]
    [clojure.string :as string]
    [contrib.css :refer [css]]
    [contrib.pprint :refer [pprint-datoms-str]]
    [contrib.reactive :as r]
    [contrib.reader :refer [read-edn-string!]]
    [contrib.ui :refer [code debounced easy-checkbox validated-cmp]]
    [contrib.ui.tooltip :refer [tooltip]]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.branch :as branch]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.security.client :as security]
    [re-com.core :as re-com]
    [re-com.tabs]))


(defn- default-tab-model [selected-dbname tab-ids]
  (if (contains? (set tab-ids) selected-dbname)
    selected-dbname
    (first tab-ids)))

(defn ^:export stage-ui-buttons [selected-dbname-ref rt branch dbname-labels]
  (let [selected-dbname (default-tab-model @selected-dbname-ref (map :id dbname-labels))
        stage (runtime/state rt [::runtime/partitions branch :stage selected-dbname])
        writes-allowed?+ (let [hf-db (domain/database (runtime/domain rt) selected-dbname)
                               subject @(runtime/state rt [::runtime/user-id])]
                           (security/subject-can-transact? hf-db subject))
        anonymous? (nil? @(runtime/state rt [::runtime/user-id]))]
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
        (when (branch/root-branch? branch)
          [:button {:disabled disabled?
                    :style (cond-> {:background-color (domain/database-color (runtime/domain rt) selected-dbname)}
                             disabled? (assoc :pointer-events "none"))
                    :on-click (fn []
                                (let [action (actions/manual-transact-db! rt branch selected-dbname)]
                                  (runtime/dispatch! rt action)))}
           "transact!"]))]
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
            is-auto-transact @(runtime/state rt [::runtime/auto-transact selected-dbname])]
        [easy-checkbox {:disabled is-disabled
                        :style (if is-disabled {:pointer-events "none"})
                        :checked (boolean is-auto-transact)
                        :on-change #(runtime/dispatch! rt [:toggle-auto-transact selected-dbname])}
         "auto-transact"])]]))

(let [parse-string (fn [s]
                     (let [v (read-edn-string! s)]
                       (assert (and (or (nil? v) (vector? v) (seq? v))
                                    (every? (fn [v] (or (map? v) (vector? v) (seq? v))) v)))
                       v))
      to-string pprint-datoms-str
      on-change (fn [rt branch dbname-ref o n]
                  (runtime/dispatch! rt (actions/reset-stage-db rt branch @dbname-ref n)))]
  (defn- tab-content [rt branch dbname-ref & children]
    (into [:div.tab-content {:style {:border-color (domain/database-color (runtime/domain rt) @dbname-ref)}}
           (let [props {:value @(runtime/state rt [::runtime/partitions branch :stage @dbname-ref])
                        :readOnly @(runtime/state rt [::runtime/auto-transact @dbname-ref])
                        :on-change (r/partial on-change rt branch dbname-ref)}]
             ^{:key (str @dbname-ref)}
             [debounced props validated-cmp parse-string to-string code])]
          children)))

(defn default-dbname-labels [rt]
  (->> (runtime/domain rt) domain/databases keys sort
       (map (fn [%] {:id % :label %}))))

(defn ^:export editor-cmp
  ([selected-dbname ctx]
   [editor-cmp selected-dbname (:peer ctx) (:branch ctx) (default-dbname-labels (:peer ctx))])
  ([selected-dbname rt branch dbname-labels & children]
   (let [dirty-dbs (->> @(runtime/state rt [::runtime/partitions branch :stage])
                        (remove (comp empty? second))
                        (map first)
                        set)
         tabs-definition (mapv (fn [{:keys [id] s-label :label}]
                                 {:id id
                                  :label [:span
                                          {:style {:border-color (domain/database-color (runtime/domain rt) id)}
                                           :class (when (contains? dirty-dbs id) "stage-dirty")}
                                          s-label]})
                               dbname-labels)
         selected-dbname' (r/fmap-> selected-dbname (default-tab-model (mapv :id dbname-labels)))]
     [:div.hyperfiddle-staging-editor-cmp
      [re-com.tabs/horizontal-tabs
       :model selected-dbname'
       :tabs tabs-definition
       :on-change (r/partial reset! selected-dbname)]
      (into [tab-content rt branch selected-dbname'] children)])))

(defn- tooltip-content [rt dbname-labels]
  [:div {:style {:text-align "left"}}
   [hyperfiddle.ui/markdown
    (->> (sort-by :label dbname-labels)
         (mapv (fn [{:keys [id] s-label :label}]
                 (let [prefix (if @(runtime/state rt [::runtime/auto-transact id])
                                "- [x] "
                                "- [ ] ")]
                   (str prefix s-label))))
         (string/join "\n")
         (str "##### Auto-transact:\n\n"))
    {:hyperfiddle.ui.markdown-extensions/unp true}]])

(defn ^:export popover-button [rt branch dbname-labels]
  (let [show-tooltip (r/atom false)
        show-stage (r/atom false)
        selected-dbname (runtime/state rt [:staging/selected-uri])]
    (fn [rt branch dbname-labels]
      [:div.hyperfiddle-staging-popover-button
       [re-com/popover-tooltip
        :showing? (r/atom (and @show-tooltip (not @show-stage)))
        :label [tooltip-content rt dbname-labels]
        :anchor [re-com/popover-anchor-wrapper
                 :showing? show-stage
                 :position :below-center
                 :anchor (let [stage-is-dirty (not @(r/fmap empty? (runtime/state rt [::runtime/partitions branch :stage])))]
                           [:button {:on-click #(reset! show-stage true)
                                     :on-mouse-over #(do (reset! show-tooltip true) nil)
                                     :on-mouse-out #(do (reset! show-tooltip false) nil)
                                     :class (cond-> "hyperfiddle btn-default"
                                              stage-is-dirty (css "stage-dirty"))}
                            "stage▾"])
                 :popover [re-com/popover-content-wrapper
                           :no-clip? true?
                           :body [:div.hyperfiddle-popover-body
                                  [editor-cmp selected-dbname rt branch dbname-labels
                                   [stage-ui-buttons selected-dbname rt branch dbname-labels]
                                   [:button.close-popover {:on-click (r/partial reset! show-stage false)} "close"]]]]]]])))
