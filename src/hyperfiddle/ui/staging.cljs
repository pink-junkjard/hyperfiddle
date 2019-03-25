(ns hyperfiddle.ui.staging
  (:require
    [cats.monad.either :as either]
    [clojure.string :as string]
    [contrib.css :refer [css]]
    [contrib.pprint :refer [pprint-datoms-str]]
    [contrib.reactive :as r]
    [contrib.reader :refer [read-edn-string!]]
    [contrib.hfrecom :refer [anchor-tabs]]
    [contrib.ui :refer [code debounced easy-checkbox validated-cmp]]
    [contrib.ui.tooltip :refer [tooltip]]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.security.client :as security]
    [re-com.core :as re-com]
    [re-com.tabs]))


(defn- default-tab-model [selected-dbname tab-ids]
  (if (contains? (set tab-ids) selected-dbname)
    selected-dbname
    (first tab-ids)))


(defn- anonymous? [rt] (nil? @(runtime/state rt [::runtime/user-id])))

(defn- writes-allowed?+ [rt selected-dbname]
  (let [hf-db (domain/database (runtime/domain rt) selected-dbname)
        subject @(runtime/state rt [::runtime/user-id])]
    (security/subject-can-transact? hf-db subject)))

(defn transact-button [rt branch-id selected-dbname-ref label]
  (let [selected-dbname @selected-dbname-ref
        writes-allowed?+ (writes-allowed?+ rt selected-dbname)
        stage (runtime/state rt [::runtime/partitions branch-id :stage selected-dbname])]
    [tooltip (either/branch
               writes-allowed?+
               (fn [e] {:status :warning :label "Misconfigured db security"})
               (fn [writes-allowed?]
                 (cond (and (not writes-allowed?) (anonymous? rt)) {:status :warning :label "Please login"}
                       (not writes-allowed?) {:status :warning :label "Writes restricted"}
                       (empty? @stage) {:status :warning :label "no changes"})))
     (let [color (domain/database-color (runtime/domain rt) selected-dbname)
           is-disabled (either/branch writes-allowed?+ (constantly true)
                                      (fn [writes-allowed?]
                                        (or (not writes-allowed?) (empty? @stage))))]
       [:button {:disabled is-disabled
                 :class (css "btn btn-sm hf-btn-xs" (if is-disabled "btn-outline-secondary" "btn-secondary"))
                 :style (cond-> {(if is-disabled :color :background-color) color
                                 :border-color color}
                                is-disabled (assoc :pointer-events "none"))
                 :on-click (fn []
                             (let [action (actions/manual-transact-db! rt branch-id selected-dbname)]
                               (runtime/dispatch! rt action)))}
        (str label " transact!")])]))

(defn auto-transact-control [rt branch-id selected-dbname-ref]
  (let [selected-dbname @selected-dbname-ref
        writes-allowed?+ (writes-allowed?+ rt selected-dbname)
        stage (runtime/state rt [::runtime/partitions branch-id :stage selected-dbname])]
    [tooltip (either/branch
               writes-allowed?+
               (fn [e] {:status :warning :label "Misconfigured db security"})
               (fn [writes-allowed?]
                 (cond (and (anonymous? rt) (not writes-allowed?)) {:status :warning :label "Please login"}
                       (not writes-allowed?) {:status :warning :label "Writes restricted"}
                       (not (empty? @stage)) {:status :warning :label "please transact! all changes first"})))
     (let [is-disabled (either/branch
                         writes-allowed?+
                         (constantly true)
                         (fn [writes-allowed?]
                           (or (not writes-allowed?) (not (empty? @stage)))))
           is-auto-transact @(runtime/state rt [::runtime/auto-transact selected-dbname])]
       [easy-checkbox {:disabled is-disabled
                       :style (cond-> {:margin-left "0.5em"}
                                is-disabled (assoc :pointer-events "none"))
                       :checked (boolean is-auto-transact)
                       :on-change #(runtime/dispatch! rt [:toggle-auto-transact selected-dbname])}
        " auto-transact"])]))

(let [parse-string (fn [s]
                     (let [v (read-edn-string! s)]
                       (assert (and (or (nil? v) (vector? v) (seq? v))
                                    (every? (fn [v] (or (map? v) (vector? v) (seq? v))) v)))
                       (some-> v reverse vec)))
      to-string (fn [v] (pprint-datoms-str (some-> v reverse)))
      on-change (fn [rt branch dbname-ref o n]
                  (runtime/dispatch! rt (actions/reset-stage-db rt branch @dbname-ref n)))]
  (defn- tab-content [rt branch dbname-ref & children]
    (into [:div.hyperfiddle-stage-content
           {:style {:border-color (domain/database-color (runtime/domain rt) @dbname-ref)}}
           children
           (let [props {:value @(runtime/state rt [::runtime/partitions branch :stage @dbname-ref])
                        :readOnly @(runtime/state rt [::runtime/auto-transact @dbname-ref])
                        :on-change (r/partial on-change rt branch dbname-ref)
                        :lineNumbers false}]
             ^{:key (str @dbname-ref)}
             [debounced props validated-cmp parse-string to-string code])]
          )))

(defn default-dbname-labels [rt]
  (->> (runtime/domain rt) domain/databases keys sort
       (map (fn [%] {:id % :label (domain/dbname-label %)}))))

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

(defn ^:export popover-button [rt branch dbname-labels & {:keys [show-auto-tx]}]
  (let [show-tooltip (r/atom false)
        show-stage (r/atom false)
        selected-dbname (runtime/state rt [:staging/selected-uri])]
    (fn [rt branch dbname-labels & {:keys [show-auto-tx]}]
      (let [maybe-tooltip-wrapper (fn [button-cmp]
                                    (if show-auto-tx
                                      [re-com/popover-tooltip
                                       :showing? (r/atom (and @show-tooltip (not @show-stage)))
                                       :label [tooltip-content rt dbname-labels]
                                       :anchor button-cmp]
                                      button-cmp))]
        [:div.hyperfiddle-staging-popover-button
         (maybe-tooltip-wrapper
           [re-com/popover-anchor-wrapper
            :showing? show-stage
            :position :below-center
            :anchor (let [stage-is-dirty (not @(r/fmap empty? (runtime/state rt [::runtime/partitions branch :stage])))]
                      [:button (cond-> {:on-click #(reset! show-stage true)
                                        :class (cond-> "hyperfiddle btn-default"
                                                 stage-is-dirty (css "stage-dirty"))}
                                 show-auto-tx (assoc :on-mouse-over #(do (reset! show-tooltip true) nil)
                                                     :on-mouse-out #(do (reset! show-tooltip false) nil)))
                       "stage▾"])
            :popover [re-com/popover-content-wrapper
                      :no-clip? true?
                      :body (let [selected-dbname' (r/fmap-> selected-dbname (default-tab-model (mapv :id dbname-labels)))]
                              [:div.hyperfiddle-popover-body
                               [editor-cmp selected-dbname rt branch dbname-labels
                                [transact-button rt branch selected-dbname']
                                (when show-auto-tx
                                  [auto-transact-control rt branch selected-dbname'])
                                [:button.close-popover {:on-click (r/partial reset! show-stage false)} "close"]]])]])]))))

(defn dirty-dbs [rt branch]
  (->> @(runtime/state rt [::runtime/partitions branch :stage])
       (remove (comp empty? second))
       (map first)
       set))

(defn inline-stage "ctx needs :peer and :branch only"
  ([ctx] (inline-stage ctx (default-dbname-labels (:peer ctx))))
  ([ctx dbname-labels]
   (let [rt (:peer ctx) branch (:branch ctx)
         tabs-definition (mapv (fn [{:keys [id] s-label :label}]
                                 {:id id
                                  :href "#"
                                  :label [:span
                                          {:style {:border-color (domain/database-color (runtime/domain rt) id)}
                                           :class (when (contains? (dirty-dbs rt branch) id) "stage-dirty")}
                                          s-label]})
                               dbname-labels)
         selected-dbname (runtime/state rt [:staging/selected-uri])
         selected-dbname' (r/fmap-> selected-dbname (default-tab-model (mapv :id dbname-labels)))
         label (->> dbname-labels (some #(when (= (:id %) @selected-dbname') (:label %))))]
     [:div.hyperfiddle-staging-editor-cmp
      [tab-content rt branch selected-dbname'
       [:div.hyperfiddle-stage-actions {:style {:display "flex"}}
        [:div {:style {:margin-left "auto"}} "stage: "
         [anchor-tabs
          :model selected-dbname'
          :tabs tabs-definition
          :on-change (r/partial reset! selected-dbname)]
         [transact-button rt branch selected-dbname' label]]]]])))
