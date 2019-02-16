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
    [hypercrud.ui.connection-color :refer [connection-color]]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.security.client :as security]
    [re-com.core :as re-com]
    [re-com.tabs]))


(letfn [(toggle-auto-transact! [ctx selected-dbname]
          (runtime/dispatch! (:peer ctx) [:toggle-auto-transact @selected-dbname]))]
  (defn ^:export stage-ui-buttons [selected-dbname ctx]
    (let [stage (runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :stage @selected-dbname])
          writes-allowed?+ (let [hf-db (domain/database (runtime/domain (:peer ctx)) @selected-dbname)
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
                    :style (cond-> {:background-color (connection-color @selected-dbname)}
                             disabled? (assoc :pointer-events "none"))
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

(defn ^:export editor-cmp [selected-dbname ctx & children]
  (let [dirty-dbs (->> @(runtime/state (:peer ctx) [::runtime/partitions nil :stage])
                       (remove (comp empty? second))
                       (map first)
                       set)
        tabs-definition (->> (runtime/domain (:peer ctx))
                             domain/databases
                             keys
                             sort
                             (mapv (fn [dbname]
                                     {:id dbname
                                      :label [:span
                                              {:style {:border-color (connection-color dbname)}
                                               :class (when (contains? dirty-dbs dbname) "stage-dirty")}
                                              dbname]})))]
    [:div.hyperfiddle-staging-editor-cmp
     [re-com.tabs/horizontal-tabs
      :model (r/fmap-> selected-dbname (default-tab-model (mapv :id tabs-definition)))
      :tabs tabs-definition
      :on-change (r/partial reset! selected-dbname)]
     (into [:div.tab-content {:style {:border-color (connection-color @selected-dbname)}}
            [staging-control ctx selected-dbname]]
           children)]))

(defn- tooltip-content [ctx]
  [:div {:style {:text-align "left"}}
   [hyperfiddle.ui/markdown
    (->> (runtime/domain (:peer ctx))
         domain/databases
         keys
         sort
         (map (fn [dbname]
                (let [prefix (if @(runtime/state (:peer ctx) [::runtime/auto-transact dbname])
                               "- [x] "
                               "- [ ] ")]
                  (str prefix dbname))))
         (string/join "\n")
         (str "##### Auto-transact:\n\n"))
    {:hyperfiddle.ui.markdown-extensions/unp true}]])

(defn ^:export popover-button [ctx]
  (let [show-tooltip (r/atom false)
        show-stage (r/atom false)
        selected-dbname (runtime/state (:peer ctx) [:staging/selected-uri])]
    (fn [ctx]
      [:div.hyperfiddle-staging-popover-button
       [re-com/popover-tooltip
        :showing? (r/atom (and @show-tooltip (not @show-stage)))
        :label [tooltip-content ctx]
        :anchor [re-com/popover-anchor-wrapper
                 :showing? show-stage
                 :position :below-center
                 :anchor (let [stage-is-dirty (not @(r/fmap empty? (runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :stage])))]
                           [:button {:on-click #(reset! show-stage true)
                                     :on-mouse-over #(do (reset! show-tooltip true) nil)
                                     :on-mouse-out #(do (reset! show-tooltip false) nil)
                                     :class (cond-> "hyperfiddle btn-default"
                                              stage-is-dirty (css "stage-dirty"))}
                            "stage▾"])
                 :popover [re-com/popover-content-wrapper
                           :no-clip? true?
                           :body [:div.hyperfiddle-popover-body
                                  [editor-cmp selected-dbname ctx
                                   [stage-ui-buttons selected-dbname ctx]
                                   [:button.close-popover {:on-click #(reset! show-stage false)} "close"]]]]]]])))
