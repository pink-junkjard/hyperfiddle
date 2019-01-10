(ns hyperfiddle.ide.staging
  (:require
    [contrib.pprint :refer [pprint-datoms-str]]
    [contrib.reactive :as r]
    [contrib.reader :refer [read-edn-string!]]
    [contrib.ui :refer [code debounced validated-cmp]]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.runtime :as runtime]
    [re-com.tabs]
    [hyperfiddle.domain :as domain]))


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
      [debounced props validated-cmp parse-string to-string code])))

(defn ^:export staging [ctx & [child]]
  (let [selected-dbname (runtime/state (:peer ctx) [:staging/selected-uri]) ; this is only writable because the browser's state impl uses reagent cursors all the way to the top
        change-tab #(reset! selected-dbname %)]
    (fn [ctx & [child]]
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
                                          :label [:span {:class (when (contains? dirty-dbs dbname) "stage-dirty")} dbname]})))
            _ (when-not (contains? (->> tabs-definition (map :id) (into #{})) @selected-dbname)
                (when-let [first-tab (:id (first tabs-definition))]
                  (reset! selected-dbname first-tab)))
            stage (runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :stage @selected-dbname])]
        [:<> {:key "topnav"}
         [re-com.tabs/horizontal-tabs
          :model selected-dbname
          :tabs tabs-definition
          :on-change change-tab]
         ^{:key (str @selected-dbname)}
         [staging-control ctx selected-dbname]
         (when child
           [child selected-dbname stage ctx])]))))
