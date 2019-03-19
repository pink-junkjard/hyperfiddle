(ns contrib.timbre-util
  (:require
    [taoensso.encore :as encore]
    [taoensso.timbre :as timbre]))


; Adapted from https://github.com/ptaoussanis/timbre/issues/208#issuecomment-269008644
(defn ns-patterns-mw [ns-patterns]
  (let [filters (->> (keys ns-patterns)
                     (filter string?)
                     (sort-by count) reverse                ; longest ns pattern wins
                     (map (juxt identity encore/compile-ns-filter))
                     doall)
        ns-str->log-level (encore/memoize_
                            (fn [?ns-str]
                              (let [namesp (-> (some (fn [[namesp f]] (when (f ?ns-str) namesp)) filters)
                                               (or :all))]
                                (get ns-patterns namesp))))]
    (fn [{:keys [?ns-str config level] :as opts}]
      (let [loglevel (or (ns-str->log-level ?ns-str) (get config :level))]
        (when (timbre/level>= level loglevel)
          opts)))))
