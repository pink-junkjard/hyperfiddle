(ns contrib.timbre-util
  (:require
    [taoensso.encore :as encore]
    [taoensso.timbre :as timbre]))


(defn ns-filter [fltr] (-> fltr encore/compile-ns-filter encore/memoize_))

; Adapted from https://github.com/ptaoussanis/timbre/issues/208#issuecomment-269008644
(defn ns-patterns-mw [ns-patterns]
  (fn [{:keys [?ns-str config level] :as opts}]
    (let [namesp (or (some->> ns-patterns
                              keys
                              (filter #(and (string? %)
                                            ((ns-filter %) ?ns-str)))
                              not-empty
                              (apply max-key count))
                     :all)
          loglevel (get ns-patterns namesp (get config :level))]
      (when (timbre/level>= level loglevel)
        opts))))
