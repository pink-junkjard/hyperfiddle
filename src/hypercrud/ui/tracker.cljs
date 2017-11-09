(ns hypercrud.ui.tracker)


(defn tracker [c & args]
  (js/console.log "tracker:mount")
  (let [state (atom nil)
        f (fn x [path v]
            (cond
              (map? v)
              (doseq [[k v] v]
                (x (conj path k) v))

              (or (seq? v) (vector? v))
              (doseq [[i v] (map-indexed (fn [i v] [i v]) v)]
                (x (conj path i) v))

              :else (swap! state update-in path
                           (fn [o]
                             (when-not (= o v)
                               (js/console.log (str (pr-str path) ":\n\told:" (pr-str o) "\n\tnew:" (pr-str v))))
                             v))))]
    (fn [c & args]
      (js/console.log "tracker:render")
      (f [] args)
      ; return comp
      (into [c] args))))
