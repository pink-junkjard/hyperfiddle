(ns contrib.reducers)


(defn combine-reducers [reducer-map]
  (fn [value action & args]
    (reduce (fn [state [k reducer]]
              (update state k #(apply reducer % action args)))
            value
            reducer-map)))

(defn dispatch! [state-atom root-reducer action]
  (let [[action & args] action]
    (if (= :batch action)
      (swap! state-atom (fn [state]
                          (reduce (fn [state [action & args]]
                                    (apply root-reducer state action args))
                                  state
                                  args)))
      (swap! state-atom #(apply root-reducer % action args))))
  nil)
