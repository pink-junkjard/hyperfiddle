(ns hypercrud.state.core)


(defn combine-reducers [reducer-map]
  (fn [value action & args]
    (reduce (fn [state [k reducer]]
              (update state k #(apply reducer % action args)))
            value
            reducer-map)))

(defn build-dispatch [state-atom root-reducer peer]
  (let [get-state (fn [] @state-atom)]
    (fn dispatch! [action-or-func]
      (if (goog/isFunction action-or-func)
        (action-or-func dispatch! get-state peer)

        (let [[action & args] action-or-func]
          (if (= :batch action)
            (swap! state-atom (fn [state]
                                (reduce (fn [state [action & args]]
                                          ; todo what if action = func?
                                          (apply root-reducer state action args))
                                        state
                                        args)))
            (swap! state-atom #(apply root-reducer % action args)))))
      nil)))
