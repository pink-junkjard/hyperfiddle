(ns hyperfiddle.state)


(defn combine-reducers [reducer-map]
  (fn [value action & args]
    (reduce (fn [state [k reducer]]
              (update state k #(apply reducer % action args)))
            value
            reducer-map)))

(defn build-dispatch [state-atom root-reducer]
  (let [get-state (fn [] @state-atom)]
    (fn dispatch! [action-or-func]
      (if #?(:clj  (fn? action-or-func)
             :cljs (goog/isFunction action-or-func))
        (action-or-func dispatch! get-state)

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
