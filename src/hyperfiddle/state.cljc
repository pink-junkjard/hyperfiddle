(ns hyperfiddle.state
  (:require
    [taoensso.timbre :as timbre]))


(defn combine-reducers [reducer-map]
  (fn [value action & args]
    (reduce (fn [state [k reducer]]
              (update state k #(apply reducer % action args)))
            value
            reducer-map)))

(defn dispatch! [state-atom root-reducer action-or-func & [f!]]
  (if #?(:clj  (fn? action-or-func)
         :cljs (goog/isFunction action-or-func))
    (action-or-func #(dispatch! state-atom root-reducer % f!) (fn [] @state-atom))

    (let [[action & args] action-or-func]
      (if (= :batch action)
        (do
          (timbre/debug "dispatch!" :batch (pr-str (mapv first args)))
          (swap! state-atom (fn [state]
                              (reduce (fn [state [action & args]]
                                        (if f! (apply f! action args))
                                        ; todo what if action = func?
                                        (apply root-reducer state action args))
                                      state
                                      args))))
        (do
          (timbre/debug "dispatch!" action)
          (if f! (apply f! action args))
          (swap! state-atom #(apply root-reducer % action args))))))
  nil)
