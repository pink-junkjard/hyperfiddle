(ns hypercrud.state.actions.util)


(defn confirm [message]
  #?(:clj  (throw (ex-info "confirm unsupported by platform" nil))
     :cljs (js/confirm message)))


(defn navigable? [route {:keys [encoded-route branches] :as state}]
  (and (not= route encoded-route)
       (or (empty? branches)
           (confirm "Unstaged work will be lost on navigate, are you sure?"))))
