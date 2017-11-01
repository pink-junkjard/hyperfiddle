(ns hypercrud.state.actions.util)


(defn navigable? [route {:keys [encoded-route popovers] :as state}]
  (and (not= route encoded-route)
       (or (empty? popovers)
           (js/confirm "Unstaged work will be lost on navigate, are you sure?"))))
