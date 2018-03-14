(ns hyperfiddle.ide.fiddles.topnav-bindings)


(defn bindings [ctx]
  (assoc ctx
    :cell (fn [control field ctx]
            (let [rtype (-> ctx :cell-data deref :fiddle/type)
                  visible (case (:attribute field)
                            :fiddle/name false
                            :fiddle/query (= rtype :query)
                            :fiddle/pull (= rtype :entity)
                            true)]
              (if visible
                ; Omit form-cell, we don't want any cell markup at all.
                [control field {} ctx])))
    ;:fields {:fiddle/query {:renderer (fn [field links props ctx]
    ;                                    [code field links {:minHeight 100} ctx])}}
    ;:label (fn [field links ctx] nil)
    ))
