(ns hyperfiddle.ide.fiddles.topnav-bindings)


(defn bindings [ctx]
  (assoc ctx
    :uri (get-in ctx [:repository :repository/environment "$"]) ; branched index link needs explicit conn-id
    :cell (fn [control field links ctx]
            (let [rtype (get-in ctx [:cell-data :fiddle/type])
                  visible (case (:attribute field) #_(get-in ctx [:attribute :db/ident])
                            :fiddle/name false
                            :fiddle/query (= rtype :query)
                            :fiddle/pull (= rtype :entity)
                            true)]
              (if visible
                ; Omit form-cell, we don't want any cell markup at all.
                [control field links {} ctx])))
    ;:fields {:fiddle/query {:renderer (fn [field links props ctx]
    ;                                    [code field links {:minHeight 100} ctx])}}
    ;:label (fn [field links ctx] nil)
    ))
