(ns hyperfiddle.io.legacy)


(defn stage->staged-branches [stage-val]
  (->> stage-val
       (mapcat (fn [[branch-ident branch-content]]
                 (->> branch-content
                      (map (fn [[dbname tx]]
                             {:branch-ident branch-ident
                              :dbname dbname
                              :tx tx})))))))
