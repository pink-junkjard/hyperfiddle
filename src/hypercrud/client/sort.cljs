(ns hypercrud.client.sort
  (:require [hypercrud.client.core :as hc]
            [promesa.core :as p]))


(defn hc-sort-by [client eids keyfns]
  (->> eids
       (map (fn [eid]
              (->> keyfns
                   (reduce p/then (hc/entity* client eid))
                   (p/map (fn [sortval] [eid sortval])))))
       (p/all)
       (p/map (fn [pairs] (->> pairs
                               (sort-by second)
                               (map first))))))

(comment
  ;; scratch work for sort-by

  [hcr/promised
   (hc-sort-by client eids [#(hc/entity* client (:community/orgtype %))
                            #(p/resolved (:db/ident %))])

   (fn [eids]
     [cj-grid client forms eids])]

  (cats/mlet [community (hc/entity* client eid)
              neighborhood (hc/entity* client (:community/neighborhood community))]
             [community neighborhood])

  (p/then (hc/entity* client eid)
          (fn [community]
            (p/then (hc/entity* client (:community/neighborhood community))
                    (fn [neighborhood]
                      [community neighborhood])))))
