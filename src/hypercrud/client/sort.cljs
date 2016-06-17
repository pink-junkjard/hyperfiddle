(ns hypercrud.client.sort
  (:require [hypercrud.client.core :as hc]
            [promesa.core :as p]))


;; flatmap resolved promises in the same stack frame instead of next tick
(defn immediate-then [promise promise-fn]
  (if (p/resolved? promise)
    (promise-fn (p/extract promise))
    (p/then promise promise-fn)))


(defn immediate-map [pf p]
  (immediate-then p pf))


(defn immediate-all [promises]
  (if (every? p/resolved? promises)
    (p/resolved (map p/extract promises))
    (p/all promises)))


(defn hc-sort-by [client eids keyfns]
  (->> eids
       (map (fn [eid]
              (->> keyfns
                   (reduce immediate-then (hc/entity* client eid))
                   (immediate-map (fn [sortval] (p/resolved [eid sortval]))))))
       (immediate-all)
       (immediate-map (fn [pairs] (p/resolved (->> pairs
                                                   (sort-by second)
                                                   (map first)))))))

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
