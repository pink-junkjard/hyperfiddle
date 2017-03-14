(ns hypercrud.browser.link-util) ; Just to break dependency cycles


(defn link-type [link]                                      ; system, blank
  (let [link-key-namespaces (->> (keys (:link/request link)) (mapv namespace) set)]
    (cond
      (contains? link-key-namespaces "link-query") :link-query
      (contains? link-key-namespaces "link-entity") :link-entity
      ; system links are entity links in practice
      ;(system-links/system-link? (:db/id link)) :link-system
      :else :link-blank)))
