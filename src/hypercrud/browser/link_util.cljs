(ns hypercrud.browser.link-util)                            ; Just to break dependency cycles


(defn link-type [link]                                      ; system, blank
  (if (= (-> link :link/request :request/type) :entity)
    :link-entity
    (let [link-key-namespaces (->> (keys (:link/request link)) (mapv namespace) set)]
      (if (contains? link-key-namespaces "link-query")
        :link-query
        :link-blank))))
