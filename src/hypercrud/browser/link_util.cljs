(ns hypercrud.browser.link-util)                            ; Just to break dependency cycles


(defn link-type [link] ; system, blank
  ; todo kill me
  (condp = (:request/type link)
    :entity :link-entity
    :query :link-query
    :link-blank))
