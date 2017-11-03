(ns hypercrud.browser.context-util)


; deprecated. just invoke the body
(defn ident->database-uri [ident ctx]
  (get-in ctx [:repository :repository/environment ident]))
