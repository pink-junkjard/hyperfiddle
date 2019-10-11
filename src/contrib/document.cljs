(ns contrib.document)


(defn document-location! "get the root-relative URL as string for routing
  Getting this as a string is useful for unifying portable backend/frontend routing code"
  []
  (str js/document.location.pathname
       js/document.location.search
       js/document.location.hash))
