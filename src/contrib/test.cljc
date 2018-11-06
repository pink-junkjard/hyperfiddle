(ns contrib.test
  (:require
    [clojure.spec.alpha :as s]))


; Modified from https://github.com/cognitect-labs/transcriptor/blob/master/transcriptor.clj
(defmacro test! [doc form spec]
  `(let [v# ~form]
     (when-not (s/valid? ~spec v#)
       (let [ed# (s/explain-data ~spec v#)
             err# (ex-info (str "\n" ~doc "\n" (with-out-str (s/explain-out ed#)))
                           ed#)]
         (throw err#)))))

(comment
  ; Example

  (ns footest
    (:require
      [clojure.spec.alpha :as s]
      [contrib.test :refer [test!]]))


  (test!
    "inc"
    (do (inc 1))
    #{2})

  (test!
    "inc2"
    (do (inc 1))
    #{3})

  )
