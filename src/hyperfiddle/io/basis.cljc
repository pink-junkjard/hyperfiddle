(ns hyperfiddle.io.basis)


(def ERROR-MISMATCHED-DBNAMES "Bases cannot be compared; mismatched dbnames")
(def ERROR-BOTH-GREATER-AND-LESS-THAN "Bases cannot be compared; different values are > and <")

(defn compare-uri-maps [x y]
  (cond
    (identical? x y) 0
    (nil? x) -1
    (nil? y) 1
    :else (if-not (= (set (keys x)) (set (keys y)))
            (throw (ex-info ERROR-MISMATCHED-DBNAMES {:x x :y y}))
            (reduce
              (fn [acc [xk xv]]
                (let [r (compare xv (get y xk))]
                  (cond
                    (= 0 acc) r
                    (= 0 r) acc
                    (not= acc r) (throw (ex-info ERROR-BOTH-GREATER-AND-LESS-THAN {:x x :y y}))
                    :else acc)))
              0
              x))))
