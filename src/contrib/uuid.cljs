(ns contrib.uuid
  (:require
    [clojure.string :as string]
    [cuerdas.core :as str]))


(def uuid-regex-pattern #"^([0-9a-fA-F]{1,8})-([0-9a-fA-F]{1,4})-([0-9a-fA-F]{1,4})-([0-9a-fA-F]{1,4})-([0-9a-fA-F]{1,12})$")

; for consistency with java.net.UUID/fromString
; such that: (= (str (UUID/fromString s)) (str (read-uuid s)))
(defn read-uuid [s]
  (if-let [[_ & groups] (re-find uuid-regex-pattern s)]
    (->> (map string/lower-case groups)
         (map #(str/pad %2 {:length %1 :padding "0"}) [8 4 4 4 12])
         (string/join "-")
         (uuid))
    (throw (ex-info (str "Invalid UUID string: " s) {}))))
