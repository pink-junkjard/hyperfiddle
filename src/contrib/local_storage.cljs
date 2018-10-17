(ns contrib.local-storage
  (:require
    [hypercrud.transit :as transit]))


(def ^:private -transit-encoding :json-verbose)

(defn get-item
  ([k]
   (get-item js/localStorage k))
  ([storage k]
   (-> (.getItem storage (transit/encode k :type -transit-encoding))
       (transit/decode :type -transit-encoding))))

(defn set-item!
  ([k v]
   (set-item! js/localStorage k v))
  ([storage k v]
   (->> (transit/encode v :type -transit-encoding)
        (.setItem storage (transit/encode k :type -transit-encoding)))))

(defn remove-item!
  ([k]
   (remove-item! js/localStorage k))
  ([storage k]
   (.removeItem storage (transit/encode k :type -transit-encoding))))

(defn clear!
  ([]
   (clear! js/localStorage))
  ([storage]
   (.clear storage)))

; https://developer.mozilla.org/en-US/docs/Web/API/Web_Storage_API/Using_the_Web_Storage_API#Feature-detecting_localStorage
(def is-supported
  (try
    (and (exists? js/localStorage)
         (let [x "__storage_test__"]
           (set-item! x x)
           (remove-item! x)
           true))
    (catch :default e
      (and (instance? js/DOMException e)
           (or
             ; everything except Firefox
             (= (.-code e) 22)
             ; Firefox
             (= (.-code e) 1014)
             ; test name field too, because code might not be present
             ; everything except Firefox
             (= (.-name e) "QuotaExceededError")
             ; Firefox
             (= (.-name e) "NS_ERROR_DOM_QUOTA_REACHED"))
           ; acknowledge QuotaExceededError only if there's something already stored
           (not= (.-length js/localStorage) 0)))))
