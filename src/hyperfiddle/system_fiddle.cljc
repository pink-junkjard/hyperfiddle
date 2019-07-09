(ns hyperfiddle.system-fiddle
  (:require
    [contrib.try$ :refer [try-either]]))


(def decoding-error
  {:fiddle/ident :hyperfiddle.system/decoding-error
   :fiddle/type :blank
   :fiddle/renderer (str
                      '(let [[s message data] (:hyperfiddle.route/datomic-args @(:hypercrud.browser/route ctx))]
                         [:div
                          [:h3 (str "Unable to decode route: " s)]
                          [:h4 message]
                          [:pre data]]))})

(def not-found
  {:fiddle/ident :hyperfiddle.system/not-found
   :fiddle/type :blank
   :fiddle/markdown "# Route for url not found"})

(def unauthorized
  {:fiddle/ident :hyperfiddle.system/unauthorized
   :fiddle/type :blank
   :fiddle/markdown "## Credentials invalid or stale. Please login again."})

(defn system-fiddle? [fiddle-ident]
  (and (keyword? fiddle-ident) (= "hyperfiddle.system" (namespace fiddle-ident))))

(defn hydrate [fiddle-ident]
  (try-either
    (case fiddle-ident
      :hyperfiddle.system/decoding-error decoding-error
      :hyperfiddle.system/not-found not-found
      :hyperfiddle.system/unauthorized unauthorized)))
