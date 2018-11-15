(ns hyperfiddle.ide.fiddles.errors)


(def decoding-error
  {:fiddle/ident :hyperfiddle.system.route/decoding-error
   :fiddle/type :blank
   :fiddle/renderer (str
                      '(let [[_ [s message data]] @(:hypercrud.browser/route ctx)]
                         [:div
                          [:h3 (str "Unable to decode route: " s)]
                          [:h4 message]
                          [:pre data]]))})

(def home-route-error
  {:fiddle/ident :hyperfiddle.system.route/home-route-error
   :fiddle/type :blank
   :fiddle/renderer (str
                      '(let [[_ [message]] @(:hypercrud.browser/route ctx)]
                         [:div
                          [:h3 "Invalid home route:"]
                          [:h4 message]]))})

(def not-found
  {:fiddle/ident :hyperfiddle.system.route/not-found
   :fiddle/type :blank
   :fiddle/markdown "# Route for url not found"})

(def unauthorized
  {:fiddle/ident :hyperfiddle.system/unauthorized
   :fiddle/type :blank
   :fiddle/markdown "## Credentials invalid or stale. Please login again."})
