(ns hyperfiddle.ide.fiddles.errors)


(def invalid-route
  {:fiddle/ident :hyperfiddle.system/invalid-route
   :fiddle/type :blank
   :fiddle/renderer (str
                      '(let [[_ [route]] @(:hypercrud.browser/route ctx)]
                         [:div
                          [:h3 "Invalid route:"]
                          [:pre (cljs.spec.alpha/explain-str :hyperfiddle/route route)]]))})

(def not-found
  {:fiddle/ident :hyperfiddle.system/not-found
   :fiddle/type :blank
   :fiddle/markdown "# Fiddle not found"})

(def unauthorized
  {:fiddle/ident :hyperfiddle.system/unauthorized
   :fiddle/type :blank
   :fiddle/markdown "## Credentials invalid or stale. Please login again."})
