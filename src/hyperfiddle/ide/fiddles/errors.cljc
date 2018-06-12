(ns hyperfiddle.ide.fiddles.errors)


(def not-found
  {:fiddle/ident :hyperfiddle.system/not-found
   :fiddle/type :blank
   :fiddle/markdown "# Fiddle not found"})

(def unauthorized
  {:fiddle/ident :hyperfiddle.system/unauthorized
   :fiddle/type :blank
   :fiddle/markdown "## Credentials invalid or stale. Please login again."})
