(ns hyperfiddle.service.cookie)


; make sure the jwt cookie attributes are identical across platforms
(defn jwt-options-pedestal [hyperfiddle-hostname]
  {:http-only true
   :domain hyperfiddle-hostname
   :path "/"})

(defn jwt-options-express [hyperfiddle-hostname]
  {"httpOnly" true
   "domain" hyperfiddle-hostname
   "path" "/"})
