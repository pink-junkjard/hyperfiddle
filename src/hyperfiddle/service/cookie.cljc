(ns hyperfiddle.service.cookie)


; make sure the jwt cookie attributes are identical across platforms
(defn jwt-options-pedestal [domain]
  {:http-only true
   :domain domain
   :path "/"})

(defn jwt-options-express [domain]
  {"httpOnly" true
   "domain" domain
   "path" "/"})
