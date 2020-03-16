(ns hyperfiddle.service.resolve)

(def mode #?(:clj user/mode
             :cljs (-> js/document (.getElementById "build") .-innerHTML)))

(def dev (or (= mode :dev)(= mode "dev")))

(def api-version-tag (if dev "dev" "0.0.1"))

(def locations
  {:assets {:url  "/static/_"
            :path "./.serve"}})

(defn api-routes [route-ns]
  {"global-basis" {:get (keyword route-ns "global-basis")
                   #".+" (keyword route-ns "404")
                   true (keyword route-ns "405")}

   ["hydrate-requests/" [#"[^/]*" :local-basis]] {:post (keyword route-ns "hydrate-requests")
                                                  #".+" (keyword route-ns "404")
                                                  true (keyword route-ns "405")}

   ["hydrate-route/" [#"[^/]*" :local-basis] "/" [#"[^/]*" :partition-id] "/" [#".*" :encoded-route]]
   {:get (keyword route-ns "hydrate-route")
    :post (keyword route-ns "hydrate-route")
    true (keyword route-ns "405")}

   ["local-basis/" [#"[^/]*" :global-basis] "/" [#".*" :encoded-route]]
   {:get (keyword route-ns "local-basis")
    :post (keyword route-ns "local-basis")
    true (keyword route-ns "405")}

   "sync" {:post (keyword route-ns "sync")
           #".+" (keyword route-ns "404")
           true (keyword route-ns "405")}

   "transact" {:post (keyword route-ns "transact")
               #".+" (keyword route-ns "404")
               true (keyword route-ns "405")}

   true (keyword route-ns "404")})

(def domain-routes
  ["/" {"api/" {(str api-version-tag "/") (api-routes nil)
                [[#"[^/]*" :version] "/"] {true :force-refresh}
                true                      :404}
        "static/" {[:build "/" [#".+" :resource-name]] {:get :static-resource
                                                        true :405}
                   true :404}
        "favicon.ico" :favicon
        true {:get :ssr
              true :405}}])

(def ide-routes
  ["/" {"api/"        {(str api-version-tag "/") (api-routes nil)
                       [[#"[^/]*" :version] "/"]    {true :force-refresh}
                       true                         :404}
        "api-user/"   {(str api-version-tag "/") (api-routes "user")
                       [[#"[^/]*" :version] "/"]    {true :force-refresh}
                       true                         :404}
        "auth0"       {:get  :hyperfiddle.ide/auth0-redirect
                       #".+" :404
                       true  :405}
        "logout"      {:post :hyperfiddle.ide/logout
                       #".+" :404
                       true  :405}
        "static/"     {[:build "/" [#".+" :resource-name]] {:get :static-resource
                                                            true :405}
                       true                                :404}
        "favicon.ico" :favicon
        true          {:get :ssr
                       true :405}}])

(def ide-user-routes
  ["/" {"api-user/" {(str api-version-tag "/") (api-routes nil)
                     [[#"[^/]*" :version] "/"]    {true :force-refresh}
                     true                         :404}
        ; todo this static path conflicts with the ide
        "static/"   {[:build "/" [#".+" :resource-name]] {:get :static-resource
                                                          true :405}
                     true                                :404}
        true        {:get :ssr
                     true :405}}])

(defprotocol HF-Resolve
  :extend-via-metadata true

  (setup [R])
  (handle [R request])

  (attr [R topic ks])
  (set-attr [R topic ks val])

  (uri-for [R topic location])
  (request [R topic])
  (dispatch [R topic])

  (render [R topic])
  (serve [R topic])
  (IO [R topic])
  (run-IO [R topic f]))

;(defprotocol HF-Resolve-In) ; Todo

(defprotocol Resolve-From
  :extend-via-metadata true
  (from [topic]))

(defn via [task f & args]
  (apply f (from task) task args))

(defn assoc-with [val R]
  (vary-meta val
    (fn [M]
      (apply merge
        (or M {})
        {`from (fn R-from [& _] R)}
        (for [method (->> HF-Resolve :method-builders keys)]
          {(symbol method) (fn R-proxy [_ & args] (apply @method R args))})))))
