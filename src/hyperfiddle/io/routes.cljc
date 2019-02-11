(ns hyperfiddle.io.routes)


(defn api [route-ns]
  {"global-basis" {:get (keyword route-ns "global-basis")
                   #".+" (keyword route-ns "404")
                   true (keyword route-ns "405")}

   ["hydrate-requests/" [#"[^/]*" :local-basis]] {:post (keyword route-ns "hydrate-requests")
                                                  #".+" (keyword route-ns "404")
                                                  true (keyword route-ns "405")}

   ["hydrate-route/" [#"[^/]*" :local-basis] "/" [#"[^/]*" :branch] "/" [#".*" :encoded-route]]
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

(defn build [build]
  ["/" {"api/" {(str build "/") (api nil)
                [[#"[^/]*" :build] "/"] {true :force-refresh}
                true :404}
        "auth0" {:get :auth0-redirect
                 #".+" :404
                 true :405}
        true {:get :ssr
              true :405}}])
