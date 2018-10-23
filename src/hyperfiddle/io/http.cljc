(ns hyperfiddle.io.http)


(def api
  {"global-basis" {:get :global-basis
                   #".+" :404
                   true :405}

   ["hydrate-requests/" [#"[^/]*" :local-basis]] {:post :hydrate-requests
                                                  #".+" :404
                                                  true :405}

   ["hydrate-route/" [#"[^/]*" :local-basis] "/" [#"[^/]*" :branch] "/" [#"[^/]*" :branch-aux] "/" [#".*" :encoded-route]]
   {:get :hydrate-route
    :post :hydrate-route
    true :405}

   ["local-basis/" [#"[^/]*" :global-basis] "/" [#"[^/]*" :branch] "/" [#"[^/]*" :branch-aux] "/" [#".*" :encoded-route]]
   {:get :local-basis
    :post :local-basis
    true :405}

   "sync" {:post :sync
           #".+" :404
           true :405}

   "transact" {:post :transact
               #".+" :404
               true :405}

   true :404})

(defn build-routes [build]
  ["/" {"api/" {(str build "/") api
                [[#"[^/]*" :build] "/"] {true :force-refresh}
                true :404}
        "auth0" {:get :auth0-redirect
                 #".+" :404
                 true :405}
        true {:get :ssr
              true :405}}])
