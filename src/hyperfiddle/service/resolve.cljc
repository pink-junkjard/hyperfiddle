(ns hyperfiddle.service.resolve
  (:require [contrib.etc :refer [tag]]))

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

(def ^:dynamic *Rs [])

(defmacro with [R & body]
  `(binding [*Rs (conj *Rs R)]
     ~@body))

(defmulti aspects (fn [R] (.getTypeName (type R))))

(defn tag-fragments [t]
  (assert (keyword? t))
  (clojure.string/split (name t) #"."))

(defn R-for [topic]
  (some (fn [R] (if (topic (aspects R)) R)) (reverse *Rs)))

(defn ! [& action]
  (assert (not (empty? *Rs)) "called in resolve context")
  (let [[topic _] (tag-fragments (tag action))
        R (R-for topic)]
    (assert (fn? (get (topic (aspects R)) (tag action))) (str "has handler for " action))
    (apply (get (topic (aspects R)) (tag action)) R (rest action))))

(defn repr [R]
  (if-let [repr-fn (:repr (aspects R))]
    (repr-fn R)
    R))

(defmacro log [& args]
  (with-meta
    `(taoensso.timbre/info ~@args)
    (meta &form)))

(defn cont [f]
  (let [*Rs' *Rs]
    (fn [& args]
      (binding [*Rs *Rs'] (apply f args)))))
