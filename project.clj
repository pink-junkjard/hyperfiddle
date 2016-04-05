(defproject org.hypercrud/hypercrud-client "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojurescript "1.7.228"]
                 [reagent "0.6.0-alpha"]
                 [io.nervous/kvlt "0.1.1"]
                 [funcool/promesa "1.1.1"]
                 [funcool/cats "1.2.1"]
                 [com.cognitect/transit-cljs "0.8.237"]]

  :plugins [[lein-cljsbuild "1.0.3"]]

  :source-paths ["src"]

  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src"]
                        :compiler {:output-to "out/main.js"
                                   :output-dir "out"
                                   :optimizations :none
                                   :source-map true}}]})
