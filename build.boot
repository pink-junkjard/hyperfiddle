(set-env!
  :dependencies '[[com.cemerick/url "0.1.1"]
                  [com.cognitect/transit-cljs "0.8.239"]
                  ;[funcool/promesa "1.1.1"]
                  [io.nervous/kvlt "0.1.1"]
                  [markdown-clj "0.9.88"]
                  [org.clojure/clojurescript "1.9.36"]
                  [org.clojure/core.match "0.3.0-alpha4"]
                  [org.clojars.cemerick/loom "0.6.1-SNAPSHOT"] ; see https://github.com/aysylu/loom/pull/91
                  [reagent "0.6.0-rc" :exclusions [] #_ [cljsjs/react cljsjs/react-dom cljsjs/react-dom-server]]
                  [re-com "2.0.0"]

                  [adzerk/boot-cljs "1.7.228-1" :scope "test"]
                  [adzerk/bootlaces "0.1.13" :scope "test"]
                  [adzerk/boot-test "1.1.1" :scope "test"]
                  [crisptrutski/boot-cljs-test "0.2.2-SNAPSHOT" :scope "test"]
                  [onetom/boot-lein-generate "0.1.3" :scope "test"]]
  :resource-paths #{"src"})

(require '[adzerk.boot-cljs :refer :all]
         '[adzerk.bootlaces :refer :all]
         '[crisptrutski.boot-cljs-test :refer [test-cljs]])

(require 'boot.lein) (boot.lein/generate)

(def +version+ "0.1.0-SNAPSHOT")

(bootlaces! +version+)

(task-options!
  pom {:project 'org.hypercrud/hypercrud.client
       :version +version+}
  test-cljs {:js-env :node})


(deftask test []
         ; cljs tests have to have _test in the filename
         #_ (merge-env! :source-paths #{"test"})
         #_ (test-cljs))
