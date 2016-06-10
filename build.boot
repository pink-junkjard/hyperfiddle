(set-env!
  :dependencies '[[cljs-hash "0.0.2"]
                  [com.cemerick/url "0.1.1"]
                  [com.cognitect/transit-cljs "0.8.237"]
                  [funcool/promesa "1.1.1-HYPERCRUD"]
                  [io.nervous/kvlt "0.1.1"]
                  [org.clojure/core.async "0.2.374"]
                  [org.clojure/clojurescript "1.7.228"]
                  [org.clojure/core.match "0.3.0-alpha4"]
                  [reagent "0.6.0-alpha"]

                  [adzerk/boot-cljs "1.7.228-1" :scope "test"]
                  [adzerk/bootlaces "0.1.13" :scope "test"]
                  [adzerk/boot-test "1.0.6" :scope "test"]
                  [crisptrutski/boot-cljs-test "0.2.2-SNAPSHOT" :scope "test"]]
  :resource-paths #{"src"})

(require '[adzerk.boot-cljs :refer :all]
         '[adzerk.bootlaces :refer :all]
         '[crisptrutski.boot-cljs-test :refer [test-cljs]])

(def +version+ "0.1.0-SNAPSHOT")

(bootlaces! +version+)

(task-options!
  pom {:project 'org.hypercrud/hypercrud.client
       :version +version+}
  test-cljs {:js-env :node})


(deftask test []
         ; cljs tests have to have _test in the filename
         (merge-env! :source-paths #{"test"})
         (test-cljs))
