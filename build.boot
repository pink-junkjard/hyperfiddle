(set-env!
  :dependencies '[[adzerk/boot-cljs "1.7.228-1" :scope "test"]
                  [adzerk/bootlaces "0.1.13" :scope "test"]
                  [org.clojure/clojurescript "1.7.228"]
                  [reagent "0.6.0-alpha"]
                  [io.nervous/kvlt "0.1.1"]
                  [funcool/promesa "1.1.1"]
                  [funcool/cats "1.2.1"]
                  [com.cognitect/transit-cljs "0.8.237"]
                  [datascript "0.15.0"]]
  :resource-paths #{"src"})

(require '[adzerk.boot-cljs :refer :all]
         '[adzerk.bootlaces :refer :all])

(def +version+ "0.1.0-SNAPSHOT")

(bootlaces! +version+)

(task-options!
  pom {:project 'org.hypercrud/hypercrud-client
       :version +version+})

