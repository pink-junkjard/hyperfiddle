(set-env!
  :dependencies '[
                  ; provided
                  [org.clojure/clojurescript "1.9.36" :scope "provided"]
                  [reagent "0.6.0-rc" :scope "provided"]

                  ; build/test/dev
                  [adzerk/boot-cljs "1.7.228-1" :scope "test"]
                  [adzerk/bootlaces "0.1.13" :scope "test"]])

(require '[adzerk.boot-cljs :refer :all]
         '[adzerk.bootlaces :refer [push-snapshot]])

(def +version+ "0.2.0-SNAPSHOT")

(task-options!
  push #(into % {:repo "deploy-clojars" :ensure-version +version+})
  pom {:version +version+})

(deftask browser []
         (set-env! :resource-paths #{"src-browser"})
         (comp (pom :project 'com.hyperfiddle/hypercrud.platform.browser)
               (jar)))

(deftask node []
         (set-env! :resource-paths #{"src-node"})
         (comp (pom :project 'com.hyperfiddle/hypercrud.platform.node)
               (jar)))
