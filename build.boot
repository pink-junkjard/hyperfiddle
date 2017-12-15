(set-env!
  :dependencies '[[com.taoensso/timbre "4.10.0"]
                  [datascript "0.16.2"]
                  [funcool/cats "2.1.0"]
                  [funcool/cuerdas "2.0.4"]
                  [funcool/promesa "1.8.1"]
                  [io.nervous/kvlt "0.1.4"]
                  [re-com "2.0.0"]

                  ; provided
                  [reagent "0.7.0" :scope "provided"]

                  ; provided clj
                  [org.clojure/clojure "1.8.0" :scope "provided"]
                  [com.cognitect/transit-clj "0.8.300" :scope "provided"]

                  ; provided cljs
                  [org.clojure/clojurescript "1.9.946" :scope "provided"]
                  [com.cognitect/transit-cljs "0.8.243" :scope "provided"]

                  ; build/test/dev
                  [adzerk/boot-cljs "2.1.4" :scope "test"]
                  [adzerk/bootlaces "0.1.13" :scope "test"]
                  [adzerk/boot-test "1.2.0" :scope "test"]
                  [crisptrutski/boot-cljs-test "0.3.4" :scope "test"]
                  [sparkfund/boot-lein-generate "0.3.0" :scope "test"]]
  :resource-paths #{"src" "resources"})

(require '[adzerk.boot-cljs :refer :all]
         '[adzerk.bootlaces :refer [push-snapshot]]
         '[crisptrutski.boot-cljs-test :refer [test-cljs]]
         'boot.lein)

(def +version+ "0.3.0-SNAPSHOT")

(task-options!
  push #(into % {:repo "deploy-clojars" :ensure-version +version+})
  pom {:project 'com.hyperfiddle/hyperfiddle
       :version +version+}
  test-cljs {:js-env :node})

(deftask testing []
         ; cljs tests have to have _test in the filename
         (merge-env! :source-paths #{"test"}
                     :dependencies '[])
         identity)

(when (> (.lastModified (clojure.java.io/file "build.boot"))
         (.lastModified (clojure.java.io/file "project.clj")))
  (boot.lein/write-project-clj))
