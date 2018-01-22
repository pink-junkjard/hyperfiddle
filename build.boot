(def datomic-dep '[com.datomic/datomic-free "0.9.5561" :scope "provided"])
(def +version+ "0.3.0-SNAPSHOT")
(let [dependencies
      '[[com.taoensso/timbre "4.10.0"]
        [datascript "0.16.2"]
        [funcool/cats "2.2.0"]
        [funcool/cuerdas "2.0.4"]
        [funcool/promesa "1.8.1"]
        [io.nervous/kvlt "0.1.4"]
        [re-com "2.0.0"]

        [com.cognitect/transit-clj "0.8.300" :scope "provided"]
        [com.cognitect/transit-cljs "0.8.243" :scope "provided"]
        ;[com.datomic/datomic-free "0.9.5561" :scope "provided"] ; test-cljs chokes on this for some reason
        [org.clojure/clojure "1.8.0" :scope "provided"]
        [org.clojure/clojurescript "1.9.946" :scope "provided"]
        [reagent "0.7.0" :scope "provided"]]

      build-dependencies
      '[[adzerk/boot-cljs "2.1.4" :scope "test"]
        [adzerk/boot-test "1.2.0" :scope "test"]
        [adzerk/bootlaces "0.1.13" :scope "test"]
        [crisptrutski/boot-cljs-test "0.3.4" :scope "test"]
        [sparkfund/boot-lein-generate "0.3.0" :scope "test"]]]

  (set-env!
    :dependencies (concat dependencies build-dependencies)
    :resource-paths #{"src" "resources"}
    :boot.lein/project-clj {:dependencies (conj dependencies datomic-dep)}))

(require '[adzerk.boot-cljs :refer :all]
         '[adzerk.boot-test :as boot-test]
         '[adzerk.bootlaces :refer [push-snapshot]]
         '[crisptrutski.boot-cljs-test :refer [test-cljs]]
         'boot.lein)

(task-options!
  push #(into % {:repo "deploy-clojars" :ensure-version +version+})
  pom {:project 'com.hyperfiddle/hyperfiddle :version +version+}
  test-cljs {:js-env :node})

(deftask testing []
         ; cljs tests have to have _test in the filename
         (merge-env! :source-paths #{"test"}
                     :dependencies '[])
         identity)

(deftask test-clj []
         (merge-env! :dependencies [datomic-dep])
         (boot-test/test :include #".*-test"))

(when (> (.lastModified (clojure.java.io/file "build.boot"))
         (.lastModified (clojure.java.io/file "project.clj")))
  (boot.lein/write-project-clj))
