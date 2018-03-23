(def datomic-dep '[com.datomic/datomic-free "0.9.5561" :scope "provided" :exclusions [org.slf4j/slf4j-nop]])
(def +version+ "0.3.0-SNAPSHOT")
(let [dependencies
      '[[com.taoensso/timbre "4.10.0"]
        [datascript "0.16.2"]
        [bidi "2.1.3"]
        [funcool/cats "2.2.0"]
        [funcool/cuerdas "2.0.4"]
        [funcool/promesa "1.8.1"]
        [io.nervous/kvlt "0.1.4"]
        [net.cgrand/packed-printer "0.2.1"]
        [re-com "2.0.0"]

        [com.cognitect/transit-clj "0.8.300" :scope "provided"]
        [com.cognitect/transit-cljs "0.8.243" :scope "provided"]
        ;[com.datomic/datomic-free "0.9.5561" :scope "provided"] ; test-cljs chokes on this for some reason
        [org.clojure/clojure "1.8.0" :scope "provided"]
        [org.clojure/clojurescript "1.9.946" :scope "provided"]
        [reagent "0.7.0" :exclusions [cljsjs/react cljsjs/react-dom cljsjs/react-dom-server cljsjs/create-react-class] :scope "provided"]]

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
         '[crisptrutski.boot-cljs-test :as boot-cljs-test]
         'boot.lein)

(task-options!
  push #(into % {:repo "deploy-clojars" :ensure-version +version+})
  pom {:project 'com.hyperfiddle/hyperfiddle :version +version+})

; shadowing test-cljs breaks the test-cljs CLI e.g. boot test-cljs -n "contrib"
(deftask test-cljs [x exit? bool "Throw exception on error or inability to run tests."]
         (merge-env! :source-paths #{"test"}
                     :dependencies '[[doo "0.1.8"]])
         (boot-cljs-test/test-cljs :ids ["hyperfiddle/tests"]
                                   :js-env :node
                                   :exit? exit?))

(deftask test-clj []
         (merge-env! :source-paths #{"test"}
                     :dependencies [datomic-dep])
         (boot-test/test :include #".*-test"))

(when (> (.lastModified (clojure.java.io/file "build.boot"))
         (.lastModified (clojure.java.io/file "project.clj")))
  (boot.lein/write-project-clj))
