(let [dependencies
      '[[com.auth0/java-jwt "2.2.0"]
        [com.cognitect/transit-clj "0.8.300"]
        [com.taoensso/timbre "4.10.0"]
        [datascript "0.16.2"]
        [funcool/cuerdas "2.0.4"]
        [funcool/promesa "1.8.1"]
        [io.nervous/kvlt "0.1.4"]
        [io.pedestal/pedestal.service "0.5.1"]
        ;[org.apache.commons/commons-lang3 "3.4"]
        [org.clojure/clojure "1.9.0-alpha14"]
        [org.clojure/core.async "0.2.395" #_"transitive - override pedestal"]
        [org.clojure/core.incubator "0.1.4" #_"transitive - override pedestal"]]
      build-dependencies
      '[[adzerk/bootlaces "0.1.13" :scope "test"]
        [sparkfund/boot-lein-generate "0.3.0" :scope "test"]]]

  (set-env!
    :resource-paths #{"src"}
    :dependencies (concat dependencies build-dependencies)

    :boot.lein/project-clj {:resource-paths #{"src"}
                            :source-paths #{"dev"}
                            :dependencies (concat dependencies
                                                  '[[funcool/cats "2.1.0"]
                                                    [com.hyperfiddle/hyperfiddle "0.3.0-SNAPSHOT"]])}))

(require '[adzerk.bootlaces :refer [push-snapshot]]
         'boot.lein)

(def +version+ "0.3.0-SNAPSHOT")

(task-options!
  push #(into % {:repo "deploy-clojars" :ensure-version +version+})
  pom {:project 'com.hyperfiddle/hypercrud.server
       :version +version+})

(when (> (.lastModified (clojure.java.io/file "build.boot"))
         (.lastModified (clojure.java.io/file "project.clj")))
  (boot.lein/write-project-clj))


; Show intellij the mvn artifacts; show boot the submodule'd src
; we don't want util snapshot to be resolved to a real id when generating a project.clj [com.hyperfiddle/hyperfiddle "0.3.0-20170725.174645-3"]
; intellij will only join the module if the project.clj is written like: [com.hyperfiddle/hyperfiddle "0.3.0-SNAPSHOT"]