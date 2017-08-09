(def dependencies '[[com.cognitect/transit-clj "0.8.285"]
                    [com.hyperfiddle/util "0.0.1-SNAPSHOT"]
                    [org.apache.commons/commons-lang3 "3.4"]
                    [org.clojure/clojure "1.9.0-alpha14"]

                    ; build/test/dev
                    [adzerk/bootlaces "0.1.13" :scope "test"]
                    [sparkfund/boot-lein-generate "0.3.0" :scope "test"]
                    ])

(set-env!
  :resource-paths #{"src"}
  :dependencies dependencies
  ; we don't want util snapshot to be resolved to a real id when generating a project.clj
  ; [com.hyperfiddle/util "0.0.1-20170725.174645-3"]
  ; intellij will only join the module if the project.clj is written like:
  ; [com.hyperfiddle/util "0.0.1-SNAPSHOT"]
  :boot.lein/project-clj {:dependencies dependencies})

(require '[adzerk.bootlaces :refer :all]
         'boot.lein)

(def +version+ "0.2.0-SNAPSHOT")
(bootlaces! +version+)

(task-options!
  pom {:project 'com.hyperfiddle/hypercrud.server
       :version +version+})

(when (> (.lastModified (clojure.java.io/file "build.boot"))
         (.lastModified (clojure.java.io/file "project.clj")))
  (boot.lein/write-project-clj))
