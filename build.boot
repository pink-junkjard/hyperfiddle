(set-env!
  :resource-paths #{"src" "resources"}
  :dependencies '[[com.cognitect/transit-clj "0.8.285" :scope "provided"]

                  ;provided
                  [com.datomic/datomic-free "0.9.5561" :scope "provided"]
                  [org.clojure/clojure "1.9.0-alpha14" :scope "provided"]

                  ; unchecked
                  ;[ch.qos.logback/logback-classic "1.1.7" :exclusions [org.slf4j/slf4j-api]]
                  ;[io.pedestal/pedestal.service "0.5.1"]
                  ;[io.pedestal/pedestal.jetty "0.5.1"]
                  ;[ns-tracker "0.3.0"]
                  ;[org.apache.commons/commons-lang3 "3.4"]

                  ; build/test/dev
                  [adzerk/bootlaces "0.1.13" :scope "test"]
                  [sparkfund/boot-lein-generate "0.3.0" :scope "test"]
                  ])

(require '[adzerk.bootlaces :refer :all]
         'boot.lein)

(def +version+ "0.2.0-SNAPSHOT")
(bootlaces! +version+)

(task-options!
  pom {:project 'com.hyperfiddle/hypercrud.service
       :version +version+}
  ;aot {:namespace '#{service.main}}
  ;jar {:main 'service.main}
  )

(when (> (.lastModified (clojure.java.io/file "build.boot"))
         (.lastModified (clojure.java.io/file "project.clj")))
  (boot.lein/write-project-clj))
