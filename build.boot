(set-env!
  :dependencies '[[adzerk/bootlaces "0.1.13" :scope "test"]
                  [deps-to-boot "1.0.0" :scope "test"]
                  [sparkfund/boot-lein-generate "0.3.0" :scope "test"]])

(require '[adzerk.bootlaces :refer [push-snapshot]]
         'boot.lein
         '[deps-to-boot.core :refer [boot-deps]])

(let [env (boot-deps)]
  ; Show intellij the mvn artifacts; show boot the submodule'd src
  ; we don't want util snapshot to be resolved to a real id when generating a project.clj [com.hyperfiddle/hyperfiddle "0.3.0-20170725.174645-3"]
  ; intellij will only join the module if the project.clj is written like: [com.hyperfiddle/hyperfiddle "0.3.0-SNAPSHOT"]
  (->> (assoc-in env [:boot.lein/project-clj :dependencies] (->> (:dependencies env)
                                                                 (sort-by (comp str first))
                                                                 vec))
       (apply concat)
       (apply merge-env!)))

(def +version+ "0.0.1-SNAPSHOT")

(task-options!
  push #(into % {:repo "deploy-clojars" :ensure-version +version+})
  pom {:project 'com.hyperfiddle/hyperfiddle-server
       :version +version+})

(let [lein-modified (.lastModified (clojure.java.io/file "project.clj"))]
  (when (or (> (.lastModified (clojure.java.io/file "build.boot")) lein-modified)
            (> (.lastModified (clojure.java.io/file "deps.edn")) lein-modified))
    (boot.lein/write-project-clj)))
