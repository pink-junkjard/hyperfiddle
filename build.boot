(set-env!
  :dependencies '[[deps-to-boot "1.1.0" :scope "test"]
                  [seancorfield/boot-tools-deps "0.4.7" :scope "test"]
                  [sparkfund/boot-lein "0.4.0" :scope "test"]])

(require '[boot-tools-deps.core :refer [deps]]
         '[deps-to-boot.core :refer [boot-deps]]
         '[sparkfund.boot-lein :as boot-lein])

(set-env!
  ::boot-lein/project-clj (-> (->> [(boot-deps)
                                    (boot-deps :lein)]
                                   (apply merge-with into))
                              (assoc :source-paths nil)     ; drop local/root paths for intellij
                              (update :dependencies #(vec (sort-by (comp str first) %)))))

(task-options!
  pom {:project 'com.hyperfiddle/ide
       :version "0.0.1-SNAPSHOT"})

(when-not (get-sys-env "CI")
  (let [lein-modified (.lastModified (clojure.java.io/file "project.clj"))]
    (when (or (> (.lastModified (clojure.java.io/file "build.boot")) lein-modified)
              (> (.lastModified (clojure.java.io/file "deps.edn")) lein-modified))
      (boot-lein/write-project-clj))))
