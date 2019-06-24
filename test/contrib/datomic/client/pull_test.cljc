(ns contrib.datomic.client.pull-test
  (:require
    [clojure.spec.alpha :as s]
    [clojure.test :refer [deftest is]]
    [contrib.datomic.client.pull :as client-pull]))


(deftest valid-pull-patterns []
  ; examples pulled from https://docs.datomic.com/cloud/query/query-pull.html
  (doseq [pull-pattern '[
                         [:artist/name :artist/startYear]
                         [:release/artists]
                         [*]
                         [* :release/_artists]
                         [:track/name {:track/artists [:db/id :artist/name]}]
                         [{:release/media
                           [{:medium/tracks
                             [:track/name {:track/artists [:artist/name]}]}]}]
                         [(:artist/name :as "Band Name")]
                         [:artist/name (:track/_artists :limit 10)]
                         [{(:track/_artists :limit 10) [:track/name]}]
                         [:artist/name (:track/_artists :limit nil)]
                         [:artist/name (:artist/endYear :default 0)]
                         [:artist/name (:artist/endYear :default "N/A")]
                         [* {:track/artists [:artist/name]}]
                         [:person/firstName :person/lastName {:person/friends 6}]
                         [:person/firstName :person/lastName {:person/friends ...}]
                         [:penguins]
                         [{:track/artists [:penguins]}]
                         [:release/media]
                         [:artist/_country]
                         [:artist/name :died-in-1966?]
                         [:artist/name (limit :track/_artists 10)]
                         [{(limit :track/_artists 10) [:track/name]}]
                         [:artist/name (limit :track/_artists nil)]
                         [:artist/name (default :artist/endYear 0)]
                         [:artist/name (default :artist/endYear "N/A")]
                         ]]
    (is (nil? (s/explain-data ::client-pull/pattern pull-pattern)))))
