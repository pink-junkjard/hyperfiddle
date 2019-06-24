(ns contrib.datomic.client.query-test
  (:require
    [clojure.spec.alpha :as s]
    [clojure.test :refer [deftest is]]
    [contrib.datomic.client.query :as client-query]))


; todo tests for map-form

(deftest valid-queries []
  ; examples pulled from https://docs.datomic.com/cloud/query/query-data-reference.html
  (doseq [query '[
                  [:find ?e
                   :where [?e :artist/name "The Beatles"]]

                  [:find ?name ?duration
                   :where [?e :artist/name "The Beatles"]
                   [?track :track/artists ?e]
                   [?track :track/name ?name]
                   [?track :track/duration ?duration]]

                  [:find (pull ?e [:artist/startYear :artist/endYear])
                   :where [?e :artist/name "The Beatles"]]

                  [:find (min ?dur) (max ?dur)
                   :where [_ :track/duration ?dur]]

                  [:find (sum ?count)
                   :with ?medium
                   :where [?medium :medium/trackCount ?count]]

                  [:find (count ?name) (count-distinct ?name)
                   :with ?artist
                   :where [?artist :artist/name ?name]]

                  [:find ?year (median ?namelen) (avg ?namelen) (stddev ?namelen)
                   :with ?track
                   :where [?track :track/name ?name]
                   [(count ?name) ?namelen]
                   [?medium :medium/tracks ?track]
                   [?release :release/media ?medium]
                   [?release :release/year ?year]]

                  [:find (distinct ?sortName)
                   :with ?artist
                   :where [?artist :artist/name "Fire"]
                   [?artist :artist/sortName ?sortName]]

                  [:find (min 5 ?millis) (max 5 ?millis)
                   :where [?track :track/duration ?millis]]

                  [:find (rand 2 ?name) (sample 2 ?name)
                   :where [_ :artist/name ?name]]

                  [:find ?artist-name ?release-name
                   :keys artist release
                   :where [?release :release/name ?release-name]
                   [?release :release/artists ?artist]
                   [?artist :artist/name ?artist-name]]

                  [:find (pull ?e [:artist/startYear :artist/endYear])
                   :in $ ?name
                   :where [?e :artist/name ?name]]

                  [:find ?e
                   :in $data ?age
                   :where [$data ?e :age ?age]]

                  [:find ?e
                   :in $ ?age
                   :where [?e :age ?age]]

                  [:find ?e
                   :where [?e :age ?age]]

                  [:find (pull ?e pattern)
                   :in $ ?name pattern
                   :where [?e :artist/name ?name]]

                  [:find ?release
                   :in $ [?artist-name ?release-name]
                   :where [?artist :artist/name ?artist-name]
                   [?release :release/artists ?artist]
                   [?release :release/name ?release-name]]

                  [:find ?release-name
                   :in $ [?artist-name ...]
                   :where [?artist :artist/name ?artist-name]
                   [?release :release/artists ?artist]
                   [?release :release/name ?release-name]]

                  [:find ?release
                   :in $ [[?artist-name ?release-name]]
                   :where [?artist :artist/name ?artist-name]
                   [?release :release/artists ?artist]
                   [?release :release/name ?release-name]]

                  [:find ?year
                   :where [?artist :artist/name "The Beatles"]
                   [?release :release/artists ?artist]
                   [?release :release/year ?year]]

                  [:find ?year
                   :where [?artist :artist/name "Janis Joplin"]
                   [?release :release/artists ?artist]
                   [?release :release/year ?year]]

                  [:find ?year
                   :where [?artist :artist/name "The Beatles"]
                   [?release :release/artists ?artist]
                   [?release :release/year ?year]
                   [?artist2 :artist/name "Janis Joplin"]
                   [?release2 :release/artists ?artist2]
                   [?release2 :release/year ?year]]

                  [:find ?e ?tx ?op
                   :in $mbrainz
                   :where [$mbrainz ?e :artist/name "The Beatles" ?tx ?op]]

                  [:find (sample 1 ?name)
                   :where [_ :artist/name ?name]]

                  [:find ?name
                   :where [_ :artist/name ?name]
                   [(<= "Q" ?name)]
                   [(< ?name "R")]]

                  [:find ?track-name ?minutes
                   :in $ ?artist-name
                   :where [?artist :artist/name ?artist-name]
                   [?track :track/artists ?artist]
                   [?track :track/duration ?millis]
                   [(quot ?millis 60000) ?minutes]
                   [?track :track/name ?track-name]]

                  [:find ?name ?count
                   :where [_ :artist/name ?name]
                   [(<= "Q" ?name)]
                   [(count ?name) ?count]
                   [(< 7 ?count)]]

                  [:find ?artist-name ?year
                   :in $ [?artist-name ...]
                   :where [?artist :artist/name ?artist-name]
                   [(get-else $ ?artist :artist/startYear "N/A") ?year]]

                  [:find ?e ?attr ?name
                   :in $ ?e
                   :where [(get-some $ ?e :country/name :artist/name) [?attr ?name]]]

                  [:find ?name
                   :where [?artist :artist/name ?name]
                   [(missing? $ ?artist :artist/startYear)]]

                  [:find ?k ?v
                   :where [(System/getProperties) [[?k ?v]]]]

                  [:find ?name
                   :where [_ :artist/name ?name]
                   [(.contains ^String ?name "woo")]]

                  [:find ?prefix
                   :in [?word ...]
                   :where [(subs ?word 0 5) ?prefix]]

                  [:find (count ?eid)
                   :where [?eid :artist/name]
                   (not [?eid :artist/country :country/CA])]

                  [:find (count ?artist)
                   :where [?artist :artist/name]
                   (not-join [?artist]
                             [?release :release/artists ?artist]
                             [?release :release/year 1970])]

                  [:find (count ?r)
                   :where [?r :release/name "Live at Carnegie Hall"]
                   (not-join [?r]
                             [?r :release/artists ?a]
                             [?a :artist/name "Bill Withers"])]

                  [:find (count ?medium)
                   :where (or [?medium :medium/format :medium.format/vinyl7]
                              [?medium :medium/format :medium.format/vinyl10]
                              [?medium :medium/format :medium.format/vinyl12]
                              [?medium :medium/format :medium.format/vinyl])]

                  [:find (count ?artist)
                   :where (or [?artist :artist/type :artist.type/group]
                              (and [?artist :artist/type :artist.type/person]
                                   [?artist :artist/gender :artist.gender/female]))]

                  [:find (count ?release)
                   :where [?release :release/name]
                   (or-join [?release]
                            (and [?release :release/artists ?artist]
                                 [?artist :artist/country :country/CA])
                            [?release :release/year 1970])]

                  [:find ?year
                   :where [?artist :artist/name "Bob Dylan"]
                   [?release :release/artists ?artist]
                   [?release :release/year ?year]]

                  [:find ?year
                   :with ?release
                   :where [?artist :artist/name "Bob Dylan"]
                   [?release :release/artists ?artist]
                   [?release :release/year ?year]]

                  [:find ?name ?duration
                   :in $ % ?aname
                   :where [?artist :artist/name ?aname]
                   (track-info ?artist ?name ?duration)]

                  [:find ?name
                   :in $ %
                   :where (benelux ?artist)
                   [?artist :artist/name ?name]]

                  [:find ?artist ?name ?duration
                   :in $ %
                   :where (track-info ?artist ?name ?duration)]

                  [:find ?artist ?name ?duration
                   :in $ %
                   :where [?artist :artist/name "The Rolling Stones"]
                   (track-info ?artist ?name ?duration)]

                  [:find ?name ?duration
                   :in $mbrainz $artists %
                   :where [$artists ?aname]
                   [$mbrainz ?artist :artist/name ?aname]
                   ($mbrainz track-info ?artist ?name ?duration)]
                  ]]
    (is (nil? (s/explain-data :contrib.datomic.client/query query)))))

(deftest valid-find-specs []
  ; examples pulled from https://docs.datomic.com/cloud/query/query-data-reference.html
  (doseq [find-spec '[[:find ?a (min ?b) (max ?b) ?c (sample 12 ?d)]]]
    (is (nil? (s/explain-data ::client-query/find-spec find-spec)))))

(deftest valid-data-patterns []
  ; examples pulled from https://docs.datomic.com/cloud/query/query-data-reference.html
  (doseq [data-pattern '[[_ :artist/name ?name _ _]
                         [_ :artist/name ?name]]]
    (is (nil? (s/explain-data ::client-query/data-pattern data-pattern)))))

(deftest valid-pred-expressions []
  ; examples pulled from https://docs.datomic.com/cloud/query/query-data-reference.html
  (doseq [pred-expr '[[(<= "Q" ?name)]
                      [(< ?name "R")]
                      [(clojure.string/starts-with? ?name "Q")]]]
    (is (nil? (s/explain-data ::client-query/pred-expr pred-expr)))))

(deftest valid-rules []
  ; examples pulled from https://docs.datomic.com/cloud/query/query-data-reference.html
  (doseq [rules '[
                  [[(track-info ?artist ?name ?duration)
                    [?track :track/artists ?artist]
                    [?track :track/name ?name]
                    [?track :track/duration ?duration]]]

                  [[(benelux ?artist)
                    [?artist :artist/country :country/BE]]
                   [(benelux ?artist)
                    [?artist :artist/country :country/NL]]
                   [(benelux ?artist)
                    [?artist :artist/country :country/LU]]]

                  [[(track-info [?artist] ?name ?duration)
                    [?track :track/artists ?artist]
                    [?track :track/name ?name]
                    [?track :track/duration ?duration]]]

                  ; todo
                  ; "In all the examples above, the body of each rule is made up solely of data clauses.
                  ; However, rules can contain any type of clause that a where clause might contain:
                  ;  data, expressions, or even other rule invocations."
                  ]]
    (is (nil? (s/explain-data ::client-query/rules rules)))))
