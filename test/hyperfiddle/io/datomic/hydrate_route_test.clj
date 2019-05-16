(ns hyperfiddle.io.datomic.hydrate-route-test
  (:require
    [cats.monad.exception :as exception]
    [clojure.java.io :as io]
    [clojure.test :refer [deftest is testing use-fixtures]]
    [contrib.reader :as reader]
    [contrib.uri :refer [->URI]]
    [hyperfiddle.database.fixtures :as fixtures]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.io.datomic.hydrate-route :as hydrate-route]
    [hyperfiddle.io.datomic.peer :as peer]                  ; todo run tests for client as well
    [hyperfiddle.io.datomic.sync :as datomic-sync]
    [promesa.core :as p]
    [taoensso.timbre :as timbre])
  (:import
    (clojure.lang ExceptionInfo)
    (java.util.regex Pattern)))


(def fiddle-ontology (-> (io/resource "schema/fiddle.edn") slurp reader/read-edn-string!))

(def test-domain
  (reify domain/Domain
    (fiddle-dbname [domain] "$src")
    (databases [domain]
      {"$" {:database/uri (->URI (str "datomic:mem://" 'hyperfiddle.io.datomic.hydrate-route-test "$"))}
       "$src" {:database/uri (->URI (str "datomic:mem://" 'hyperfiddle.io.datomic.hydrate-route-test "$src"))}})
    (system-fiddle? [domain fiddle-ident] false)))

(use-fixtures :each
  (fixtures/init-domain
    test-domain
    :schemas {"$" [{:db/ident :person/name
                    :db/valueType :db.type/string
                    :db/unique :db.unique/identity
                    :db/cardinality :db.cardinality/one}
                   {:db/ident :person/age
                    :db/valueType :db.type/long
                    :db/cardinality :db.cardinality/one}]
              "$src" fiddle-ontology}
    :init-txs {"$" [{:person/name "Bob"
                     :person/age 40}
                    {:person/name "Sue"
                     :person/age 50}]
               "$src" [{:fiddle/ident :persons
                        :fiddle/type :query
                        :fiddle/query (str '[:find (pull ?e [*]) :where [?e :person/name]])}]}))

(deftest duplicate-datoms []
  (testing "non source db"
    (let [response (timbre/with-config {:enabled? false}
                     (let [local-basis (datomic-sync/sync peer/impl test-domain ["$" "$src"])
                           route [:persons]
                           branch foundation/root-branch
                           stage {foundation/root-branch {"$" [[:db/add [:person/name "Bob"] :person/age 41]
                                                               [:db/add [:person/name "Bob"] :person/age 42]]}}
                           subject nil]
                       @(hydrate-route/hydrate-route peer/impl test-domain local-basis route branch stage subject)))]
      (is (-> response :schemas (get "$") exception/failure?))
      (is (-> response :schemas (get "$src") exception/success?))))

  (testing "source db"
    (let [response+ (timbre/with-config {:enabled? false}
                      (let [local-basis (datomic-sync/sync peer/impl test-domain ["$" "$src"])
                            route [:persons]
                            branch foundation/root-branch
                            stage {foundation/root-branch {"$src" [[:db/add [:fiddle/ident :persons] :db/doc "foo"]
                                                                   [:db/add [:fiddle/ident :persons] :db/doc "bar"]]}}
                            subject nil]
                        @(p/branch (hydrate-route/hydrate-route peer/impl test-domain local-basis route branch stage subject)
                                   exception/success
                                   exception/failure)))]
      (is (thrown-with-msg? ExceptionInfo (re-pattern (Pattern/quote ":db.error/datoms-conflict")) @response+)))))
