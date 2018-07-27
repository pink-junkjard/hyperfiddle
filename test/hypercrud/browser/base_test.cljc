(ns hypercrud.browser.base-test
  (:require
    [clojure.test :refer [deftest is]]
    [hypercrud.browser.base :refer [validate-query-params]]
    ))


(def ctx {:peer nil
          :branch nil
          :hypercrud.browser/domain
          {#_#_:domain/environment
           {"$" #uri "datomic:free://datomic:4334/~dustin.getz"
            "$seattle" #uri "datomic:free://datomic:4334/seattle"}}})
(deftest validate-query-params-1
  []
  ; CompilerException java.lang.IllegalArgumentException: No implementation of method: :db of protocol: #'hypercrud.client.core/Peer found for class: nil,
  #_(is (= (validate-query-params '[:in $ ?e] "e" ctx)))
  )