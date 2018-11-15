(ns hyperfiddle.foundation-test
  (:require [clojure.test :refer [deftest is]]
            [contrib.reactive :as r]
            [contrib.reader]                                ; wrong dependency?
            [hyperfiddle.foundation :refer [route-encode]]
            [hyperfiddle.runtime :as runtime]))


(def state
  (r/atom
    {::runtime/domain
     {:domain/home-route (pr-str
                           [:hyperblog/index :a #entity["$" :highlights]])
      :domain/router (pr-str
                       ["/"
                        {"drafts/" :hyperblog/drafts
                         "pairing/" :user/pairing
                         [#entity["$" :a]] :hyperblog/post}])}}))

(def rt (reify runtime/State
          (state [_ path]
            (r/cursor state path))))

(def tests
  {[:hyperblog/post [#entity["$" :capitalism]]]
   "/:capitalism"

   [:hyperblog/post [#entity["$" 1234]]]
   "/1234"

   [:hyperblog/post [#entity["$" :capitalism]] nil ":src"]
   "/:capitalism#:src"
   })

(deftest user-router-1 []
  (doseq [[k v] tests]
    (is (= (route-encode rt k) v)))
  )
