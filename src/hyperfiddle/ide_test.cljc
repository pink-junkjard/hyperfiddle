(ns hyperfiddle.ide-test
  (:require [clojure.test :refer [deftest is]]
            [contrib.reader]                                ; wrong dependency?
            [bidi.bidi :as bidi]
            [hyperfiddle.runtime :as hfr]
            [contrib.reactive :as r]
            [hyperfiddle.ide :refer [route-encode route-decode ->bidi-consistency-wrapper bidi->hf ->bidi
                                     activate-ide?]]
            [hyperfiddle.foundation :as foundation]))


(def state
  (atom
    {:hyperfiddle.runtime/domain
     {:domain/home-route (pr-str
                           [:hyperblog/index :a #entity["$" :highlights]])
      :domain/router (pr-str
                       ["/"
                        {"drafts/" :hyperblog/drafts
                         "pairing/" :user/pairing
                         [#entity["$" :a] "/"] :hyperblog/index
                         [#entity["$" :a]] :hyperblog/post}])}}))

(def rt (reify hfr/State
          (state [_ path]
            (r/cursor state path))))

(def tests
  {[:hyperblog/post [#entity["$" :capitalism]]]
   "/:capitalism"

   [:hyperblog/post [#entity["$" 1234]]]
   "/1234"

   [:hyperblog/index [#entity["$" :personal]]]
   "/:personal/"
   })

(deftest user-router-1
  []
  (for [[k v] tests]
    (is (= (route-encode rt k) v)))
  )

(def a-ctx-1 {:hyperfiddle-hostname "hyperfiddle.net"})

(deftest activate-ide-1
  []
  (is (not (activate-ide?
             (foundation/hostname->hf-domain-name
               (assoc a-ctx-1 :hostname "www.hyperfiddle.net"))))))


(comment
  ; An example user router (this is provided on the domain)
  (def router                                               ; only needs to support fq routes
    ["/"
     {"drafts/" :hyperblog/drafts
      [#entity["$" :a] "/"] :hyperblog/index
      [#entity["$" :a]] :hyperblog/post}]
    #_["/"
       {"drafts/" :hyperblog/drafts
        [#entity["$" :a] "/"] :hyperblog/index
        #{[#entity["$" :a] "/" :b]
          [#entity["$" :a]]} :hyperblog/post}])

  ;[#entity["$" :a] "/"] 17592186045454 ;IllegalArgumentException: No implementation of method: :unresolve-handler of protocol: #'bidi.bidi/Matched found for class: java.lang.Long

  (some-> router (bidi/match-route "/:idddd/slug"))

  (->> [
        (some-> router (bidi/match-route "/1/"))
        #_(some-> router (bidi/match-route "/a/"))
        #_(some-> router (bidi/match-route "/:a/"))
        (some-> router (bidi/match-route "/:highlights/"))
        (some-> router (bidi/match-route "/1/slug"))
        #_(some-> router (bidi/match-route "/:sup"))        ; routes but can't rebuild
        (some-> router (bidi/match-route "/:sup/slug"))
        (some-> router (bidi/match-route "/drafts/"))
        (some-> router (bidi/match-route "/1/slug-for-a-post"))
        (some-> router (bidi/match-route "/_/asdf"))
        ]
       (map ->bidi-consistency-wrapper)
       (map bidi->hf)
       (map ->bidi)
       (map #(if % (apply bidi/path-for router %))))

  (apply bidi/path-for router (-> router (bidi/match-route "/1/") ->bidi-consistency-wrapper bidi->hf ->bidi))
  (some-> router (bidi/match-route "/1/slug") ->bidi-consistency-wrapper bidi->hf ->bidi ((partial apply bidi/path-for router)))
  (some-> router (bidi/match-route "/:sup/a") ->bidi-consistency-wrapper bidi->hf ->bidi ((partial apply bidi/path-for router)))

  (->> [(bidi/path-for router :hyperblog/index :a #entity["$" 1])
        (bidi/path-for router :hyperblog/index :a #entity["$" :highlights])
        (bidi/path-for router :hyperblog/post :a #entity["$" 1] :b "asdf")
        (bidi/path-for router :hyperblog/post :a #entity["$" :sup])
        (bidi/path-for router :hyperblog/drafts)
        (bidi/path-for router :not-routed)

        (apply bidi/path-for router [:hyperblog/index 0 #entity["$" 17592186046269]])
        ;(apply bidi/path-for router [17592186045454 0 #entity["$" 17592186046269]])
        ])

  (->> [(some-> router (bidi/match-route "/:sup"))
        (some-> router (bidi/match-route "/:personal/"))]
       (map ->browser))

  (apply bidi/path-for router [:hyperblog/post 0 #entity["$" :sup]])
  (apply bidi/path-for router [:hyperblog/index 0 #entity["$" :personal]])
  (apply bidi/path-for router [:hyperblog/drafts])
  )