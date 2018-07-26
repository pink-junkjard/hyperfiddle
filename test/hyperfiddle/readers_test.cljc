(ns hyperfiddle.readers-test
  (:require [clojure.test :refer [deftest is]]
            [contrib.eval :as eval]
            [contrib.reader :as reader :refer [read-edn-string]]
            [hypercrud.transit :as transit]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.types.Entity :refer [->Entity]]
            [hypercrud.types.ThinEntity :refer [->ThinEntity]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.Err :refer [->Err]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [contrib.uri :refer [->URI]])
  (:import #?(:clj java.util.Date)))


(defn test-compile-read [control literal-read]
  (is (= control literal-read)))

(defn test-runtime-read [control strd]
  (is (= control
         (reader/read-string (pr-str control))
         (reader/read-string strd))))

(defn test-edn-read [control strd]
  (is (= control
         (read-edn-string (pr-str control))
         (read-edn-string strd))))

(defn test-eval [control strd]
  (is (= control
         (eval/eval-string (pr-str control))
         (eval/eval-string strd))))

(defn test-transit [control transit-strd]
  (is (= control
         (transit/decode (transit/encode control))
         (transit/decode transit-strd :type :json-verbose))))

(defn test-all-forms [control literal-read strd transit-strd]
  (test-compile-read control literal-read)
  (test-runtime-read control strd)
  (test-edn-read control strd)
  (test-eval control strd)
  (test-transit control transit-strd))

(deftest DbVal []
  (test-all-forms (->DbVal "foo" "bar")
                  #hypercrud.types.DbVal.DbVal{:uri "foo" :branch "bar"}
                  "#hypercrud.types.DbVal.DbVal{:uri \"foo\" :branch \"bar\"}"
                  "{\"~#DbVal\":[\"foo\",\"bar\"]}"))

(deftest Entity []
  (let [control (->Entity "foo" "bar")
        transit-strd "{\"~#Entity\":[\"foo\",\"bar\"]}"]
    (is (= (pr-str control) "\"bar\""))
    (test-transit control transit-strd)))

(deftest entity []
  (test-all-forms (->ThinEntity "foo" "bar")
                  #entity["foo" "bar"]
                  "#entity[\"foo\" \"bar\"]"
                  "{\"~#entity\":[\"foo\",\"bar\"]}"))

(deftest EReq []
  (test-all-forms (->EntityRequest "foo" "fizz" "buzz")
                  #hypercrud.types.EntityRequest.EntityRequest{:e "foo" :db "fizz" :pull-exp "buzz"}
                  "#hypercrud.types.EntityRequest.EntityRequest{:e \"foo\" :db \"fizz\" :pull-exp \"buzz\"}"
                  "{\"~#EReq\":[\"foo\",\"fizz\",\"buzz\"]}"))

(deftest Err-test []
  (test-all-forms (->Err "foo")
                  #hypercrud.types.Err.Err{:msg "foo"}
                  "#hypercrud.types.Err.Err{:msg \"foo\"}"
                  "{\"~#err\":\"foo\"}"))

(deftest QReq []
  (test-all-forms (->QueryRequest "foo" "bar")
                  #hypercrud.types.QueryRequest.QueryRequest{:query "foo" :params "bar"}
                  "#hypercrud.types.QueryRequest.QueryRequest{:query \"foo\" :params \"bar\"}"
                  "{\"~#QReq\":[\"foo\",\"bar\"]}"))

(deftest uri []
  (test-all-forms (->URI "foo")
                  #uri "foo"
                  "#uri \"foo\""
                  "{\"~#'\":\"~rfoo\"}"))

(deftest inst []
  (test-all-forms #?(:cljs (js/Date. "2017-12-31") :clj #inst "2017-12-31")
                  #inst "2017-12-31"
                  "#inst \"2017-12-31\""
                  "{\"~#t\":\"2017-12-31\"}"))
