; todo this test belongs in hypecrud.util, should be cljc, and tested on both platforms
(ns hypercrud.readers-test
  (:require-macros [cljs.test :refer [deftest is]])
  (:require [cljs.test]
            [cljs.reader :as reader]
            [hypercrud.client.transit :as transit]
            [hypercrud.compile.eval :as eval]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.types.Entity :refer [->Entity ->ThinEntity]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.Err :refer [->Err]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hypercrud.types.URI :refer [->URI]]))

(defn test-compile-read [control literal-read]
  (is (= control literal-read)))

(defn test-runtime-read [control strd]
  (is (= control
         (reader/read-string (pr-str control))
         (reader/read-string strd))))

(defn test-eval [control strd]
  (is (= control
         (eval/eval-str-and-throw (pr-str control))
         (eval/eval-str-and-throw strd))))

(defn test-transit [control transit-strd]
  (is (= control
         (transit/decode (transit/encode control))
         (transit/decode transit-strd))))

(defn test-all-forms [control literal-read strd transit-strd]
  (test-compile-read control literal-read)
  (test-runtime-read control strd)
  (test-eval control strd)
  (test-transit control transit-strd))

(deftest DbVal []
  (test-all-forms (->DbVal "foo" "bar")
                  #DbVal ["foo" "bar"]
                  "#DbVal[\"foo\" \"bar\"]"
                  "{\"~#DbVal\":[\"foo\",\"bar\"]}"))

(deftest Entity []
  (test-all-forms (->Entity "foo" "bar")
                  #Entity ["foo" "bar"]
                  "#Entity[\"foo\" \"bar\"]"
                  "{\"~#Entity\":[\"foo\",\"bar\"]}"))

(deftest ->entity []
  (test-all-forms (->ThinEntity "foo" "bar")
                  #->entity["foo" "bar"]
                  "#->entity[\"foo\" \"bar\"]"
                  "{\"~#->entity\":[\"foo\",\"bar\"]}"))

(deftest EReq []
  (test-all-forms (->EntityRequest "foo" "bar" "fizz" "buzz")
                  #EReq["foo" "bar" "fizz" "buzz"]
                  "#EReq[\"foo\" \"bar\" \"fizz\" \"buzz\"]"
                  "{\"~#EReq\":[\"foo\",\"bar\",\"fizz\",\"buzz\"]}"))

(deftest Err-test []
  (test-all-forms (->Err "foo")
                  #hypercrud.types.Err.Err{:msg "foo"}
                  "#hypercrud.types.Err.Err{:msg \"foo\"}"
                  "{\"~#err\":\"foo\"}"))

(deftest QReq []
  (test-all-forms (->QueryRequest "foo" "bar" "fizz")
                  #QReq["foo" "bar" "fizz"]
                  "#QReq[\"foo\" \"bar\" \"fizz\"]"
                  "{\"~#QReq\":[\"foo\",\"bar\",\"fizz\"]}"))

(deftest URI []
  (test-all-forms (->URI "foo")
                  #URI "foo"
                  "#URI \"foo\""
                  "{\"~#'\":\"~rfoo\"}"))
