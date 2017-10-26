(ns hypercrud.readers-test
  (:require-macros [cljs.test :refer [deftest is]])
  (:require [cljs.test]
            [cljs.reader :as reader]
            [hypercrud.client.transit :as transit]
            [hypercrud.compile.eval :as eval]
            [hypercrud.types.DbError :refer [->DbError]]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.types.Entity :refer [->Entity ->ThinEntity]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hypercrud.types.URI :refer [->URI]]))

(defn test-all-forms [control literal-read strd transit-strd]
  (is (= control literal-read
         (reader/read-string (str control))
         (reader/read-string strd)
         (eval/eval-str-and-throw (str control))
         (eval/eval-str-and-throw strd)
         (transit/decode (transit/encode control))
         (transit/decode transit-strd))))

(deftest DbError []
  (test-all-forms (->DbError "foo")
                  #DbError "foo"
                  "#DbError\"foo\""
                  "{\"~#DbError\":\"foo\"}"))

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
