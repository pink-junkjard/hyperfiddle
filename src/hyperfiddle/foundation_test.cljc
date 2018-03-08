(ns hyperfiddle.foundation-test
  (:require [#?(:clj clojure.test :cljs cljs.test) #?(:clj :refer :cljs :refer-macros) [deftest is]]
            #_[hyperfiddle.foundation :refer [hostname->hf-domain-name alias?]]))



;(def ctx {:hyperfiddle-hostname "hyperfiddle.net", :hostname "www.hyperfiddle.net"})
;
;(deftest alias-scrap
;  (is (= true (alias? "www.hyperfiddle.net")))
;  (is (= false (alias? "www")))
;  (is (= (hostname->hf-domain-name ctx) "www")))
