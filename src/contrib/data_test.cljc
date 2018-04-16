(ns contrib.data-test
  (:require [clojure.test :refer [deftest is]]
            [contrib.data :refer [split-first rtrim-coll abc]]
            [clojure.pprint]
            [net.cgrand.packed-printer :as packed-printer]))



(def s "0/1/2?3?4#5#6")
(deftest split-first-1
  []
  (is (= (split-first s "/") ["0" "1/2?3?4#5#6"]))
  (is (= (split-first s "?") ["0/1/2" "3?4#5#6"]))
  (is (= (split-first s "#") ["0/1/2?3?4" "5#6"])))


(deftest seq-rtrim-1
  []
  (is (= (rtrim-coll nil? [:post nil]) [:post])))

(deftest abc-1
  []
  (is (= (take 4 (abc)) '(:a :b :c :d)))
  (is (= (count (doall (abc))) 26))
  (is (= (last (abc)) :z))
  )


(def code-form
  '(fn [ctx]
     (let [hide-datomic (reagent.core/atom true)
           hide-archived (reagent.core/atom true)
           db-attr? #(<= (:db/id %) 62)
           archived? #(cuerdas.core/starts-with? (namespace (:db/ident %)) "zzz") ; "zzz/" and "zzz.", we are inconsistent. It should be modeled and queried and never shown
           do-filter-reactive (fn [xs] ; perf sensitive
                                (as-> xs xs
                                      (if @hide-datomic (remove db-attr? xs) xs)
                                      (if @hide-archived (remove archived? xs) xs)))]
       (fn [ctx]
         [:div.hyperfiddle-schema
          [hypercrud.ui.control.markdown-rendered/markdown (-> ctx :hypercrud.browser/fiddle deref :db/doc)]
          [:label {:style {:font-weight "400" :display "block"}} [:input {:type "checkbox" :checked @hide-datomic :on-change #(swap! hide-datomic not)}] " hide Datomic system attributes"]
          [:label {:style {:font-weight "400" :display "block"}} [:input {:type "checkbox" :checked @hide-archived :on-change #(swap! hide-archived not)}] " hide Hyperfiddle archived attributes"]
          (let [ctx (-> ctx
                        (dissoc :relation :relations)
                        (update :hypercrud.browser/result (partial contrib.reactive/fmap do-filter-reactive #_(contrib.reactive/partial filter f?)))
                        (hypercrud.browser.context/with-relations))]
            [hypercrud.ui.result/result ctx])]))))

(deftest pprint-performance-1
  []

  (time
    (with-out-str
      (clojure.pprint/pprint code-form)))

  (time
    (with-out-str
      (packed-printer/pprint code-form)))

  )