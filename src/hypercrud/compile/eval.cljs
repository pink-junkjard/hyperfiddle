(ns hypercrud.compile.eval
  (:require [cljs.js :as cljs]
            [markdown.core]))


(defn uate [code-str]
  (cljs/eval-str (cljs/empty-state)
                 code-str
                 nil
                 {:eval cljs/js-eval}
                 identity))
