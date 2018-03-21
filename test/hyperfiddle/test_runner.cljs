(ns hyperfiddle.test-runner
  (:require
    [clojure.test :refer [deftest is]]
    [doo.runner :refer-macros [doo-tests]]
    [reagent.dom.server]                                    ; needed for hypercrud.ui.safe-render

    hypercrud.client.tx-test
    hypercrud.compile.eval-test
    hypercrud.readers-test
    hypercrud.ui.control.link-controls-test
    hyperfiddle.foundation-test
    hyperfiddle.ide-test
    ))

(defn run []
  (doo-tests
    'hypercrud.client.tx-test
    'hypercrud.compile.eval-test
    'hypercrud.readers-test
    'hypercrud.ui.control.link-controls-test
    'hyperfiddle.foundation-test
    'hyperfiddle.ide-test
    ))
