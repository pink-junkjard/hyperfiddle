(ns hyperfiddle.test-runner
  (:require
    [doo.runner :refer-macros [doo-tests]]

    ; NOT clj tests, this is only for CLJS tests
    contrib.char$-test
    contrib.data-test
    contrib.datomic-errors-test
    contrib.datomic-test
    contrib.datomic-tx-test
    contrib.ednish-test
    contrib.eval-test
    contrib.match-test
    contrib.pprint-test
    contrib.reactive-test
    contrib.rfc3986-test
    contrib.string-test
    contrib.validation-test
    hypercrud.browser.router-bidi-test
    hypercrud.browser.context-test
    hyperfiddle.fiddle-test
    hyperfiddle.foundation-test
    hyperfiddle.ide.fiddles.schema_test
    hyperfiddle.io.global-basis-test
    hyperfiddle.io.rpc-router-test
    hyperfiddle.readers-test
    hyperfiddle.route-test
    hyperfiddle.ui.docstring-test
    ))

(defn run []
  (doo-tests
    'contrib.char$-test
    'contrib.data-test
    'contrib.datomic-errors-test
    'contrib.datomic-test
    'contrib.datomic-tx-test
    'contrib.ednish-test
    'contrib.eval-test
    'contrib.match-test
    'contrib.pprint-test
    'contrib.reactive-test
    'contrib.rfc3986-test
    'contrib.string-test
    'contrib.validation-test
    'hypercrud.browser.router-bidi-test
    'hypercrud.browser.context-test
    'hyperfiddle.fiddle-test
    'hyperfiddle.foundation-test
    'hyperfiddle.ide.fiddles.schema_test
    'hyperfiddle.io.global-basis-test
    'hyperfiddle.io.rpc-router-test
    'hyperfiddle.readers-test
    'hyperfiddle.route-test
    'hyperfiddle.ui.docstring-test
    ))
