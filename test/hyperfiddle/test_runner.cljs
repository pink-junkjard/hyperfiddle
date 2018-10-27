(ns hyperfiddle.test-runner
  (:require
    [doo.runner :refer-macros [doo-tests]]

    ; NOT clj tests, this is only for CLJS tests
    contrib.char$-test
    contrib.data-test
    #_contrib.datomic-errors-test
    contrib.datomic-test
    contrib.datomic-tx-test
    contrib.eval-test
    contrib.reactive-test
    contrib.rfc3986-test
    contrib.string-test
    hypercrud.browser.field-test
    hypercrud.browser.nested-pull-test
    hypercrud.browser.router-test
    hyperfiddle.data-test
    hyperfiddle.fiddle-test
    hyperfiddle.ide-test
    hyperfiddle.ide.console-links-test
    hyperfiddle.ide.fiddles.schema_test
    hyperfiddle.readers-test
    ))

(defn run []
  (doo-tests
    'contrib.char$-test
    'contrib.data-test
    #_'contrib.datomic-errors-test
    'contrib.datomic-test
    'contrib.datomic-tx-test
    'contrib.eval-test
    'contrib.reactive-test
    'contrib.rfc3986-test
    'contrib.string-test
    'hypercrud.browser.field-test
    'hypercrud.browser.nested-pull-test
    'hypercrud.browser.router-test
    'hyperfiddle.data-test
    'hyperfiddle.fiddle-test
    'hyperfiddle.ide-test
    'hyperfiddle.ide.console-links-test
    'hyperfiddle.ide.fiddles.schema_test
    'hyperfiddle.readers-test
    ))
