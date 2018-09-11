(ns hyperfiddle.ide.fiddles.schema_test
  (:require
    [clojure.test :refer [deftest is]]
    [contrib.reactive :as r]
    [hyperfiddle.ide.fiddles.schema :as schema]
    [hyperfiddle.ui :refer []]
    [reagent.dom.server :as dom-server]))


(deftest schema-renderer []
  (let [ctx {:cell (constantly [:pre])
             :hypercrud.browser/data (r/atom [])
             :hypercrud.browser/fiddle (r/atom nil)
             :hypercrud.browser/schemas (r/atom nil)}]
    ; just test it renderers something
    #_(is (not (nil? (dom-server/render-to-static-markup
                       [ui-comp ctx {:user-renderer (:fiddle/renderer (schema/schema "$"))}]))))))

(deftest db-attribute-renderer []
  (let [ctx {:cell (constantly [:pre])
             :hypercrud.browser/data (r/atom nil)
             :hypercrud.browser/fiddle (r/atom nil)
             :hypercrud.browser/schemas (r/atom nil)}]
    ; just test it renderers something
    ; Working, but missing a find-element etc mock. We need actual context mocks.
    #_(is (not (nil? (dom-server/render-to-static-markup
                       [ui-comp ctx {:user-renderer (:fiddle/renderer (schema/db-attribute-edit "$"))}]))))))
