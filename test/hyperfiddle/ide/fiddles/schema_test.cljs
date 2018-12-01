(ns hyperfiddle.ide.fiddles.schema_test
  (:require
    [clojure.test :refer [deftest is]]
    [contrib.reactive :as r]
    [contrib.reader :refer [read-edn-string! read-string]]
    [contrib.uri :refer [->URI]]
    [hypercrud.client.core :refer [Peer]]
    [hypercrud.client.peer :as peer]
    [hypercrud.types.EntityRequest :refer [->EntityRequest]]
    [hypercrud.types.QueryRequest :refer [->QueryRequest]]
    [hyperfiddle.core]
    [hyperfiddle.ide]
    [hyperfiddle.ide.fiddles.schema :as schema-fiddle]
    [hyperfiddle.project :as project]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui.iframe :refer [iframe-cmp]]
    [reagent.dom.server :as dom-server]))


(deftype TestRuntime [state-atom]
  runtime/State
  (state [rt] state-atom)
  (state [rt path] (r/cursor state-atom path))

  Peer
  (hydrate [this branch request]
    (peer/hydrate state-atom branch request))

  (db [this uri branch]
    (peer/db-pointer uri branch)))

(defn render [c]
  (try (dom-server/render-to-static-markup c)
       (catch :default e (js/console.error e) (throw e))))

(defn render-system-fiddle [route req res]
  (let [ctx {:branch nil
             :peer (->TestRuntime (r/atom nil))
             :hypercrud.browser/domain {:domain/fiddle-database {:database/uri (->URI "fiddle-db")}
                                        :domain/databases #{{:domain.database/name "$"
                                                             :domain.database/record {:database/uri (->URI "$-db")}}}}
             :hypercrud.ui/display-mode (r/atom :hypercrud.browser.browser-ui/user)}
        ptm {req res
             (project/attrs-request ctx) []}
        ctx (assoc ctx :peer (->TestRuntime (r/atom {::runtime/partitions {nil {:ptm ptm}}})))]
    (render [iframe-cmp ctx {:route route}])))

(deftest schema []
  (let [req (-> (schema-fiddle/schema "$")
                :fiddle/query
                read-string
                (->QueryRequest [(peer/db-pointer (->URI "$-db") nil)]))]
    (is (not (nil? (render-system-fiddle [:hyperfiddle.schema/$] req nil))))))

(deftest db-attribute-edit []
  (let [req (->> (schema-fiddle/db-attribute-edit "$")
                 :fiddle/pull
                 read-edn-string!
                 (->EntityRequest nil (peer/db-pointer (->URI "$-db") nil)))]
    (is (not (nil? (render-system-fiddle [:hyperfiddle.schema.db-attribute-edit/$] req nil))))))
