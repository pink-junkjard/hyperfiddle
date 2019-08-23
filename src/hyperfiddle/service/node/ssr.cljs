(ns hyperfiddle.service.node.ssr
  (:require
    ["react-dom/server" :as dom-server]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.http-client :as http-client]
    [hyperfiddle.io.local-basis :as local-basis]
    [promesa.core :as p]
    [reagent.impl.template :as tmpl]
    [reagent.impl.util :as rutil]
    [reagent.ratom :as ratom]))


(deftype IOImpl [domain service-uri ?jwt]
  io/IO
  (global-basis [io]
    (http-client/global-basis! domain service-uri ?jwt))

  (local-basis [io global-basis route]
    (p/resolved (local-basis/local-basis io global-basis route)))

  (hydrate-requests [io local-basis partitions requests]
    (http-client/hydrate-requests! domain service-uri local-basis partitions requests ?jwt))

  (hydrate-route [io local-basis route pid partitions]
    (http-client/hydrate-route! domain service-uri local-basis route pid partitions ?jwt))

  (sync [io dbnames]
    (http-client/sync! domain service-uri dbnames ?jwt)))

(defn render-to-node-stream
  [component]
  (ratom/flush!)
  (binding [rutil/*non-reactive* true]
    (dom-server/renderToNodeStream (tmpl/as-element component))))
