(ns hyperfiddle.ide.preview.runtime
  (:require
    [contrib.reactive :as r]
    [hypercrud.client.core :as hc]
    [hypercrud.client.peer :as peer]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.state :as state]))


(deftype Runtime [domain io state-atom root-reducer]
  runtime/State
  (dispatch! [rt action-or-func] (state/dispatch! state-atom root-reducer action-or-func))
  (state [rt] state-atom)
  (state [rt path] (r/cursor state-atom path))

  runtime/HF-Runtime
  (domain [rt] domain)
  (io [rt] io)

  hc/Peer
  (hydrate [this branch request] (peer/hydrate state-atom branch request))

  IEquiv
  (-equiv [o other]
    (and (instance? Runtime other)
         (= (.-domain o) (.-domain other))
         (= (.-io o) (.-io other))
         (= (.-state-atom o) (.-state-atom other))
         (= (.-root-reducer o) (.-root-reducer other))))

  IHash
  (-hash [this] (hash [domain io state-atom root-reducer])))
