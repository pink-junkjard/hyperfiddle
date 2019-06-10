(ns hyperfiddle.ide.preview.runtime
  (:require
    [contrib.reactive :as r]
    [hypercrud.client.peer :as peer]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.branch :as branch]
    [hyperfiddle.ide.routing :as ide-routing]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.state :as state]))


(deftype Runtime [ide-rt preview-root-branch domain io state-atom root-reducer]
  runtime/State
  (dispatch! [rt action-or-func] (state/dispatch! state-atom root-reducer action-or-func))
  (state [rt] state-atom)
  (state [rt path] (r/cursor state-atom path))

  runtime/HF-Runtime
  (domain [rt] domain)
  (io [rt] io)
  (hydrate [rt branch request] (peer/hydrate state-atom branch request))
  (-set-route [rt branch preview-route force-hydrate]
    (if (= preview-root-branch branch)
      ; just blast the ide's route and let things trickle down
      (let [ide-route (ide-routing/preview-route->ide-route preview-route)]
        (actions/set-route ide-rt (branch/parent-branch-id branch) ide-route force-hydrate))
      (actions/set-route rt branch preview-route force-hydrate)))

  IEquiv
  (-equiv [o other]
    (and (instance? Runtime other)
         (= (.-ide-rt o) (.-ide-rt other))
         (= (.-preview-root-branch o) (.-preview-root-branch other))
         (= (.-domain o) (.-domain other))
         (= (.-io o) (.-io other))
         (= (.-state-atom o) (.-state-atom other))))

  IHash
  (-hash [this] (hash [ide-rt preview-root-branch domain io state-atom])))
