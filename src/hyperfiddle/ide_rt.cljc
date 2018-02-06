(ns hyperfiddle.ide-rt)


(defprotocol SplitRuntime
  (sub-rt [rt foo target-repo])
  (target-repo [rt]))
