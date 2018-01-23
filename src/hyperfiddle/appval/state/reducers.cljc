(ns hyperfiddle.appval.state.reducers
  (:require [hyperfiddle.foundation.reducers :as foundation-reducers]
            [hyperfiddle.ide.reducers :as ide-reducers]
            [hyperfiddle.state :as state]))


(def root-reducer-map
  (merge foundation-reducers/reducer-map
         ide-reducers/reducer-map))

(def root-reducer (state/combine-reducers root-reducer-map))
