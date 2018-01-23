(ns hyperfiddle.appval.state.reducers
  (:require [hyperfiddle.state :as state]
            [hyperfiddle.foundation.reducers :as foundation-reducers]
            [hyperfiddle.ide.reducers :as ide-reducers]))


(def root-reducer-map
  (merge foundation-reducers/reducer-map
         ide-reducers/reducer-map))

(def root-reducer (state/combine-reducers root-reducer-map))
