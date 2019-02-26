(ns hyperfiddle.ide.preview.state
  (:require
    [contrib.reactive :as r]
    [reagent.ratom :refer [IReactiveAtom]]))


(deftype FAtom [^:mutable ratom to from]
  IAtom
  IReactiveAtom

  IEquiv
  (-equiv [o other]
    (and (instance? FAtom other)
         (= (.-ratom o) (.-ratom other))
         (= (.-to o) (.-to other))
         (= (.-from o) (.-from other))))

  IDeref
  (-deref [this] @(r/fmap from ratom))

  IReset
  (-reset! [this new-value] (from (-swap! ratom to new-value)))

  ISwap
  (-swap! [this f] (-reset! this (f (from @ratom))))
  (-swap! [this f x] (-reset! this (f (from @ratom) x)))
  (-swap! [this f x y] (-reset! this (f (from @ratom) x y)))
  (-swap! [this f x y more] (-reset! this (apply f (from @ratom) x y more)))

  IWithMeta
  (-with-meta [this meta] (set! ratom (with-meta ratom meta)))

  IMeta
  (-meta [_] (meta ratom))

  IPrintWithWriter
  (-pr-writer [a w opts] (-pr-writer ratom w opts))

  IWatchable
  (-notify-watches [this old new] (-notify-watches ratom old new))
  (-add-watch [this key f] (-add-watch ratom key (fn [k r o n] (f k r (from o) (from n)))))
  (-remove-watch [this key] (-remove-watch ratom key))

  IHash
  (-hash [this] (hash [ratom to from])))
