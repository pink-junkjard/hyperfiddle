(ns hyperfiddle.ide.fiddles.topnav
  (:require [hyperfiddle.ide.fiddles.topnav-bindings :as topnav-bindings]))


(defn bindings [ctx] (topnav-bindings/bindings ctx))
