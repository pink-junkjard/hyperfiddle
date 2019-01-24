(ns hyperfiddle.runtime)


(defprotocol HF-Runtime
  (domain [rt])
  (io [rt]))

(defprotocol State                                          ; internal
  (dispatch! [rt action-or-func])
  (state [rt] [rt path]))

(defn attribute-renderer [rt branch ident] @(state rt [::partitions branch :attr-renderers ident]))
