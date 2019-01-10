(ns hyperfiddle.runtime)


(defprotocol HF-Runtime
  (domain [rt])
  (io [rt]))

(defprotocol State                                          ; internal
  (dispatch! [rt action-or-func])
  (state [rt] [rt path]))
