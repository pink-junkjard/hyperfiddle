(ns hypercrud.types.DbVal)

; dbval has a stage
; dbref has a branch-name and is interpreted in context of a stage-val

; dbref goes to server because there is a stage-val alongside it, so the dbval can be decided
; dbval has been hydrated and has a tempid map. Used by Entity to do tempid reversal for URLs

; Server contract - send a DBRef UP,with stage, and DBVal down....
; (Or, really, equivalent. Construct the DBVal on the Client, from the DBRef and the tempidmap.
; Server sends down Entities, which have pointer to DBVal, all the same one, which has pointer to staging-val,
; and transit needs to make that space efficient.

(defrecord DbVal [uri branch #_history?])
