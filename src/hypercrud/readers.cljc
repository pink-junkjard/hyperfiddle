(ns hypercrud.readers)


(def DbVal #(apply list 'hypercrud.types.DbVal/->DbVal %))
(def Entity #(apply list 'hypercrud.types.Entity/->Entity %))
(def ->entity #(apply list 'hypercrud.types.Entity/->ThinEntity %))
(def URI #(list 'hypercrud.types.URI/->URI %))
