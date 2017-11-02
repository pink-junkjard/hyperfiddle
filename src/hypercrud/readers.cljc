(ns hypercrud.readers)


(def ->entity #(apply list 'hypercrud.types.ThinEntity/->ThinEntity %))
(def URI #(list 'hypercrud.types.URI/->URI %))
