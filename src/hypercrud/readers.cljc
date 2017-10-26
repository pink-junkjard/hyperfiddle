(ns hypercrud.readers)


(def DbError #(list 'hypercrud.types.DbError/->DbError %))
(def DbVal #(apply list 'hypercrud.types.DbVal/->DbVal %))
(def Entity #(apply list 'hypercrud.types.Entity/->Entity %))
(def ->entity #(apply list 'hypercrud.types.Entity/->ThinEntity %))
(def EReq #(apply list 'hypercrud.types.EntityRequest/->EntityRequest %))
(def QReq #(apply list 'hypercrud.types.QueryRequest/->QueryRequest %))
(def URI #(list 'hypercrud.types.URI/->URI %))
