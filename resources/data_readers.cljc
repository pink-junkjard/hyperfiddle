{
 dbname hyperfiddle.readers/dbname
 entity hyperfiddle.readers/entity
 uri hyperfiddle.readers/uri

 ; defrecords get these for free in clj
 hypercrud.types.DbRef.DbRef hypercrud.types.DbRef/map->DbRef
 hypercrud.types.DbVal.DbVal hypercrud.types.DbVal/read-DbVal
 hypercrud.types.EntityRequest.EntityRequest hypercrud.types.EntityRequest/read-EntityRequest
 hypercrud.types.Err.Err hypercrud.types.Err/read-Err
 hypercrud.types.QueryRequest.QueryRequest hypercrud.types.QueryRequest/read-QueryRequest}