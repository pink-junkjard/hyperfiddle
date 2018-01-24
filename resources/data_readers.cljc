{entity hypercrud.readers/entity
 uri hypercrud.readers/uri

 ; defrecords get these for free in clj
 hypercrud.types.DbVal.DbVal hypercrud.types.DbVal/read-DbVal
 hypercrud.types.EntityRequest.EntityRequest hypercrud.types.EntityRequest/read-EntityRequest
 hypercrud.types.Err.Err hypercrud.types.Err/read-Err
 hypercrud.types.QueryRequest.QueryRequest hypercrud.types.QueryRequest/read-QueryRequest}