(ns hyperfiddle.ide.preview.io
  (:require
    [goog.object :as object]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.http-client :as http-client]
    [hyperfiddle.io.local-basis :as local-basis]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(defn handle-401 [domain e]
  (if (= 401 (:status (ex-data e)))
    (do
      (timbre/error e)
      ; just blast the window location to force a refresh
      (object/set js/window "location" (domain/url-encode domain [:hyperfiddle.system/unauthorized])))
    (throw e)))

(deftype IOImpl [domain]
  io/IO
  (global-basis [io]
    (-> (http-client/global-basis! domain)
        (p/catch (partial handle-401 domain))))

  (local-basis [io global-basis route]
    (p/resolved (local-basis/local-basis io global-basis route)))

  (hydrate-route [io local-basis route branch stage]
    (-> (http-client/hydrate-route! domain local-basis route branch stage)
        (p/catch (partial handle-401 domain))))

  (hydrate-requests [io local-basis staged-branches requests]
    (-> (http-client/hydrate-requests! domain local-basis staged-branches requests)
        (p/catch (partial handle-401 domain))))

  (sync [io dbnames]
    (-> (http-client/sync! domain dbnames)
        (p/catch (partial handle-401 domain))))

  (transact! [io tx-groups]
    (-> (http-client/transact! domain tx-groups)
        (p/catch (partial handle-401 domain))))

  IEquiv
  (-equiv [o other]
    (and (instance? IOImpl other) (= (.-domain o) (.-domain other))))

  IHash
  (-hash [this] (hash [domain])))
