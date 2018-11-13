(ns hyperfiddle.route
  (:require
    [cats.monad.either :as either]
    [clojure.spec.alpha :as s]
    [hyperfiddle.fiddle]                                    ; for ::fiddle spec
    ))


(s/def ::fiddle (s/or
                  :ident :fiddle/ident
                  ;:dbid ...
                  ;:lookup ...
                  :uuid :fiddle/uuid))
(s/def ::datomic-args (s/and seqable? seq))
(s/def ::service-args some?)
(s/def ::fragment some?)

(s/def :hyperfiddle/route
  ; todo routes should be maps #243 and errors become sensible
  #_(s/keys
      :req [::fiddle]
      :opt [::datomic-args ::service-args ::fragment])
  (s/or
    :a (s/tuple ::fiddle)
    :b (s/tuple ::fiddle ::datomic-args)
    :c (s/tuple ::fiddle ::datomic-args ::service-args)
    :d (s/tuple ::fiddle ::datomic-args ::service-args ::fragment)))

(defn validate-route+ [route]
  (if (s/valid? :hyperfiddle/route route)
    (either/right route)
    (either/left (ex-info (str "Invalid route\n" (s/explain-str :hyperfiddle/route route))
                          (s/explain-data :hyperfiddle/route route)))))
