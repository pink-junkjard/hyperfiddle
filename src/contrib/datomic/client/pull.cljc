(ns contrib.datomic.client.pull
  (:require
    [clojure.spec.alpha :as s]))


; https://docs.datomic.com/cloud/query/query-pull.html

; pattern            = [attr-spec+]
(s/def ::pattern (s/+ ::attr-spec))

; attr-spec          = attr-name | wildcard | map-spec | attr-with-opts | attr-expr
(s/def ::attr-spec (s/or :attr-name ::attr-name
                         :wildcard ::wildcard
                         :map-spec ::map-spec
                         :attr-with-opts ::attr-with-opts
                         :attr-expr ::attr-expr))

; attr-name          = an edn keyword that names an attr
(s/def ::attr-name keyword?)

; map-spec           = { ((attr-name | attr-with-opts | attr-expr) (pattern | recursion-limit))+ }
(s/def ::map-spec (s/map-of (s/or :attr-name ::attr-name
                                  :attr-with-opts ::attr-with-opts
                                  :attr-expr ::attr-expr)
                            (s/or :pattern ::pattern
                                  :recursion-limit ::recursion-limit)
                            :min-count 1))

; attr-with-opts     = [attr-name attr-options+]
(s/def ::attr-with-opts (s/cat :attr-name ::attr-name
                               :attr-options (s/+ ::attr-options)))

; attr-options       = :as any-value | :limit (positive-number | nil) | :default any-value
(s/def ::attr-options (s/alt :as (s/cat :option #{:as}
                                        :any-value any?)
                             :limit (s/cat :option #{:limit}
                                           :limit (s/nilable pos-int?))
                             :default (s/cat :option #{:default}
                                             :any-value any?)))

; wildcard           = '*'
(s/def ::wildcard #{'*})

; recursion-limit    = positive-number | '...'
(s/def ::recursion-limit (s/or :positive-number pos-int?
                               :ellipsis #{'...}))

; attr-expr          = limit-expr | default-expr
(s/def ::attr-expr (s/or :limit-expr ::limit-expr
                         :default-expr ::default-expr))

; limit-expr         = [("limit" | 'limit') attr-name (positive-number | nil)]
(s/def ::limit-expr (s/cat :expr-name #{"limit" 'limit}
                           :attr-name ::attr-name
                           :limit (s/nilable pos-int?)))

; default-expr       = [("default" | 'default') attr-name any-value]
(s/def ::default-expr (s/cat :expr-name #{"default" 'default}
                             :attr-name ::attr-name
                             :any-value any?))
