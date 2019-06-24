(ns contrib.datomic.client.query
  (:require
    [clojure.spec.alpha :as s]
    [clojure.string :as string]
    [contrib.datomic.client.pull :as pull]))


; https://docs.datomic.com/cloud/query/query-data-reference.html

(defmulti query-form (fn [q]
                       (cond
                         (map? q) :map
                         (sequential? q) :list)))
(defmethod query-form :list [_] ::query-list)
(defmethod query-form :map [_] ::query-map)

(s/def :contrib.datomic.client/query (s/multi-spec query-form (fn [gv _] gv)))

; query                      = [find-spec return-map-spec? with-clause? inputs? where-clauses?]
; todo query lists are actually order agnostic
; todo spec for: "At least one of inputs or where-clauses must be specified."
(s/def ::query-list (s/cat :find-spec ::find-spec
                           :return-map-spec (s/? ::return-map-spec)
                           :with (s/? ::with-clause)
                           :in (s/? ::inputs)
                           :where (s/? ::where-clauses)))

; find-spec                  = ':find' find-rel
(s/def ::find-spec (s/cat :find #{:find}
                          :find-rel ::find-rel))

; find-rel                   = find-elem+
(s/def ::find-rel (s/+ ::find-elem))

; return-map-spec            = (return-keys | return-syms | return-strs)
(s/def ::return-map-spec (s/alt :return-keys ::return-keys
                                :return-syms ::return-syms
                                :return-strs ::return-strs))

; return-keys                = ':keys' symbol+
(s/def ::return-keys (s/cat :keys #{:keys}
                            :symbols (s/+ symbol?)))

; return-syms                = ':syms' symbol+
(s/def ::return-syms (s/cat :syms #{:syms}
                            :symbols (s/+ symbol?)))

; return-strs                = ':strs' symbol+
(s/def ::return-strs (s/cat :str #{:strs}
                            :symbols (s/+ symbol?)))

; find-elem                  = (variable | pull-expr | aggregate)
(s/def ::find-elem (s/or :variable ::variable
                         :pull-expr ::pull-expr
                         :aggregate ::aggregate))

; extracted from public grammar @2019-06-20
; pull-expr                  = ['pull' variable pattern]
; deviating to
; pull-expr                  = ['pull' ?src-var variable pattern]
(s/def ::pull-expr (s/cat :pull #{'pull}
                          :src-var (s/? ::src-var)
                          :variable ::variable
                          :pattern ::pattern))

; pattern                    = (input-name | pattern-data-literal)
(s/def ::pattern (s/or :input-name ::input-name
                       :pattern-data-literal ::pull/pattern))

; aggregate                  = [aggregate-fn-name fn-arg+]
(s/def ::aggregate (s/cat :aggregate-fn-name ::aggregate-fn-name
                          :fn-args (s/+ ::fn-arg)))

(s/def ::aggregate-fn-name symbol?)

; fn-arg                     = (variable | constant | src-var)
(s/def ::fn-arg (s/or :variable ::variable
                      :constant ::constant
                      :src-var ::src-var))

; with-clause                = ':with' variable+
(s/def ::with-clause (s/cat :with #{:with}
                            :variables (s/+ ::variable)))

; where-clauses              = ':where' clause+
(s/def ::where-clauses (s/cat :where #{:where}
                              :clauses (s/+ ::clause)))

; inputs                     = ':in' (src-var | variable | binding | rules-var)+
; extracting to              = ':in' input+
(s/def ::inputs (s/cat :in #{:in}
                       :inputs (s/+ ::input)))

; extracted from public grammar @2019-06-20
; input                      = (src-var | variable | binding | rules-var)
; deviating to
; input                      = (src-var | variable | binding | rules-var | pattern-name )
(s/def ::input (s/or :src-var ::src-var
                     :variable ::variable
                     :binding ::binding
                     :rules-var ::rules-var
                     :pattern-name ::pattern-name))

; undefined in public grammar @2019-06-20
(s/def ::input-name ::input)

; src-var                    = symbol starting with "$"
(s/def ::src-var (s/and symbol? #(string/starts-with? (str %) "$")))

; variable                   = symbol starting with "?"
(s/def ::variable (s/and symbol? #(string/starts-with? (str %) "?")))

; rules-var                  = the symbol "%"
(s/def ::rules-var #{'%})

; plain-symbol               = symbol that does not begin with "$" or "?"
(s/def ::plain-symbol (s/and symbol?
                             #(let [s (str %)]
                                (not (or (string/starts-with? "?" s)
                                         (string/starts-with? "$" s))))))

; undefined in public grammar @2019-06-20
(s/def ::pattern-name ::plain-symbol)

; and-clause                 = [ 'and' clause+ ]
(s/def ::and-clause (s/cat :and #{'and}
                           :clauses (s/+ ::clause)))

; expression-clause          = (data-pattern | pred-expr | fn-expr | rule-expr)
(s/def ::expression-clause (s/or :data-pattern ::data-pattern
                                 :pred-expr ::pred-expr
                                 :fn-expr ::fn-expr
                                 :rule-expr ::rule-expr))

; rule-expr                  = [ src-var? rule-name (variable | constant | '_')+]
(s/def ::rule-expr (s/cat :src-var (s/? ::src-var)
                          :rule-name ::rule-name
                          :rule-args (s/+ (s/alt :variable ::variable
                                                 :constant ::constant
                                                 :underscore #{'_}))))

; not-clause                 = [ src-var? 'not' clause+ ]
(s/def ::not-clause (s/cat :src-var (s/? ::src-var)
                           :not #{'not}
                           :clauses (s/+ ::clause)))

; not-join-clause            = [ src-var? 'not-join' [variable+] clause+ ]
(s/def ::not-join-clause (s/cat :src-var (s/? ::src-var)
                                :not-join #{'not-join}
                                :variables (s/spec (s/+ ::variable))
                                :clauses (s/+ ::clause)))

; or-clause                  = [ src-var? 'or' (clause | and-clause)+]
(s/def ::or-clause (s/cat :src-var (s/? ::src-var)
                          :or #{'or}
                          :clauses (s/+ (s/alt :clause ::clause
                                               :and-clause (s/spec ::and-clause)))))

; or-join-clause             = [ src-var? 'or-join' [variable+] (clause | and-clause)+ ]
(s/def ::or-join-clause (s/cat :src-var (s/? ::src-var)
                               :or-join #{'or-join}
                               :variables (s/spec (s/+ ::variable))
                               :clauses (s/+ (s/alt :clause ::clause
                                                    :and-clause (s/spec ::and-clause)))))

; rule-vars                  = [variable+ | ([variable+] variable*)]
(s/def ::rule-vars (s/alt :variables (s/+ ::variable)
                          :required-bindings (s/cat :bound (s/spec (s/+ ::variable))
                                                    :unbound (s/* ::variable))))

; clause                     = (not-clause | not-join-clause | or-clause | or-join-clause | expression-clause)
(s/def ::clause (s/or :not-clause ::not-clause
                      :not-join-clause ::not-join-clause
                      :or-clause ::or-clause
                      :or-join-clause ::or-join-clause
                      :expression-clause ::expression-clause))

; data-pattern               = [ src-var? (variable | constant | '_')+ ]
(s/def ::data-pattern (s/cat :src-var (s/? ::src-var)
                             :elements (s/+ (s/alt :variable ::variable
                                                   :constant ::constant
                                                   :underscore #{'_}))))

; constant                   = any non-variable data literal
; going by https://clojure.org/reference/reader#_literals
(s/def ::constant (s/or :string string?
                        :number number?
                        :char char?
                        :nil nil?
                        :boolean boolean?
                        :keyword keyword?))

; undefined in public grammar @2019-06-20; described as:
; "A predicate is an arbitrary Java or Clojure function"
(s/def ::pred ifn?)                                         ; unresolved functions are just symbols and symbol implements IFn

; deviating from public grammar @2019-06-20
; pred-expr                  = [ [pred fn-arg+] ]
; to
; pred-expr                  = [ [pred fn-arg*] ]
(s/def ::pred-expr (s/cat :invocation (s/spec (s/cat :pred ::pred
                                                     :fn-args (s/* ::fn-arg)))))

; deviating from public grammar @2019-06-20
; fn-expr                    = [ [fn fn-arg+] binding]
; to
; fn-expr                    = [ [fn fn-arg*] binding]
(s/def ::fn-expr (s/cat :invocation (s/spec (s/cat :fn ifn? ; unresolved functions are just symbols and symbol implements IFn
                                                   :fn-args (s/* ::fn-arg)))
                        :binding ::binding))

; binding                    = (bind-scalar | bind-tuple | bind-coll | bind-rel)
(s/def ::binding (s/or :bind-scalar ::bind-scalar
                       :bind-tuple ::bind-tuple
                       :bind-coll ::bind-coll
                       :bind-rel ::bind-rel))

; bind-scalar                = variable
(s/def ::bind-scalar ::variable)

; bind-tuple                 = [ (variable | '_')+]
(s/def ::bind-tuple (s/+ (s/alt :variable ::variable
                                :underscore #{'_})))

; bind-coll                  = [variable '...']
(s/def ::bind-coll (s/cat :variable ::variable
                          :ellipsis #{'...}))

; bind-rel                   = [ [(variable | '_')+] ]
(s/def ::bind-rel (s/coll-of (s/+ (s/alt :variable ::variable
                                         :underscore #{'_}))))

; rule-head                  = [rule-name rule-vars]
(s/def ::rule-head (s/cat :rule-name ::rule-name
                          :rule-vars ::rule-vars))

; deviating from public grammar @2019-06-20
; rule                       = [ [rule-head clause+]+ ]
; to
; rule                       = [rule-head clause+]
(s/def ::rule (s/cat :head (s/spec ::rule-head)
                     :clauses (s/+ ::clause)))

; undefined
; extracted from public grammar @2019-06-20
; rules                      = [ rule+ ]
(s/def ::rules (s/cat :rules (s/+ (s/spec ::rule))))

; rule-name                  = plain-symbol
(s/def ::rule-name ::plain-symbol)

; todo spec for: "At least one of inputs or where-clauses must be specified."
(s/def ::query-map (s/keys :req-un [::find]
                           :opt-un [::keys
                                    ::syms
                                    ::strs
                                    ::with
                                    ::in
                                    ::where]))

(s/def ::find ::find-rel)
(s/def ::keys (s/+ symbol?))
(s/def ::syms (s/+ symbol?))
(s/def ::strs (s/+ symbol?))
(s/def ::with (s/+ ::variable))
(s/def ::in (s/+ ::input-name))
(s/def ::where (s/+ ::clause))
