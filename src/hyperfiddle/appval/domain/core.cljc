(ns hyperfiddle.appval.domain.core
  (:require [clojure.string :as string]
            [cuerdas.core :as cuerdas]
            [hypercrud.client.core :as hc]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.URI]))


(def domain-uri #uri "datomic:free://datomic:4334/domains")
(def root-uri #uri "datomic:free://datomic:4334/root")      ; ide
(def auth0-redirect-path "/auth0")                          ; ide

(defn hostname->hf-domain-name [hostname hyperfiddle-hostname]
  ; buggy
  (string/replace hostname (str "." hyperfiddle-hostname) ""))

(defn alias? [hf-domain-name]
  (string/includes? hf-domain-name "."))

(defn domain-request [hf-domain-name peer]
  (let [e (if (alias? hf-domain-name)
            [:domain/aliases hf-domain-name]
            [:domain/ident hf-domain-name])]
    (->EntityRequest e nil
                     (hc/db peer domain-uri nil)
                     [:db/id :domain/ident :domain/home-route :domain/aliases
                      {:domain/code-databases [:db/id :dbhole/name :dbhole/uri :repository/environment]}])))

(defn user-profile->ident [user-profile]
  (-> user-profile :email (cuerdas/replace #"\@.+$" "") (cuerdas/slug)))
