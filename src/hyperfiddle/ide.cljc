(ns hyperfiddle.ide
  (:require hyperfiddle.appval.domain.foundation
    #?(:cljs [hypercrud.browser.core :as browser])
    #?(:cljs [hyperfiddle.appval.domain.foundation-view :as foundation-view])))



(def domain {})

(defn ide-route [ctx]
  ; Depends on the parsed browser location which happens inside app-ui/ui
  ; We can probably invert the logic so that we can do this from the outside.
  (let [target-user-fiddle (get-in ctx [:target-route :link-id])]
    {:code-database "root"
     :link-id :hyperfiddle/main
     :entity #entity["$" target-user-fiddle]}))

; Users can't bypass the foundation.

#?(:cljs
   (defn view [ctx]
     (browser/ui-from-route (ide-route ctx) ctx)))

(defn api [foo ctx]
  (browser/request-from-route (ide-route ctx) ctx))



; Ide-basis wraps user-basis
; ide-basis assumes a browser in userland
; ide-basis can just be global-basis if userland is a function
(defn local-basis [foo global-basis domain route ctx]
  ; Given all the reachable dbs, return only from this route.
  ; If hc browser, we can prune a lot.
  ; If userland is a fn, local-basis is global-basis (minus domain)
  ; The foundation will concat on the domain.

  (let [{:keys [domain ide user]} global-basis
        ; basis-maps: List[Map[uri, t]]

        ; This is browser's local-basis.

        ; userland needs to optimize the basis. 1) by running the api-fn and see what is queried. 2) configure a predicate, hyperfiddle.ide/local-basis
        ; Appfn can have local-basis, it defaults to global-basis. They'd have to code it since there's no link structure hints.

        user-basis (get user (:code-database route))        ; hardcoded browser route knowledge
        topnav-basis (get user (:code-database foo))        ; todo the only repo uri is needed from user. dont need the environment as well
        basis-maps (condp = (:type foo)
                     ; everybody using foundation has domain - even the IDE's new-anchor popover needs to do a subdomain lookup on the server side
                     "page" (concat [user-basis] (vals ide))
                     "ide" (concat [topnav-basis] (vals ide))
                     "user" [user-basis])
        local-basis (->> basis-maps                         ; Userland api-fn should filter irrelevant routes
                         (apply concat)
                         (apply concat)
                         (apply sorted-map))]

    ; Local-basis is for the subset of databases visible now for this fiddle.
    ; Does not include popovers or navigates, they have their own local basis.
    ; In the future, we can do better than reachable-basis if we hydrate and see what came out.
    #_(hydrate-route rt hostname foo state-val)
    #_(determine-local-basis)
    local-basis))
