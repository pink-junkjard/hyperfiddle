(ns hypercrud.state.hydrating-action-batched                ; browser
  (:require [cats.core :refer [mlet]]
            [hypercrud.state.core :as state]
            [hypercrud.util.base-64-url-safe :as base-64-url-safe]
            [hypercrud.types.URI]
            [kvlt.core :as kvlt]
            [promesa.core :as p]))


; todo the *service-uri* binding should be used instead,
; but node api and service endpoints are different still
; https://github.com/hyperfiddle/hyperfiddle/issues/96
(def service-uri #uri "/")

(defn local-hydrate! [root-rel-path local-basis stage-val]
  (let [local-basis-encoded ((comp base-64-url-safe/encode pr-str) local-basis)
        foo "page"
        ; todo what about the rest of the state?
        ; display-mode impacts bindings & request-fn
        ; popovers will eventually need to be included
        ; is the tempid lookup needed?
        req (merge {:url (str (.-uri-str service-uri) "local-hydrate/" local-basis-encoded "/" foo root-rel-path)
                    :accept :application/transit+json :as :auto}
                   (if (empty? stage-val)
                     {:method :get}                         ; Try to hit CDN
                     {:method :post
                      :form stage-val                       ; UI-facing interface is stage-val
                      :content-type :application/transit+json}))]
    (-> (kvlt/request! req) (p/then :body))))

(defn local-basis! [global-basis foo root-rel-path stage-val]
  (let [global-basis' ((comp base-64-url-safe/encode pr-str) global-basis)
        req (merge {:url (str (.-uri-str service-uri) "local-basis/" global-basis' "/" foo root-rel-path)
                    :accept :application/transit+json :as :auto}
                   (if (empty? stage-val)
                     {:method :get}                         ; Try to hit CDN
                     {:method :post
                      :form stage-val
                      :content-type :application/transit+json}))]
    (-> (kvlt/request! req) (p/then :body))))

; batch doesn't make sense with thunks (can be sync or async dispatches in a thunk),
; user beware
(defn batch [& action-list] (cons :batch action-list))      ; WARNING copy pasted

; browser
(defn hydrating-action-batched [{:keys [on-start]} dispatch! get-state]
  (dispatch! (apply batch [:hydrate!-start (js/Math.random)] (if on-start (on-start get-state))))
  (let [{:keys [stage encoded-route] :as state} (get-state)]
    (mlet [local-basis (local-basis! state/*global-basis* "page" encoded-route #_"has leading slash" stage)]
      (-> (local-hydrate! encoded-route local-basis stage)
          (p/then (fn [{:keys [pulled-trees-map id->tempid]}]
                    (dispatch! [:batch
                                [:set-ptm pulled-trees-map id->tempid]
                                [:hydrate!-success]])))
          (p/catch #(dispatch! [:hydrate!-failure %])))))
  nil)
