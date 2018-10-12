(ns hyperfiddle.ide.hf-live
  (:require
    [cats.monad.either :as either]
    [contrib.css :refer [css]]
    [contrib.data :refer [compare-by-index]]
    [contrib.pprint]
    [contrib.reactive :as r]
    [contrib.ui]
    [hypercrud.ui.error :refer [error-comp]]
    [hypercrud.browser.router :as router]
    [hyperfiddle.data :as data]
    [hyperfiddle.ide.fiddles.fiddle-src :as fiddle-src]
    [hyperfiddle.ui :refer [fiddle-api field]]))

; This is an entity in the project namespace in the IDE fiddle-repo, probably
(def attr-order [:fiddle/ident :fiddle/type :fiddle/pull-database :fiddle/pull :fiddle/query
                 :fiddle/renderer :fiddle/css :fiddle/markdown :fiddle/links :fiddle/hydrate-result-as-fiddle])

(defn fiddle-src [attrs initial-tab val ctx-real props]
  [fiddle-src/fiddle-src-renderer val ctx-real (assoc props :embed-mode true :initial-tab initial-tab)]
  #_(let [attrs (or (seq attrs)
                    (clojure.set/intersection
                      (-> @(:hypercrud.browser/data ctx-real)
                          keys
                          (->> (apply sorted-set-by (compare-by-index attr-order)))
                          (disj :db/id))))
          ctx (fiddle-src/shadow-fiddle ctx-real)]
      (fn [val ctx-real props]
        (into
          [:div props]
          (for [k attrs
                :let [?f (fiddle-src/controls k)
                      props (when ?f {:embed-mode true})]]
            ^{:key (str [k])}
            [field [k] ctx ?f props])))))

(defn result-edn [attrs val ctx props]
  (let [s (-> val
              (as-> $ (if (seq attrs) (select-keys $ attrs) $)) ; omit elided fiddle attrs
              (contrib.pprint/pprint-str 50))]
    [contrib.ui/code (assoc props                           ; Class ends up not on the codemirror, todo
                       :value s
                       :read-only true)]))

; This is in source control because all hyperblogs want it.
; But, they also want to tweak it surely. Can we store this with the fiddle ontology?
(defn iframe [ctx props]
  (let [state (r/atom {:edn-fiddle false :edn-result false})]
    (fn [ctx props]
      (let [fiddle-attrs (::fiddle-attrs props)
            props (dissoc props ::fiddle-attrs)]
        [:div.row.hf-live.unp.no-gutters
         (let [as-edn (r/cursor state [:edn-result])
               f (when @as-edn fiddle-api)]
           [:div.result.col-sm
            [:div "Result:" [contrib.ui/easy-checkbox-boolean " EDN?" as-edn {:class "hf-live"}]]
            ; Careful: Reagent deep bug in prop comparison https://github.com/hyperfiddle/hyperfiddle/issues/340
            (let [ctx (if f
                        ctx
                        (dissoc ctx :hyperfiddle.ui.markdown-extensions/unp))]
              [hyperfiddle.ui/iframe ctx (-> props (update :class css "hf-live") (assoc :user-renderer f))])])
         (let [as-edn (r/cursor state [:edn-fiddle])
               f (if @as-edn
                   (r/partial result-edn fiddle-attrs)
                   (r/partial fiddle-src fiddle-attrs (:initial-tab props)))]
           [:div.src.col-sm
            [:div "Interactive Hyperfiddle editor:" [contrib.ui/easy-checkbox-boolean " EDN?" as-edn {:class "hf-live"}]]
            [hyperfiddle.ui/iframe ctx {:class (css (:class props) "devsrc hf-live")
                                        :route (router/assoc-frag (:route props) ":src")
                                        :user-renderer f}]])]))))

(defn browse [rel class ctx props]
  (either/branch
    (data/select+ ctx rel class)
    (error-comp ctx)
    (fn [link-ref]
      (let [props (assoc props :hyperfiddle.ui/custom-iframe iframe)]
        [hyperfiddle.ui/ui-from-link link-ref ctx props]))))
