(ns hyperfiddle.ide.fiddles.fiddle-src
  (:require [cuerdas.core :as str]
            [contrib.reactive :as r]
            [contrib.reagent :refer [fragment]]
            [hypercrud.types.URI :refer [is-uri?]]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hyperfiddle.ui :refer [markdown]]
            [hypercrud.ui.attribute.edn :refer [edn-many]]
            [hypercrud.ui.form :refer [form-cell]]
            [contrib.ui.tooltip :refer [tooltip]]
            [hyperfiddle.ide.fiddles.topnav :refer [shadow-fiddle]]))


(defn schema-links [ctx]
  (->> (-> ctx :target-domain :domain/environment)
       (filter (fn [[k v]] (and (str/starts-with? k "$") (is-uri? v))))
       sort
       (map (fn [[$db _]]
              (let [props {:route [(keyword "hyperfiddle.schema" $db)]
                           :target "_blank"}]
                ^{:key $db}
                [(:navigate-cmp ctx) props $db])))
       (doall)))

(defn control-with-unders [frag value ctx props]
  [:div [(auto-control ctx) value ctx props] frag])

(def underdocs
  {:fiddle/pull "See [:fiddle/pull examples](http://www.hyperfiddle.net/:docs!fiddle-pull/) and the
  [Datomic pull docs](https://docs.datomic.com/on-prem/pull.html)."
   :fiddle/query "See [:fiddle/query examples](http://www.hyperfiddle.net/:docs!fiddle-query/) and the
   [Datomic query docs](https://docs.datomic.com/on-prem/query.html)."
   :fiddle/markdown "See [:fiddle/markdown examples](http://www.hyperfiddle.net/:docs!fiddle-markdown/)."
   :fiddle/css "See [:fiddle/css examples](http://www.hyperfiddle.net/:docs!fiddle-css/)."
   :fiddle/renderer "See [:fiddle/renderer examples](http://www.hyperfiddle.net/:docs!fiddle-renderer/). `ctx` and
   `class` are in lexical scope. No `(ns (:require ...))` yet so vars must be fully qualified."
   :fiddle/links "See [:fiddle/links examples](http://www.hyperfiddle.net/:docs!fiddle-links/)."})

(defn fiddle-src-renderer [ctx-real class & {:keys [embed-mode]}]
  (let [ctx-real (dissoc ctx-real :user-renderer)           ; this needs to not escape this level; inline links can't ever get it
        ctx (shadow-fiddle ctx-real)
        controls
        {:fiddle/pull (r/partial control-with-unders (fragment :_ [:span.schema "schema: " (schema-links ctx)] [:div.hf-underdoc [markdown (:fiddle/pull underdocs)]]))
         :fiddle/query (r/partial control-with-unders (fragment :_ [:span.schema "schema: " (schema-links ctx)] [:div.hf-underdoc [markdown (:fiddle/query underdocs)]]))
         :fiddle/markdown (r/partial control-with-unders [:div.hf-underdoc [markdown (:fiddle/markdown underdocs)]])
         :fiddle/css (r/partial control-with-unders [:div.hf-underdoc [markdown (:fiddle/css underdocs)]])
         :fiddle/renderer (r/partial control-with-unders [:div.hf-underdoc [markdown (:fiddle/renderer underdocs)]])
         :fiddle/links (r/partial control-with-unders [:div.hf-underdoc [markdown (:fiddle/links underdocs)]])
         }]
    [:div.fiddle-src {:class class}
     [:h3 (str @(r/cursor (:hypercrud.browser/result ctx) [:fiddle/ident])) " source"]
     [(:cell ctx) [true 0 :fiddle/ident] ctx]
     [(:cell ctx) [true 0 :fiddle/type] ctx]
     (case @(r/cursor (:hypercrud.browser/result ctx) [:fiddle/type])
       :entity (fragment :_ [(:cell ctx) [true 0 :fiddle/pull-database] ctx]
                         [(:cell ctx) [true 0 :fiddle/pull] ctx (controls :fiddle/pull)])
       :query [(:cell ctx) [true 0 :fiddle/query] ctx (controls :fiddle/query)]
       :blank nil
       nil nil)
     [(:cell ctx) [true 0 :fiddle/markdown] ctx (controls :fiddle/markdown)]
     [(:cell ctx) [true 0 :fiddle/css] ctx (controls :fiddle/css)]
     [(:cell ctx) [true 0 :fiddle/renderer] ctx (controls :fiddle/renderer)]
     (when-not embed-mode [(:cell ctx) [true 0 :fiddle/links] ctx-real (controls :fiddle/links)])
     (when-not embed-mode [(:anchor ctx) :hyperfiddle/remove [0] ctx "Remove fiddle"])
     ]))

(defn hacked-links [value ctx props]
  [:div
   [:pre (->> value
              (remove :link/disabled?)
              (map #(select-keys % [:link/rel :link/path :link/fiddle :link/render-inline? :link/formula]))
              (map #(update % :link/fiddle :fiddle/ident))
              (map pr-str)
              (interpose "\n"))]
   [:div.hf-underdoc [markdown "Due to an issue in the live embed, the links are shown as EDN until we fix it."]]])

(defn docs-embed [& attrs]
  (fn [ctx-real class & {:keys [embed-mode]}]
    (let [ctx-real (dissoc ctx-real :user-renderer)         ; this needs to not escape this level; inline links can't ever get it
          ctx (shadow-fiddle ctx-real)
          {:keys [:fiddle/ident]} @(:hypercrud.browser/result ctx)
          controls
          {:fiddle/pull (r/partial control-with-unders (fragment :_ [:span.schema "schema: " (schema-links ctx)]))
           :fiddle/query (r/partial control-with-unders (fragment :_ [:span.schema "schema: " (schema-links ctx)]))
           :fiddle/links hacked-links}]
      (fn [ctx-real class & {:keys [embed-mode]}]
        (into
          [:div.fiddle-src {:class class}]
          (for [k attrs]
            [(:cell ctx) [true 0 k] ctx (controls k)]))))))

; This is in source control because all hyperblogs want it.
; But, they also want to tweak it surely. Can we store this with the fiddle ontology?
(defn ^:export hyperfiddle-live [rel ctx & fiddle-attrs]
  [:div.hyperfiddle-live-editor.unp
   [:div.row
    ; Reverse order so it looks right on mobile, larger views reorder
    [:div.col-sm-5.col-sm-push-7
     [:div "Result:"]
     ((:browse ctx) rel [] ctx)]
    [:div.col-sm-7.col-sm-pull-5
     [:div "Interactive Hyperfiddle editor:"]
     ((:browse ctx) rel [] ctx (apply docs-embed fiddle-attrs) :frag ":src" :class "devsrc")]
    ]])
