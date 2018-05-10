(ns hyperfiddle.ide.fiddles.fiddle-src
  (:require [cuerdas.core :as str]
            [contrib.reactive :as r]
            [contrib.reagent :refer [fragment]]
            [hypercrud.types.URI :refer [is-uri?]]
            [hypercrud.ui.auto-control :refer [auto-control']]
            [hypercrud.ui.control.markdown-rendered :refer [markdown]]
            [hypercrud.ui.form :refer [form-cell]]
            [hypercrud.ui.tooltip :refer [tooltip]]
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

(defn control-with-unders [frag field props ctx]
  (let [control [(auto-control' ctx) field props ctx]]
    [:div control frag]))

(defn cell-wrap [control ctx]
  (let [control (or control (auto-control' ctx))]
    [form-cell control (:hypercrud.browser/field ctx) ctx]))

(def underdocs
  {:fiddle/pull "`:db/id` is currently required. No nested pull yet. Just keep it simple for now. See
  [Datomic pull docs](https://docs.datomic.com/on-prem/pull.html)."
   :fiddle/query "See [:fiddle/query examples](http://www.hyperfiddle.net/:docs!fiddle-query/) and
   the [Datomic query docs](https://docs.datomic.com/on-prem/query.html). If you pull, `:db/id` is
   required. No rules yet, no nested pull yet, no d/log or d/history yet."
   :fiddle/markdown "See [:fiddle/markdown examples](http://www.hyperfiddle.net/:docs!fiddle-markdown/)."
   :fiddle/css "See [:fiddle/css examples](http://www.hyperfiddle.net/:docs!fiddle-css/)."
   :fiddle/renderer "See [:fiddle/renderer examples](http://www.hyperfiddle.net/:docs!fiddle-renderer/)."
   :fiddle/links "See [:fiddle/links examples](http://www.hyperfiddle.net/:docs!fiddle-links/)."})

(defn fiddle-src-renderer [ctx-real class & {:keys [embed-mode]}]
  (let [ctx-real (dissoc ctx-real :user-renderer)           ; this needs to not escape this level; inline links can't ever get it
        ctx (shadow-fiddle ctx-real)
        {:keys [:fiddle/type :fiddle/ident]} @(:hypercrud.browser/result ctx)
        controls
        {:fiddle/pull (r/partial cell-wrap (r/partial control-with-unders (fragment :_ [:span.schema "schema: " (schema-links ctx)] [markdown (:fiddle/pull underdocs)])))
         :fiddle/query (r/partial cell-wrap (r/partial control-with-unders (fragment :_ [:span.schema "schema: " (schema-links ctx)] [markdown (:fiddle/query underdocs)])))
         :fiddle/markdown (r/partial cell-wrap (r/partial control-with-unders [markdown (:fiddle/markdown underdocs)]))
         :fiddle/css (r/partial cell-wrap (r/partial control-with-unders [markdown (:fiddle/css underdocs)]))
         :fiddle/renderer (r/partial cell-wrap (r/partial control-with-unders [markdown (:fiddle/renderer underdocs)]))
         :fiddle/links (r/partial cell-wrap (r/partial control-with-unders [markdown (:fiddle/links underdocs)]))
         }]
    (fn [ctx-real class & {:keys [embed-mode]}]
      [:div.fiddle-src {:class class}
       [:h3 (str ident) " source"]
       ((:cell ctx) [true 0 :fiddle/ident] ctx)
       ((:cell ctx) [true 0 :fiddle/type] ctx)
       (case type
         :entity ((:cell ctx) [true 0 :fiddle/pull] ctx (controls :fiddle/pull))
         :query ((:cell ctx) [true 0 :fiddle/query] ctx (controls :fiddle/query))
         :blank nil
         nil nil)
       ((:cell ctx) [true 0 :fiddle/markdown] ctx (controls :fiddle/markdown))
       ((:cell ctx) [true 0 :fiddle/css] ctx (controls :fiddle/css))
       ((:cell ctx) [true 0 :fiddle/renderer] ctx (controls :fiddle/renderer))
       (when-not embed-mode ((:cell ctx) [true 0 :fiddle/links] ctx-real (controls :fiddle/links)))
       ((:cell ctx) [true 0 :fiddle/entrypoint?] ctx)
       (when-not embed-mode ((:anchor ctx) :hyperfiddle/remove [0] ctx "Remove fiddle"))
       (when-not embed-mode ((:browse ctx-real) :attribute-renderers [] ctx-real))
       ])))

(defn docs-embed [& attrs]
  (fn [ctx-real class & {:keys [embed-mode]}]
    (let [ctx-real (dissoc ctx-real :user-renderer)         ; this needs to not escape this level; inline links can't ever get it
          ctx (shadow-fiddle ctx-real)
          {:keys [:fiddle/ident]} @(:hypercrud.browser/result ctx)
          controls
          {:fiddle/pull (r/partial cell-wrap (r/partial control-with-unders (fragment :_ [:span.schema "schema: " (schema-links ctx)] [markdown (:fiddle/pull underdocs)])))
           :fiddle/query (r/partial cell-wrap (r/partial control-with-unders (fragment :_ [:span.schema "schema: " (schema-links ctx)] [markdown (:fiddle/query underdocs)])))
           :fiddle/markdown (r/partial cell-wrap (r/partial control-with-unders [markdown (:fiddle/markdown underdocs)]))
           :fiddle/css (r/partial cell-wrap (r/partial control-with-unders [markdown (:fiddle/css underdocs)]))
           :fiddle/renderer (r/partial cell-wrap (r/partial control-with-unders [markdown (:fiddle/renderer underdocs)]))
           :fiddle/links (r/partial cell-wrap (r/partial control-with-unders [markdown (:fiddle/links underdocs)]))
           }]
      (fn [ctx-real class & {:keys [embed-mode]}]
        (into
          [:div.fiddle-src {:class class}
           #_[:h3 (str ident) " source"]]
          (for [k attrs]
            ((:cell ctx) [true 0 k] ctx (controls k))))))))
