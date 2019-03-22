(ns hyperfiddle.ide.edit
  (:require
    [contrib.css :refer [css]]))


(defn view [_ ctx props]
  [:<>
   (let [ctx (hyperfiddle.data/browse ctx :hyperfiddle/topnav)]
     [hyperfiddle.ide.fiddles.topnav/renderer nil ctx
      {:class (hyperfiddle.ui.iframe/auto-ui-css-class ctx)}])

   [:div (select-keys props [:class])
    (let [ctx (hyperfiddle.data/browse ctx :hyperfiddle.ide/preview)]
      [hyperfiddle.ide.preview.view/view-cmp nil ctx
       {:class (hyperfiddle.ui.iframe/auto-ui-css-class ctx)}])

    [:div.fiddle-editor-col
     (let [ctx (hyperfiddle.data/browse ctx :hyperfiddle/ide)]
       [hyperfiddle.ide.fiddles.fiddle-src/fiddle-src-renderer
        nil ctx
        {:initial-tab (-> @(:hypercrud.browser/route ctx) (get 3) hyperfiddle.ide/parse-ide-fragment)
         ;:initial-tab @(contrib.reactive/fmap-> (:hypercrud.browser/route ctx) (get 3) hyperfiddle.ide/parse-ide-fragment)
         :class (css "fiddle-editor devsrc" (hyperfiddle.ui.iframe/auto-ui-css-class ctx))}])

     [hyperfiddle.ide/ide-stage ctx]]]])
