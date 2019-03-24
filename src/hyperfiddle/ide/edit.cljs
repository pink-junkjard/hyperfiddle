(ns hyperfiddle.ide.edit
  (:require
    [contrib.css :refer [css]]
    [contrib.reactive :as r]
    [hypercrud.ui.error :as error-cmps]
    [cats.monad.either :as either]
    [hyperfiddle.ide.domain :as ide-domain]
    [hyperfiddle.runtime :as runtime]))


;(def preview-state (r/atom nil))

(defn view [_ ctx props]
  [:<>
   (let [ctx (hyperfiddle.data/browse ctx :hyperfiddle/topnav)]
     [hyperfiddle.ide.fiddles.topnav/renderer nil ctx
      {:class (hyperfiddle.ui.iframe/auto-ui-css-class ctx)}])

   [:div (select-keys props [:class])
    (let [ctx (hyperfiddle.data/browse ctx :hyperfiddle.ide/preview)]
      [:div {:class (hyperfiddle.ui.iframe/auto-ui-css-class ctx)}
       (either/branch
         (::ide-domain/user-domain+ (runtime/domain (:peer ctx)))
         (fn [e]
           [:<>
            [:h2 "Domain misconfigured"]                    ; todo improve me
            [error-cmps/error-block e]])
         (fn [user-domain]

           ; Staging area
           ; Preview toolbar
           ; content pane

           (hyperfiddle.ide.preview.view/with-user-rt ctx user-domain)))])


    (let [ctx (hyperfiddle.data/browse ctx :hyperfiddle/ide)]
      [:div.fiddle-editor-col
       [hyperfiddle.ide/ide-stage ctx]
       [hyperfiddle.ide.fiddles.fiddle-src/fiddle-src-renderer
        nil ctx
        {:initial-tab (-> @(:hypercrud.browser/route ctx) (get 3) hyperfiddle.ide/parse-ide-fragment)
         ;:initial-tab @(contrib.reactive/fmap-> (:hypercrud.browser/route ctx) (get 3) hyperfiddle.ide/parse-ide-fragment)
         :class (css "fiddle-editor devsrc" (hyperfiddle.ui.iframe/auto-ui-css-class ctx))}]])
    ]])
