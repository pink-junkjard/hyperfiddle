(ns hyperfiddle.test-preamble
  (:require
    [cljs.nodejs :as node]
    [goog.object :as object]))


(enable-console-print!)

(object/set js/global "propTypes" (node/require "prop-types"))
(object/set js/global "React" (node/require "react"))
(object/set js/global "createReactClass" (node/require "create-react-class"))
(object/set js/global "reactCreateFragment" (node/require "react-addons-create-fragment"))
(object/set js/global "ReactDOM" (node/require "react-dom"))
(object/set js/global "ReactDOMServer" (node/require "react-dom/server"))

(object/set js/global "remark" (node/require "remark"))
(object/set js/global "remarkGenericExtensions" (node/require "@hyperfiddle/remark-generic-extensions/lib/browser"))
(object/set js/global "remarkReact" (node/require "remark-react"))
