(ns hyperfiddle.test-preamble
  (:require [cljs.nodejs :as node]))


(enable-console-print!)

(aset js/global "Color" (node/require "color"))
(aset js/global "propTypes" (node/require "prop-types"))
(aset js/global "React" (node/require "react"))
(aset js/global "createReactClass" (node/require "create-react-class"))
(aset js/global "reactCreateFragment" (node/require "react-addons-create-fragment"))
(aset js/global "ReactDOM" (node/require "react-dom"))
(aset js/global "ReactDOMServer" (node/require "react-dom/server"))
(aset js/global "remark" (node/require "remark"))
(aset js/global "remarkCustomBlocks" (node/require "remark-custom-blocks"))
(aset js/global "remarkGenericExtensions" (node/require "remark-generic-extensions/lib/browser"))
(aset js/global "remarkGridTables" (node/require "remark-grid-tables"))
(aset js/global "remarkReact" (node/require "remark-react"))
