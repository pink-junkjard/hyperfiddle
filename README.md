# Hypercrud Browser

> Programmable Datomic console, as a ClojureScript library

Live demos, docs and more: http://hyperfiddle.net/

**Hyperfiddle** is a drop-in Datomic Console replacement, scriptable from the web browser in ClojureScript, for making sophisticated database applications.

Hyperfiddle is built on top of the **Hypercrud** project, whose readme you are viewing.

* Hypercrud Server - serves data from Datomic, securely and efficiently
* Hypercrud Client - client/server data sync between javascript app and database
* Hypercrud Browser - Library for **data driven UIs, as an EDN value**

React offers the View as a pure function of data, but does not cover client/server data sync over network. UIs, of course, do data sync. Hypercrud handles the data sync.

Solving data sync leaves us with truly composable UIs. Compose sophisticated UIs out of simpler UIs. UIs are no longer just view components, but thick applications, with their own server data dependencies. Hypercrud lets you compose them like functions, letting us climb a rung higher on the ladder of abstraction, to the real goal: data driven UIs, as an edn value.

App-as-a-value paves the way for some seriously sophisticated application tooling.

**This entire screenshot is modeled as an edn value**. The bottom half is an editor for said edn values. The editor is itself modeled as an edn value too. [Yes it can edit itself and yes this is how we build it every day](http://hyperfiddle.net/hyperfiddle-blog/ezpkb21haW4gbmlsLCA6cHJvamVjdCAiaHlwZXJmaWRkbGUtYmxvZyIsIDpsaW5rLWRiaWQgI0RiSWRbMTc1OTIxODYwNDU4OTQgMTc1OTIxODYwNDU0MjJdLCA6cXVlcnktcGFyYW1zIHs6ZW50aXR5ICNEYklkWzE3NTkyMTg2MDQ2MjA3IDE3NTkyMTg2MDQ1ODgyXX19)).

![](https://i.imgur.com/sisRPWO.png)

Facebook is not built in hyperfiddle, but if it was, here is how we might break it down:

![](http://i.imgur.com/4mKpHhw.png)

### Library, framework or platform?

Hypercrud is 90% library, like Om Next or Reagent. The last 10% is the I/O runtime implementation that handles the data sync. The I/O runtime is general purpose, contains no application logic, and has a client piece and a server piece.

## Hypercrud's fundamental interface is two functions

Hypercrud apps export two functions: a Reagent view expression, and a request function. The request function encodes the precise, total data dependencies of an entire page, recursively.

Here is the simplest possible Hypercrud app, a blog:

```clojure
(def request-blog
  (->QueryRequest '[:find ?post :where [?post :post/title]]
                  {"$" #DbVal[:samples-blog]}
                  {"?post" [#DbVal[:samples-blog] ['*]]}))

(defn request [state peer]
  [request-blog])
```

The hydrated response looks like this:

```clojure
[{"?post" {:db/id #DbId[17592186045419 17592186045786], :post/title "First blog post"}}
 {"?post" {:db/id #DbId[17592186045420 17592186045786], :post/title "Second blog post"}} ... ]
```

The hydrated response value is passed into the view function. Since the data is already fetched, this function is synchronous.

```clojure
(defn view [state peer dispatch!]
  (-> (let [result @(hc/hydrate peer request-blog)]     ; synchronous
        [:ul
         (->> result
              (map (fn [relation]
                     (let [post (get relation "?post")]
                       [:li {:key (:db/id post)}
                        (:post/title post)]))))])))
```

![](http://i.imgur.com/zwoGq2I.png)

If the data isn't already fetched, `hc/hydrate` returns an error value. The runtime will manage the lifecycle of these functions. It is up to your choice of runtime as to the details of this lifecycle, for example, do you want to call the view function when the data is only partially loaded? You can do that if you want. Hyperfiddle.net does this and draws loading components and reports database errors appropriately. Your runtime doesn't have to do that, it's up to you.

All decision making is driven by the client. The client programming experience is that of composing functions and values, which makes it a straightforward exercise to model the app as an EDN value to be interpreted by the client.

## The client runtime

Like calling `ReactDOM.render(el, domNode)`, you'll need to provide an entrypoint which cedes control to the runtime. The simplest possible implementation is to create a single state atom, construct a Hypercrud Client with the state atom and the userland `request` function, and mount Reagent to the dom with the userland `view` function. Hypercrud Client watches the state atom for changes and will fetch data as needed.

## The server runtime

Hypercrud's server runtime is like Apache -- a general purpose data server -- you don't need to change it. It is essentially just a Pedestal service around a Datomic Peer. Datomic Peer is already inches away from being a general purpose data server. There is no interesting code in the server runtime. You can implement your own server runtime, or reuse our internals, or whatever. It's all open source and a small amount of code.

#### server data security

It works like Facebook "view my profile as other person" - the server restricts access to data based on who you are. Security is defined by two pure functions - a database filter predicate, and a transaction validator predicate. Both of these functions can query the database to make a decision. Datomic's distributed reads open the door for making this fast.

You can configure Hypercrud Server with your own arbitrary security predicates.

#### Why not already using Datomic Client API?

Historical reasons only, Datomic was still on the REST api when this started so we had to code the client. Theirs will be better and when they release a clojurescript client, we will use it.

## Hypercrud Browser

Hypercrud Browser navigates app-values like a web browser navigates HTML. **The things we generally have to write code for - security, performance, async, and failure handling - are all accidental complexity.** When you take that away, we're left with the very simple essence of an application's true business domain. For this, a simple DSL will do, the simpler the better. We're left with the essense of an application, as a value. App-values define Pages, each Page declares his data dependencies, and Links to other Pages.

Interpreted app-value in a web browser, with custom renderers disabled:

![](http://i.imgur.com/f1ngGLt.png)

Same app-value but as the end user sees it, respecting renderers:

![](http://i.imgur.com/4WlmuW8.png)

Here's the raw edn app-value:

```clojure
{:page/name "Sample Blog",
 :page/query "[:find ?post :in $ :where [?post :post/title]]",
 :page/dbs [{:dbhole/name "$", :dbhole/value {:database/ident "samples-blog",}}],
 :page/find-elements
 [{:find-element/name "?post",
   :find-element/connection {:database/ident "samples-blog"}
   :find-element/form
   {:form/name "samples-blog - post",
    :form/field
    [{:field/prompt "title",
      :field/attribute
      {:attribute/ident :post/title,
       :attribute/valueType #:db{:ident :db.type/string},
       :attribute/cardinality #:db{:ident :db.cardinality/one}}}
     {:field/prompt "date",
      :field/attribute
      {:attribute/ident :post/date,
       :attribute/valueType #:db{:ident :db.type/instant},
       :attribute/cardinality #:db{:ident :db.cardinality/one}}}
     {:field/prompt "content",
      :field/attribute
      {:attribute/ident :post/content,
       :attribute/valueType #:db{:ident :db.type/string},
       :attribute/cardinality #:db{:ident :db.cardinality/one}}}]}}]
 :page/links
 [{:link/prompt "view",
   :link/link {:db/id #DbId[17592186045791 17592186045422], :page/name "view post"},
   :link/repeating? true,
   :link/find-element {:find-element/name "?post", :find-element/connection #:db{:id #DbId[17592186045786 17592186045422]}},
   :link/ident :sys-edit-?post}
  {:link/prompt "new",
   :link/ident :sys-new-?post,
   :link/repeating? false,
   :link/find-element {:find-element/name "?post", :find-element/connection #:db{:id #DbId[17592186045786 17592186045422]}},
   :link/render-inline? true,
   :link/page {:db/id #DbId[17592186045791 17592186045422], :page/name "view post"}}]
 :page/renderer "(fn [relations colspec anchors param-ctx] ... )"}
```

You might imagine the code to interpret an app-value to produce a view and a request, to satisfy the Hypercrud core interface. This code is called Hypercrud Browser and provided as a library:

```clojure
(def app-value { ... })

(defn view [state peer dispatch!]
  [browser/safe-ui app-value (:route state) {:dispatch! dispatch!}])

(defn request [state peer]
  ; code your own data dependencies, or let the browser figure it out from an app-value
  (browser/request app-value (:route state)))
```

Pages compose by composing their data dependencies therein (like an iframe). The page abstraction is sufficient to implement composite widgets like select options, which are themselves a page with a query and form.

If you stop and think, this is a lot how a web browser works. Web browsers are a general HTML client which navigates a graph by following links between pages; this is the design objective of REST, what HATEOAS means, and is called a fully general hypermedia client. **Hyperfiddle browses CRUD apps like web browsers browse HTML.** This core browser technology is implemented as an open-source library called Hypercrud Browser. It is a fully general hypermedia client, it solves the failure of REST, and is what makes Hyperfiddle possible. Hypercrud Browser is HATEOAS.

App-values are graph-shaped and grow to be quite large. It is natural to want to store app-values in a database, and create tooling to build these up visually and interactively, which leads us to:

# Hyperfiddle

Hyperfiddle is a WYSIWYG editor for Hyperfiddle app-values ("hyperfiddles"). Basically dev-tools for Hypercrud Browser.

![](http://i.imgur.com/v3cmewv.png)