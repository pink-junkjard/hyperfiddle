# Hypercrud

> client/server data sync, for composable UIs

Live demos, docs and more: http://hyperfiddle.net/ (Hyperfiddle is the Hypercrud reference app)

* Hypercrud Browser - library for **data driven UIs, as an EDN value**
* Hypercrud Client - client/server data sync, UI as a function
* Hypercrud Server - General purpose data server for [Datomic](http://www.datomic.com/)

## Status

Hyperfiddle.net is stable. The abstractions are stable. You'll be able to fork a repo to run it yourself in a couple weeks, in the meantime we're putting some finishing touches to make that easy.

## Why

React.js offers the View as a pure function of data, but UIs are more than views. UIs are thick applications which hydrate server data. In database applications, network data sync -- I/O -- is root source of complexity. I/O has latency, it can fail, it is asynchronous, and these concerns pervade our UIs today.

Hypercrud handles the data sync, enabling us to program our UIs as pure functions of local data: truly composable UIs. Hypercrud UIs are functions, proper functions which compose, letting us climb a rung higher on the ladder of abstraction, to the real goal: data driven UIs, as an edn value.

App-as-a-value paves the way for some seriously sophisticated application tooling.

**This entire screenshot is modeled as an edn value**. The bottom half is an editor for said edn values. The editor is itself modeled as an edn value too. [Yes it can edit itself and yes this is how we build it every day.](http://hyperfiddle.net/hyperfiddle-blog/ezpkb21haW4gbmlsLCA6cHJvamVjdCAiaHlwZXJmaWRkbGUtYmxvZyIsIDpsaW5rLWRiaWQgI0RiSWRbMTc1OTIxODYwNDU4OTQgMTc1OTIxODYwNDU0MjJdLCA6cXVlcnktcGFyYW1zIHs6ZW50aXR5ICNEYklkWzE3NTkyMTg2MDQ2MjA3IDE3NTkyMTg2MDQ1ODgyXX19)

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

#### Why might I want to customize my client runtime?

Customoze your nodejs runtime for total control:

* server side rendering
* a mobile site with static html only, no javascript
* redirects, http headers, cookies
* control caching strategy
* authentication scheme

Customize your browser runtime for total control:

* Virtual dom implementation (Reagent vs Rum or whatever)
* state atom implementation (e.g. clojure atom vs reagent reactive atom)
* initialize state from server rendered initial state
* HTML5 pushstate navigation, url and routing strategy, local storage, analytics

All this is already provided with the included reference runtime and as part of Hyperfiddle, but you'll eventually want to control it. We also provide the simplest possible hello world runtime as a devkit example which doesn't have any of these features - just a React UI which syncs data, no user auth or anything else. The hello world runtime is two lines of code. The Hyperfiddle runtime is about 100 loc for the node runtime and 100 loc of web browser runtime.

We haven't built a React Native runtime yet, but this is how you would do it.


## The server runtime

Hypercrud's server runtime is like Apache -- a general purpose server -- you don't need to change it. It is essentially just a Pedestal service around a Datomic Peer. Datomic Peer is already inches away from being a general purpose data server. There is no interesting code in the server runtime. You can implement your own server runtime, or reuse our internals, or whatever. It's all open source and a small amount of code.

#### Isnt this just Datomic Peer Server and Datomic Client API?

yeah pretty much, Datomic was still on the REST api when this started so we had to code the client, and afaik their javascript client isn't out yet. Hypercrud Server is a Datomic peer + a comm protocol + a security layer, and not much else.

#### Data security

It works like Facebook "view my profile as other person" - the server restricts access to data based on who you are. Security is defined by two pure functions - a database filter predicate, and a transaction validator predicate. Both of these functions can query the database to make a decision. Datomic's distributed reads open the door for making this fast.

You can configure Hypercrud Server with your own arbitrary security predicates.

#### Performance - is it Datomic Peer or Datomic Client?

 Hypercrud Server is a Peer. Hypercrud Client may be, but is not constrained to be, implemented as a Datomic client. If you use Hypercrud Client core interface (view, request) without hypercrud browser, you are stuck with the Datomic client model, which is fine, but suboptimal, and re-introduces a theoretical performance problem caused by client/peer round trips. However, when you model the app as a value, you can literally transmit your app-value up to the server, and actually run the code to interpret the value inside the jvm Peer process. Optimal!

 Some of the above is on the roadmap and hasn't yet been implemented. We don't yet interpret app-values on JVM today, but we will. We can also run the entire application javascript in a nodejs environment colocated with a datomic peer. This happens in server side rendered cases, but not client side rendered cases today, but we will fix that soon so I can remove this sentence.

#### Why might I want to customize my server runtime?

* security
* authentication, if it isn't handled at your gateway

There aren't a lot of reasons for you to change the server runtime. It's like Apache for HTML - you just point it at Datomic and run the process.

## Hypercrud Browser

Hypercrud Browser navigates hypercrud app-values like a web browser navigates HTML. App-values define Pages, each Page declares his data dependencies, and Links to other Pages. Everything composes.

Interpreted app-value in a web browser, with custom renderers disabled:

![](http://i.imgur.com/f1ngGLt.png)

Same app-value but as the end user sees it, respecting renderers:

![](http://i.imgur.com/4WlmuW8.png)

Here's the raw edn app-value (truncated, here i don't show the whole thing):

```clojure
{:page/name "Sample Blog"
 :page/query "[:find ?post :in $ :where [?post :post/title]]"
 :page/dbs [{:dbhole/name "$" :dbhole/value {:database/ident "samples-blog"}}]
 :page/renderer "(fn [relations colspec anchors param-ctx] ... )"
 :page/find-elements [{:find-element/name "?post"
                       :find-element/connection {:database/ident "samples-blog"}
                       :find-element/form {:form/name "samples-blog - post"
                                           :form/field [{:field/prompt "title" :field/attribute {:attribute/ident :post/title :attribute/valueType #:db{:ident :db.type/string} :attribute/cardinality #:db{:ident :db.cardinality/one}}}
                                                        {:field/prompt "date" :field/attribute {:attribute/ident :post/date :attribute/valueType #:db{:ident :db.type/instant} :attribute/cardinality #:db{:ident :db.cardinality/one}}}
                                                        {:field/prompt "content" :field/attribute {:attribute/ident :post/content :attribute/valueType #:db{:ident :db.type/string} :attribute/cardinality #:db{:ident :db.cardinality/one}}}]}}]
 :page/links [{:link/prompt "view" :link/ident :sys-edit-?post
               :link/repeating? true
               :link/page {:db/id #DbId[17592186045791 17592186045422] :page/name "view post"}
               :link/find-element {:find-element/name "?post" :find-element/connection #:db{:id #DbId[17592186045786 17592186045422]}}}
              {:link/prompt "new" :link/ident :sys-new-?post :link/render-inline? true
               :link/page {:db/id #DbId[17592186045791 17592186045422] :page/name "view post"}
               :link/find-element {:find-element/name "?post" :find-element/connection #:db{:id #DbId[17592186045786 17592186045422]}}}]}
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

**Hyperfiddle** is a drop-in Datomic Console replacement, scriptable from the web browser in ClojureScript, for making sophisticated database applications.

Hyperfiddle is a WYSIWYG editor for Hyperfiddle app-values ("hyperfiddles"). Basically dev-tools for Hypercrud Browser.

![](http://i.imgur.com/v3cmewv.png)

Hyperfiddle is our reference app, because it and pushes Hypercrud in recursive ways. It forced us to converge our abstractions and data model to a total solution without any gaps. We had to shave approximately 100% of the yaks for it to even be possible.

## Hyperfiddle's runtime

single page app vs SSR hybrid
Routing, links
doesnt break web



# More reading

* [Hyperfiddle vs REST](http://hyperfiddle.net/hyperfiddle-blog/ezpkb21haW4gbmlsLCA6cHJvamVjdCAiaHlwZXJmaWRkbGUtYmxvZyIsIDpsaW5rLWRiaWQgI0RiSWRbMTc1OTIxODYwNDU4OTQgMTc1OTIxODYwNDU0MjJdLCA6cXVlcnktcGFyYW1zIHs6ZW50aXR5ICNEYklkWzE3NTkyMTg2MDQ2MTMyIDE3NTkyMTg2MDQ1ODgyXX19)
* [Datomic vs the Object/Relational Impedance Mismatch](http://hyperfiddle.net/hyperfiddle-blog/ezpkb21haW4gbmlsLCA6cHJvamVjdCAiaHlwZXJmaWRkbGUtYmxvZyIsIDpsaW5rLWRiaWQgI0RiSWRbMTc1OTIxODYwNDU4OTQgMTc1OTIxODYwNDU0MjJdLCA6cXVlcnktcGFyYW1zIHs6ZW50aXR5ICNEYklkWzE3NTkyMTg2MDQ2MjEwIDE3NTkyMTg2MDQ1ODgyXX19)