# Hypercrud Browser

> Programmable Datomic console, as a ClojureScript library

Live demos, docs and more: http://hyperfiddle.net/

**Hyperfiddle** is a drop-in Datomic Console replacement, scriptable from the web browser in ClojureScript, for making sophisticated database applications.

Hyperfiddle is built on top of the **Hypercrud** project, whose readme you are viewing.

* Hypercrud Server - serves data from Datomic, securely and efficiently
* Hypercrud Client - client/server data sync between javascript app and database
* Hypercrud Browser - Library for **data driven UIs, as an EDN value**

React offers the View as a pure function of data, but does not cover client/server data sync over network. UIs, of course, need to do data sync. Hypercrud handles the data sync.

Solving data sync leaves us with truly composable UIs. Compose sophisticated UIs out of simpler UIs. UIs are no longer just view components, but thick applications, with their own server data dependencies. Hypercrud lets you compose them like functions, letting us climb a rung higher on the ladder of abstraction, to the real goal: data driven UIs, as an edn value.

And app as an edn value paves the way for some seriously sophisticated application tooling. This entire screenshot is modeled as an edn value. The bottom half is an editor for said edn values. **Hypercrud Browser navigates edn app-values like a web browser navigates HTML.**

![](https://i.imgur.com/sisRPWO.png)

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

#### Why not already using Datomic Client API?

Historical reasons only, Datomic was still on the REST api when this started so we had to code the client. Theirs will be better and when they release a clojurescript client, we will use it.

## Hypercrud Browser