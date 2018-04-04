# Hyperfiddle — a Hypermedia Function

Hyperfiddle abstracts over client/server data sync for Datomic APIs. Hyperfiddle extends Datomic's immutable database semantics to the API. Unlike REST/GraphQL/whatever, Hyperfiddle's data sync *composes*. If React.js is managed DOM, Hyperfiddle is managed database and network.

Hyperfiddle models API inter-dependencies as a graph (I need query-X and also query-Y which depends query-Z). This graph lets the I/O runtime understand the structure and data flows of the application, which permits interesting optimization opportunities.

Managed I/O is not the point. The point is: *what does managed I/O make possible that wasn't possible before?* 

## Live Demo: <http://sandbox.hyperfiddle.net/:todomvc/>

# Dependency coordinates — Todo

    [com.hyperfiddle/hyperfiddle "0.0.0"]

# Roadmap

We are in early access / private beta, contact us.

We're nearing a 0.1 open-source release in Q2 2018.

### Blocking 0.1.0: Cloud beta

- [x] Performance (Hyperfiddle must respond as fast as a Clojure repl)
- [x] API: data loop running in JVM
- [x] API: automatically optimize hydrates for cache locality (using link graph)
- [x] UI: Full reactivity everywhere
- [x] UI: Userland custom router
- [x] UI: Human readable URLs
- [x] UI: Stabalize Hyperblog URLs
- [ ] UI: developer experience

### 0.2.0: self-host beta

- [ ] command line interface to run hyperfiddle.server on your dev machine

# Overview

Hyperfiddle is built in layers. Higher layers are optional and implemented in terms of lower.

1. **Managed I/O primitives:** API defined as a function
2. **API modeled as graph:** data-driven CRUD model, API defined in EDN
3. **Data-driven UI:** automatic UI (like Swagger)
4. **IDE for applications:** structural editor for APIs

Hyperfiddle IDE, the structural editor (<http://www.hyperfiddle.net>) is also the open-source reference application to teach you how Hyperfiddle works.

## \#1. API as a function – a composable data sync primitive

A simple API fn:

```clojure
(def datomic-samples-blog #uri "datomic:free://datomic:4334/samples-blog")
(def db-branch nil) ; This is the datomic time-basis, nil is HEAD

(def race-registration-query
  '[:find (pull ?e [:db/id :reg/email :reg/gender :reg/shirt-size]) :where [?e :reg/email]])

(def gender-options-query
  '[:find (pull ?e [:db/id :reg.gender/ident]) :where [?e :reg.gender/ident]])

(defn api [state peer]
  (let [$ (hc/db peer datomic-samples-blog db-branch)]            
    [(->QueryRequest race-registration-query [$])
     (->QueryRequest gender-options-query [$])]))
```
                        
* I/O runtime will call API fn repeatedly until its return value stabalizes
* This permits queries that depend on the results of earlier queries  
* Data loop typically runs in JVM Datomic Peer, so no network hops or N+1 problem like REST & ORM
* Data loop sometimes runs in browser (e.g. in response to incremental ui state change)
* all I/O responses are immutable, all requests have a time-basis

This is the basis on which we build *composable abstractions:*

## \#2. API defined in EDN

Data model is a graph:

* *Fiddles* have a *query* (at the top)
* Fiddles have *links* to other fiddles (links are pink)
* Three types of link: *anchors* & *iframes* (hypermedia), and *buttons* (eval)

```clojure
{:db/id 17592186045418,                                     ; Root query
 :fiddle/type :query,
 :fiddle/query "[:find (pull ?e [:db/id :reg/email :reg/gender :reg/shirt-size]) :where [?e :reg/email]]",
 :fiddle/links #{{:db/id 17592186045427,                    ; link to gender options
                  :link/rel :options,
                  :link/render-inline? true,                ; embedded like an iframe
                  :link/path "0 :reg/gender",
                  :link/fiddle {:db/id 17592186045428,      ; select options query
                                :fiddle/type :query,
                                :fiddle/query "[:find (pull ?e [:db/id :reg.gender/ident]) :where [?e :reg.gender/ident]]",
                                :fiddle/links #{{:db/id 17592186045474, ; link to new-gender-option form
                                                 :link/rel :sys-new-?e,
                                                 :link/fiddle #:db{:id 17592186045475} ; new-gender form omitted
                                                 }}}}
                 {:db/id 17592186045434,                    ; link to shirt-size options
                  :link/rel :options,
                  :link/render-inline? true,                ; embedded like an iframe
                  :link/dependent? true,                    ; dependency on parent fiddle's data
                  :link/path "0 :reg/shirt-size",
                  :link/formula "(fn [ctx] @(contrib.reactive/cursor (:cell-data ctx) [:reg/gender]))",
                  :link/fiddle #:db{:id 17592186045435}     ; shirt size options query omitted
                  }
                 {:db/id 17592186045481,                    ; link to new-registration form
                  :link/rel :sys-new-?e,
                  :link/fiddle #:db{:id 17592186045482}     ; new-registration form omitted
                  }}}
```

### API as a graph permits optimizations that human-coded I/O cannot do:  

* Today: Automatic I/O partitioning and batching, optimized for cache hits
* Today: Exact and total page responses without accidental over-fetching
* Future: Smart server prefetching and optimistic push
* Future: Machine learning

## \#3. Data-driven UI, like Swagger UI

Batteries included tooling, your API "just works" out of the box.

> <img src="https://i.imgur.com/ZtYAlTE.png" width="720px">
>
> *Above EDN definition, rendered. Pink highlights indicate a link to another fiddle. live fiddle: <http://sandbox.hyperfiddle.net/gender>*

Links are pink (links are edges in the graph). Select options are iframes with style. `:link/rel` has semantic meaning like html, `:options` matches up with the [`:db.type/ref` renderer](https://github.com/hyperfiddle/hyperfiddle/blob/bd61dfb07cbff75d5002b15999d1abc6c3c6af3c/src/hypercrud/ui/widget.cljs#L74). When the gender semantic renderer is toggled off, the link is in the header, because the query runs once, not per-row. If you override the `:db.type/ref` renderer, you may care to use `:link/rel` as semantic hint, or not. Imagine a [link/rel registry like HTML](https://www.iana.org/assignments/link-relations/link-relations.xhtml).*

*web browser : HTML documents :: hyperfiddle browser :: hyperfiddle EDN values*

All Hyperfiddle APIs must resolve to a single function. The fiddle graph interpreter function is the *Hyperfiddle Browser*.

### Views rendered through `eval`, progressive enhancement at any layer

* Datomic type #{string keyword bool inst long ref ...}
* Datomic attributes #{:post/content :post/title :post/date ...}
* Form and table renderers #{form table tr th label value ...}
* Markdown with hyperfiddle extensions
* Fiddle renderer
* Hyperfiddle Browser

Each layer is composed from the layer above, so you can change as or as little as you like.

> <img src="https://i.imgur.com/pQk6g0a.png" width="720px">
> 
> *This dashboard is fully data driven. The markdown editor is defined by ClojureScript code, stored in a database and eval'ed at runtime.*

Here is a markdown attribute renderer:

> <img src="https://i.imgur.com/Kok1tz9.png">
> 
> *On the left, we see `:post/content` attribute is a Datomic `:db.type/string`,
> being rendered as a CodeMirror with markdown syntax highlighting. On the right, we 
> see it wired up. Renderers can be any ClojureScript expression and are eval'ed at runtime.*

Here is a fiddle root renderer:

> <img src="https://i.imgur.com/KP90ClH.png">
> 
> *On the left, we see the fiddle's final rendering. It is no longer a form at all,
> we've rendered the data as HTML. On the right, we see the Reagent expression.
> The data/view toggles the fiddle renderer, so you can always get to the admin dashboard.*

Here is the [Datomic schema renderer](https://github.com/hyperfiddle/hyperfiddle/blob/bd61dfb07cbff75d5002b15999d1abc6c3c6af3c/src/hypercrud/ui/auto_control.cljs#L15-L30), TODO this should be a core.match config stored in a database...

The dashboard is backed by an API, which hydrates a route (URL), except instead of HTML the server
the server returns EDN (transit). There is also an "open" API for development that will respond to any query, not just valid routes. The "open" API has future implications for the semantic web.

## \#4. Hyperfiddle IDE: a Structural Editor for APIs

Once you start coding "data" (data-ing?), a few ideas form:

1. Data is vastly more productive than code, the surface area for bugs is tiny–mostly just typos
2. Working with EDN-serialized text on a filesystem is stupid and causes the [project.clj chaos problem](https://github.com/technomancy/leiningen/blob/2d8ceda7cb69dfd1ae32540c68902573f38a9908/sample.project.clj).

Hyperfiddles are graphs, not text, so instead of git they are stored in Datomic. This means you can query the graph e.g. "which fiddles link to me". Like git and the web, there is no central database, rather many little ones distributed. The future implications of this are profound.

## Hyperfiddle.net is 100% built in Hyperfiddle (EDN style)

The following screenshot is fully defined as Hyperfiddle EDN and simple Reagent expressions that a web 
designer could write.

> <img src="https://i.imgur.com/DCPtHN3.png" width="720px">
> 
> *The radio control on `:fiddle/type` is a custom attribute renderer, `qe-picker-control`, which is defined 
> in the view menu and eval'ed at runtime. `:fiddle/query`'s custom renderer is a CodeMirror with syntax 
> highlighting, balanced parens etc. The fiddle's own layout (including button labels and local state) 
> is a fiddle renderer, about 100 lines of Reagent markup, defined in the view menu and eval'ed at runtime.
> Each menu is a `:button` link to another fiddle, with its own data dependencies and renderers.*

# FAQ and anti-features

Integration with REST/fiat APIs: Hyperfiddle can't help you with foreign data sync but it doesn't get in the way either.
Do what you'd do in React. You'll lose out-of-the-box SSR.

Local state in UI: Hyperfiddle is about data sync; Do what you'd do in React. Keep in mind that Hyperfiddle data sync
is so fast (on par with static sites) that you may not need much local state; just use page navigation (the URL is a 
perfectly good place to store state). Local state is perfectly allowable and idiomatic

Reframe: Reframe does not support SSR, and we want SSR by default. It should be straightforward to swap in
reframe or any other rendering strategy.

Why Reagent? Vanilla React is memoized rendering oriented around values. It's not fast enough; Hyperfiddle UIs 
can get very sophisticated very fast, the reactive abstraction is required to feel snappy.

Query subscriptions: This is a Datomic question and it is possible for straightforward queries, see this [reddit thread](https://www.reddit.com/r/Clojure/comments/6rncgw/arachneframeworkfactui/dl7r1xs/?context=2).

* Eric: I would like to understand where caching actually happens
* Eric: What happens from top to bottom when a form is rendered
* Eric: synchronous and reactive - what does this mean?

## How far will it scale?

Without immutability in the database, all efforts to abstract higher will fail; because to achieve the 
necessary performance, requires writing imperative optimizations to imperative platforms (object-relational 
impedance mismatch, N+1 problems, deeply nested JOINs, caching vs batching tradeoffs). Hyperfiddle cannot 
be built on RDBMS or Neo4J; their imperative nature leaks through all attempts to abstract. This is why all 4GLs 
historically fail. Immutability, as always, to the rescue.

How high can we abstract? We aren't sure yet. It will scale until Datomic's app-as-a-value abstraction breaks down, 
at which point programmers will manually optimize their Datomic services. Datomic Peer vs Client vs Cloud makes a 
difference here as they all have different stories for code/data locality. 

Hyperfiddle's abstraction has scaled quite far already, see Hyperfiddle-in-Hyperfiddle. We think there is much more 
room to go. Imagine a world of composable applications.

# Documentation and community

Hyperfiddle is alpha software. The programming API is not frozen.

<https://www.reddit.com/r/hyperfiddle/> will aggregate all our scattered blog posts, tutorials
and documentation.
