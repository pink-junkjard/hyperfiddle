# Hyperfiddle — a Hypermedia Function

> <http://www.hyperfiddle.net/> – demos and overview

Hyperfiddle abstracts over client/server data sync for Datomic APIs by extending Datomic's immutable database semantics to the API. Unlike REST/GraphQL/whatever, Hyperfiddle's data sync *composes*. If React.js is managed DOM, Hyperfiddle is managed database and network.

Hyperfiddle models API inter-dependencies as a graph (I need query-X and also query-Y which depends query-Z). This graph lets the I/O runtime understand the structure and data flows of the application, which permits interesting optimization opportunities.

Managed I/O is not the point. The point is: *what does managed I/O make possible that wasn't possible before?* 

<img src="https://i.imgur.com/pQk6g0a.png" width="720px"> . 
> *This is what data driven API feels like. UI is auto generated from data and fully dynamic. Web service is also fully generated.*

# Dependency coordinates — Todo

    [com.hyperfiddle/hyperfiddle "0.0.0"]

# Roadmap

We are in early access / private beta, contact us.

### 0.1.0: Fast and stable in cloud

- [x] Performance (Hyperfiddle must respond as fast as a Clojure repl)
- [x] API: data loop running in JVM
- [x] API: automatically optimize hydrates for cache locality (using link graph)
- [x] UI: Full reactivity everywhere
- [x] UI: Userland custom router
- [x] UI: Human readable URLs
- [x] UI: Stabalize Hyperblog URLs
- [ ] UI: developer experience

### 0.2.0: Self-host

- [ ] self-host from github
- [ ] command line interface to run against local datomic

# Overview

Hyperfiddle is built in layers. Higher layers are optional and implemented in terms of lower.

1. **Managed I/O primitives:** API defined as a function
2. **API modeled as graph:** data-driven CRUD model, API defined in EDN
3. **Data-driven UI:** automatic UI (like Swagger)
4. **IDE for applications:** structural editor for APIs

## \#1. API as a function – composable data sync primitive

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

All fiddles are url addressable. You can hydrate fiddle urls as HTML (as a webpage), or as EDN (as an API).

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
> `:link/formula` is how Datomic query arguments are filled from a context. This line is noteworthy: `(fn [ctx] @(contrib.reactive/cursor (:cell-data ctx) [:reg/gender]))` Why is it reactive? Because in a Reagent UI, if the context updates then the arguments need to be recomputed and the query re-run. Why shim a `contrib.reactive` namespace? Because formulas also evaluate inside Datomic Peer, to predict data dependencies before the downstream API consumer asks for them.

### API as a graph permits optimizations that human-coded I/O cannot do:  

* Today: Automatic I/O partitioning and batching, optimized for cache hits
* Today: Exact and total page responses without accidental over-fetching
* Future: Smart server prefetching and optimistic push
* Future: Machine learning

## \#3. Data-driven UI means out-of-the-box tooling, batteries included

<img src="https://i.imgur.com/ZtYAlTE.png" width="720px">

> *Above EDN definition, rendered. Live: <http://sandbox.hyperfiddle.net/gender>*

Links are pink (links are edges in the graph). Select options are iframes with style. `:link/rel` has semantic meaning like html, `:options` matches up with the [`:db.type/ref` renderer](https://github.com/hyperfiddle/hyperfiddle/blob/bd61dfb07cbff75d5002b15999d1abc6c3c6af3c/src/hypercrud/ui/widget.cljs#L74). If you override the `:db.type/ref` renderer, you may care to use `:link/rel` as semantic hint, or not. Imagine a [link/rel registry like HTML](https://www.iana.org/assignments/link-relations/link-relations.xhtml).

All Hyperfiddle APIs must resolve to a single function. The fiddle graph interpreter function is *Hyperfiddle Browser*. You can of course provide your own function, but the Browser is special because it is a *hypermedia function*:

<p align="center">web browser : HTML documents :: hyperfiddle browser :: hyperfiddle EDN values</p>

The Browser is coded in CLJC and evaluates simultaneously in jvm, browser and node, which is how the service knows in advance what the UI will ask for.

### Views rendered through `eval`, progressive enhancement at any layer

* Datomic type #{string keyword bool inst long ref ...}
* Datomic attribute #{:post/content :post/title :post/date ...}
* Table, form, label, value, etc
* Markdown extensions
* Fiddle (page) renderer
* Hyperfiddle Browser - the interpreter itself

### TodoMVC with a markdown fiddle renderer:

![](https://i.imgur.com/7BqlUdn.png)

> This list item template is defined by markdown extensions. Inject view-specific renderers through markdown `!result[foo.bar/my-table-renderer]`, or even your own markdown implementation with `eval`

### TodoMVC default UI, no progressive enhancement:

<img src="https://i.imgur.com/ieLlI7M.png" width="500px">

> It is a fully functional application though, this view always works even when you break your custom views

### Reagent fiddle renderer for the blog:

<img src="https://i.imgur.com/KP90ClH.png">

> *On the left, we see the fiddle's final rendering. It is no longer a form at all,
> we've rendered the data as HTML. On the right, we see the Reagent expression.
> The data/view toggles the fiddle renderer, so you can always get to the admin dashboard.*

### Attribute renderer:

<img src="https://i.imgur.com/Kok1tz9.png">

> *On the left, we see `:post/content` attribute is a Datomic `:db.type/string`,
> being rendered as a CodeMirror with markdown syntax highlighting. On the right, we 
> see it wired up. Renderers can be any ClojureScript expression and are eval'ed at runtime.*

## \#4. Structural Editor

Once you start coding "data" (data-ing?), a few ideas form:

1. Data is vastly more productive than code, the surface area for bugs is tiny–mostly just typos
2. Working with EDN-serialized text on a filesystem is stupid and causes the [project.clj chaos problem](https://github.com/technomancy/leiningen/blob/2d8ceda7cb69dfd1ae32540c68902573f38a9908/sample.project.clj).

Hyperfiddles are graphs, not text, so instead of git they are stored in Datomic. This means you can query the graph e.g. "which fiddles link to me". Like git and the web, there is no central database, rather many little ones distributed. The future implications of this are profound.

## Hyperfiddle is built in Hyperfiddle

<img src="https://i.imgur.com/DCPtHN3.png" width="720px">

> *The radio control on `:fiddle/type` is a custom attribute renderer, `qe-picker-control`, which is eval'ed at runtime. `:fiddle/query`'s custom renderer is a CodeMirror. The fiddle's own layout (including button labels and local state) is a fiddle renderer, about 100 lines of Reagent markup eval'ed at runtime. Each menu is a link to another fiddle, with its own data dependencies and renderers.*

# FAQ and anti-features

Integration with REST/fiat APIs: Hyperfiddle can't help you with foreign data sync but it doesn't get in the way either.
Run a custom backend next to hyperfiddle.server (or integrate with hyperfiddle.server) and do legacy web development.

Local state in UI: Hyperfiddle is about data sync and has no opinion about how your views work. Local state is perfectly allowable, do what you've always done. Keep in mind that Hyperfiddle data sync is so fast (on par with static sites) that you may not need much local state; just use page navigation (the URL is a perfectly good place to store state).

Reframe: Reframe does not support SSR and the degree of local state which reframe encourages is not idiomatic in Hyperfiddle-in-the-large. Hyperfiddle internally uses a redux-like action/reducer pattern. Your custom renderers (including the root renderer) have no restrictions and you should have no trouble using Reframe.

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

Hyperfiddle's abstraction has scaled quite far already, see Hyperfiddle-in-Hyperfiddle.

![](https://i.imgur.com/OZyH0P5.png)

> *We think it will scale at least this far, and there is much more room to go. Can you imagine a world of composable applications?*

# Documentation and community

Hyperfiddle is alpha software. The programming API is not frozen.

<https://www.reddit.com/r/hyperfiddle/> will aggregate all our scattered blog posts, tutorials
and documentation.
