# Hyperfiddle — a Hypermedia Function

> <http://www.hyperfiddle.net/> – demos and overview

Hyperfiddle abstracts over client/server data sync for Datomic APIs by extending Datomic's immutable database semantics to the API. Unlike REST/GraphQL/whatever, Hyperfiddle's data sync *composes*. If React.js is managed DOM, Hyperfiddle is managed database and network.

Hyperfiddle models API inter-dependencies as a graph (I need query-X and also query-Y which depends query-Z). This graph lets the I/O runtime understand the structure and data flows of the application, which permits interesting optimization opportunities.

Managed I/O means, as a web dev, you are no longer concerned with remote data fetching or coding HTTP backends. But managed I/O is not the point. The point is: *what does managed I/O make possible that wasn't possible before?* 

# Dependency coordinates — Todo

    [com.hyperfiddle/hyperfiddle "0.0.0"]

# Roadmap

We are in early access / private beta, contact us.

### Blocking open source release:

- [ ] Developer ease of use
  - [ ] make nodejs runtime optional
  - [ ] decouple I/O runtime from cloud

On deck

- [ ] user experience
- [ ] command line interface

# Overview

Hyperfiddle is built in layers. Higher layers are optional and implemented in terms of lower.

1. API as a function
2. API as a graph
3. Reflective user interface
4. Structural editor for apps

## \#1. Composable data sync primitive: API as a function

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
                        
* I/O runtime will call API fn repeatedly until its return value stabalizes. The loop permits queries that depend on the result of earlier queries. Better than a loop would be a reactive model like Hoplon but for the server.
* Data loop typically runs in JVM Datomic Peer, so no network hops or N+1 problem like REST & ORM. Data loop sometimes runs in browser (e.g. in response to incremental ui state change).
* All API responses are immutable, because all requests have a time-basis – very very important for performance

## \#2. API as a graph

API as a function can be used to build an interpreter, it interprets *fiddles* which form a graph:

* *Fiddles* have a *query*, a *view*, and *links* to other fiddles
* Three types of link: *anchors*, *iframes*, and *buttons*

All fiddles are url addressable, as `text/HTML` (application) or `application/transit+json` (API). If the query takes input arguments, it's the link's job to decide the parameters with a *forumla* and put them in the url. Buttons execute effects which are specified by *transaction functions*. Formulas and tx-tns for basic CRUD scaffolding are automatic, you get a "CRUD admin dashboard" as a starting point and sculpt it from there.

<img src="https://i.imgur.com/ZtYAlTE.png" width="720px">

> *Links (graph edges) are pink. Select options are iframes with style. Live demo: <http://sandbox.hyperfiddle.net/gender>*

```clojure
{:fiddle/type :query,
 :fiddle/query "[:find (pull ?e [:db/id :reg/email :reg/gender :reg/shirt-size]) :where [?e :reg/email]]",
 :fiddle/links #{{:link/rel :options, :link/render-inline? true, :link/path "0 :reg/gender",
                  :link/fiddle {:fiddle/type :query,
                                :fiddle/query "[:find (pull ?e [:db/id :reg.gender/ident]) :where [?e :reg.gender/ident]]",
                                :fiddle/links #{{:link/rel :hyperfiddle/new, :link/fiddle #:db{:id 17592186045475}}}}}
                 {:link/rel :options, :link/render-inline? true, :link/dependent? true, :link/path "0 :reg/shirt-size",
                  :link/formula "(fn [ctx] @(contrib.reactive/cursor (:cell-data ctx) [:reg/gender]))",
                  :link/fiddle #:db{:id 17592186045435}}
                 {:link/rel :hyperfiddle/new, :link/fiddle #:db{:id 17592186045482}}}}
```
> `:link/rel` has semantic meaning like html, `:options` matches up with the `:db.type/ref` renderer which is a data driven extension point. If you override the `:db.type/ref` renderer, you may care to use `:link/rel :options` as semantic hint to render like select options, or not.

> `[:link/rel :hyperfiddle/new]` is a system provided button. Hyperfiddle provides sensible default anchors & buttons for basic CRUD. The ident `:hyperfiddle/new` is semantic; the system knows to auto-generate the trivial transaction function to make a Datomic transaction for the new record. We have shadowed the autolink here in order to specify the popover fiddle's d/pull, which is used to generate the form for a new record. Transaction functions are coded in CLJC.

> `:link/formula` is how Datomic query arguments are filled at the link site. Noteworthy: `(fn [ctx] @(contrib.reactive/cursor (:cell-data ctx) [:reg/gender]))` (this is cumbersome and will get simpler it should reduce to `:reg/gender`). Why is it reactive? Because in a live UI, if a dependency updates then the arguments need to be recomputed and the query re-run. Why shim a `contrib.reactive` namespace? Formulas also evaluate inside Datomic Peer, to coordinate data dependencies before the downstream API consumer asks for them. Hyperfiddle has enough semantic hints to auto-generate most formulas, this one is specified to declare the dependency.

### API as a graph permits optimizations that human-coded I/O cannot do:  

* Today: Automatic I/O partitioning and batching, optimized for cache hits
* Today: Exact and total page responses without over-fetching
* Future: Smart server prefetching and optimistic push

## \#3. Reflective (data-driven) user interface

All Hyperfiddle APIs must resolve to a single function. The fiddle graph interpreter function is *Hyperfiddle Browser*. You can of course provide your own function, but the Browser is special because it is a *hypermedia function*:

<p align="center">web browser : HTML documents :: hyperfiddle browser :: hyperfiddle EDN values</p>

The Browser is coded in CLJC and evaluates simultaneously in jvm, browser and node, which is how the service knows in advance what the UI will ask for.

Views are rendered through `eval`, progressive enhancement at any layer

* Datomic type #{string keyword bool inst long ref ...}
* Datomic attribute #{:post/content :post/title :post/date ...}
* Table, form, label, value, etc
* Markdown extensions
* Fiddle (page) renderer
* Hyperfiddle Browser - you can replace the interpreter itself

### TodoMVC with a markdown fiddle renderer:

![](https://i.imgur.com/7BqlUdn.png)

> This list item template is defined by markdown extensions. Inject view-specific renderers through markdown `!result[foo.bar/my-table-renderer]`, or even your own markdown implementation with `eval`. <http://sandbox.hyperfiddle.net/:tbd!all/>

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

## \#4. Structural editor for apps

Once you start coding "data" (data-ing?), a few ideas form:

1. Data is vastly more productive than code, the surface area for bugs is tiny–mostly just typos
2. Working with EDN-serialized text on a filesystem is stupid and causes the [project.clj chaos problem](https://github.com/technomancy/leiningen/blob/2d8ceda7cb69dfd1ae32540c68902573f38a9908/sample.project.clj).

Hyperfiddles are graphs, not text, so instead of git they are stored in Datomic. This means you can query the graph e.g. "which fiddles link to me". Like git and the web, there is no central database, rather many little ones distributed. The future implications of this are profound.

### Hyperfiddle is built in Hyperfiddle

Since hyperfiddle apps are graphs, it is straightforward to build an editor for those graphs, almost trivial. 90% of the work is a single reagent view. The backend and API is auto-generated like any other hyperfiddle app.

![](https://i.imgur.com/tGjsT0G.png)

> *The radio control on `:fiddle/type` is a custom attribute renderer, `qe-picker-control`, which is eval'ed at runtime. `:fiddle/query`'s custom renderer is a CodeMirror. The fiddle's own layout (including button labels and local state) is a fiddle renderer, about 100 lines of Reagent markup eval'ed at runtime. Each menu is a link to another fiddle, with its own data dependencies and renderers.*

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

# FAQ and anti-features

Integration with REST/fiat APIs: Hyperfiddle can't help you with foreign data sync but it doesn't get in the way either.
Run a custom backend next to hyperfiddle.server (or integrate with hyperfiddle.server) and do legacy web development.

Local state in UI: Hyperfiddle is about data sync and has no opinion about how your views work. Local state is perfectly allowable, do what you've always done. Keep in mind that Hyperfiddle data sync is so fast (on par with static sites) that you may not need much local state; just use page navigation (the URL is a perfectly good place to store state).

Reframe: Reframe does not support SSR so we can't use it in hyperfiddle core. Hyperfiddle has no opinion or technical constraints on what userland apps do for view rendering or state management. It remains to be seen whether something like reframe is desirable for Hyperfiddle views in practice.

Query subscriptions: This is a Datomic question and it is possible for straightforward queries, see this [reddit thread](https://www.reddit.com/r/Clojure/comments/6rncgw/arachneframeworkfactui/dl7r1xs/?context=2).

* Eric: I would like to understand where caching actually happens
* Eric: What happens from top to bottom when a form is rendered
* Eric: synchronous and reactive - what does this mean?

ivar: would hyperfiddle be reasonable for a chat service ? what are example apps (real or imagined) that would a) benefit from and b) be a bad fit for hyperfiddle?

dustingetz: @ivar your questions are insightful. The short answer is the I/O runtime is about 1000 line "kernel" and can be replaced, so if you want realtime push over websockets, you can drop down to that level and code it like usual. The I/O runtime implementations are general purpose and could be shared or plugged into any other hyperfiddle app. The point is that userland is sheltered from all that

ivar: so the plan (or existing state) is to support different swappable IO implementations?

dustingetz: Yes, we have a couple, different apps have different I/O needs. A simple example is you can serve the app completely without javascript and rely only on server side rendering, hyperfiddle-consulting.com works like that. It turns out that disabling javascript is actually slower, Hyperfiddle is similar to https://www.gatsbyjs.org/ and is faster with javascript once the entire app data dependencies are in browser cache. Its up to the I/O runtime to make those decisions, the default one is very good, most apps dont need to think about this
