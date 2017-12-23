# Hyperfiddle

> functional client/server data sync, for composable and data-driven CRUD applications

This is the open source library powering <http://www.hyperfiddle.net>.

## Dependency coordinates—Todo

There is a JVM server and a browser client. Prod deployments additionally target nodejs (for server-side rendering). All platforms use the same artifact:

    [com.hyperfiddle/hyperfiddle "0.0.0"]

## Community

We are eager to talk to you so please reach out!

* <https://www.reddit.com/r/hyperfiddle/>
* Slack: #Hyperfiddle @ [clojurians](http://clojurians.net/)
* <https://groups.google.com/forum/#!forum/hyperfiddle>

# Philosophy

Hyperfiddle is built in layers

* Low level I/O runtime for client/server data sync, such that userland is pure functions (**app-as-a-function**)
* High level data-driven interpreter function (**app-as-a-value**)

You can code at either level. Both data and functions compose properly (it is just Clojure functions and Clojure data).

## App-as-a-Function

Hyperfiddle UI component functions look like this:

    (fn [] :todo)

## App-as-a-Value

hyperfiddle EDN values look like this:

    {:to :do}

This is the basis for extremely powerful data-driven abstractions as seen at <http://www.hyperfiddle.net/>.

# How it works

## UI components as Clojure functions

UI, at its essense, is about two concerns:

* a view (pixels) — a function returning the virtual dom
* the data backing it — a function returning the query

Hyperfiddle UI components work like that, which means they are insulated from network and database side effects. The component defines it's query (often with dependencies on other components), and then an I/O runtime does the client/server data sync.

So there are two functions, a data fn and a view fn. Both functions are a traversal of the same UI tree, they just do different things at the leaf node. The traversal code is shared and defined in .CLJC, so it runs in Browser, Node and JVM.

This lets us run the application logic wherever is fastest for any given task. React rendering is fastest in the browser; server-side initial render is done in Node. Most importantly, the data dependency loop runs on JVM inside a Datomic peer. **No browser/service round trips like REST, no service/database round trips like SQL. No object/relational impedance mismatch.** This is why Hyperfiddle can abstract so much farther, and why Datomic is so amazing.

## UI components as data

Hyperfiddle Browser is a generic app-as-a-function which interprets app-values. A *hyperfiddle* (noun) is just an EDN value, inspired by HTML.

#### web browser : web pages :: hyperfiddle browser :: hyperfiddles

TODO write more

## How far will it scale?

Without immutability in the database, all efforts to abstract higher will fail; because to achieve the necessary performance, requires writing imperative optimizations to imperative platforms (object-relational impedance mismatch, N+1 problems, deeply nested JOINs, caching vs batching tradeoffs, [The Vietnam of Computer Science (2006)](http://blogs.tedneward.com/post/the-vietnam-of-computer-science/)). Hyperfiddle cannot be built on RDBMS or Neo4J; their imperative nature leaks through all attempts to abstract. Immutability, as always, to the rescue.

How high can we abstract? We aren't sure yet. It will scale until Datomic's app-as-a-value abstraction breaks down, at which point programmers will manually optimize their Datomic services. Datomic Peer vs Datomic Client vs Datomic Cloud makes a difference here as they all have different stories for code/data locality. We think it will scale a lot farther than imperative systems.
