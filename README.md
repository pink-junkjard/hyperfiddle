# Hyperfiddle

> functional client/server data sync, for composable and data-driven CRUD applications

## Dependency coordinates—Todo

There is a JVM server and a browser client. Prod deployments additionally target nodejs (for server-side rendering). All platforms use the same artifact:

    [com.hyperfiddle/hyperfiddle "0.0.0"]

## Community

We are eager to talk to you so please reach out!

* <https://www.reddit.com/r/hyperfiddle/>
* Slack: #Hyperfiddle @ [clojurians](http://clojurians.net/)
* <https://groups.google.com/forum/#!forum/hyperfiddle>

## App-as-a-Function

Hyperfiddle's core building block is a recursive model for UI components.

UI, at its essense, is about two concerns:

* a view (pixels) — a function returning the virtual dom
* the data backing it — a function returning the query

Hyperfiddle UI components work like that, which means they are insulated from network and database side effects. The component defines it's query (often with dependencies on other components), and then an I/O runtime does the client/server data sync. Immutability in the database means there are many clever tricks we can do to optimize this without leaking the abstraction.

Proper functional composition in the UI is the foundation necessary to climb ever higher up the ladder of abstraction.

(Okay, it's two functions. But both functions are traversals of the same UI tree, they just do different things at the leaf node, and they share much of their code and we may eventually attempt to unify them.

## App-as-a-Value

Hyperfiddle Browser is a generic app-as-a-function which interprets app-values. A *hyperfiddle* (noun) is just an EDN value, inspired by HTML.

#### web browser : web pages :: hyperfiddle browser :: hyperfiddles

This is the basis for extremely powerful data-driven abstractions. The best place to read about that is <http://www.hyperfiddle.net/>.

## How far will it scale?

Without immutability in the database, all efforts to abstract higher will fail; because to achieve the necessary performance, requires writing imperative optimizations to imperative platforms (object-relational impedance mismatch, N+1 problems, deeply nested JOINs, caching vs batching tradeoffs). This system cannot be built on RDBMS or Neo4J; their imperative nature breaks all attempts to abstract. [The Vietnam of Computer Science (2006)](http://blogs.tedneward.com/post/the-vietnam-of-computer-science/)

But immutability, as always, to the rescue. Datomic's code/data locality story brings us a solution.

How high can we abstract? We aren't sure yet. It will scale until Datomic's app-as-a-value abstraction breaks down, at which point programmers will manually optimize their Datomic services. Datomic Peer vs Datomic Client vs Datomic Cloud makes a difference here as they all have different stories for code/data locality. We think it will scale a lot farther than imperative systems.