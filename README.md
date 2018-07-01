# Hyperfiddle — a Hypermedia Function

> composable application medium, built on managed data-sync infrastructure. <http://www.hyperfiddle.net/>

If React.js is managed DOM, Hyperfiddle is managed database and network.

The "web framework of the 2020s" must solve data sync — a slow, asynchronous, failure-prone concern wired throughout almost the entire codebase — the last remaining icky bit that doesn't quite fit into functional programming due to all the network side effects involved. The Cognitect stack has the right primitives to build "functional" data sync by orienting the entire stack around values and immutability.

> Cognitect stack: value-orientation at every layer: storage ↔ database ↔ service ↔ web ↔ client ↔ view
>
> * Clojure/Script - Value-oriented programming on any platform
> * EDN - Notation for values
> * Transit - Value interchange between systems
> * Datomic - Database as a value
> * (supplemented with React.js/Reagent – View as a value)

Hyperfiddle uses the Cognitect stack as a basis to abstract over client/server data sync for APIs, by extending Datomic's immutable database semantics to the API. Unlike REST/GraphQL/whatever, Hyperfiddle's data sync *composes*.

### Functional data sync

Functional data sync is an infrastructure heavy problem any way you frame it. There are gonna be CDNs and cloud lambdas and distributed caches involved, it's going to be more than a jar file. We call this abstraction an *I/O runtime*: a small kernel implementing an I/O strategy and coordinating infrastructure, to get the right data to the right places, so that userland can program with functions and values, blissfully unconcerned with the real-world effects underlying. I/O runtimes are general purpose and pluggable; you can swap in different I/O runtimes for diverse I/O needs — client/server http, server rendered pages, predictive prefetching, realtime — without changing the application. General-purpose I/O is an abstraction-resistant class of problems which is why web developers code essentially the same boilerplate over and over in each project — shuffling data from the database through a web serivce to a UI and then back — but end-to-end immutability (including through the database) is the fundamental breakthrough which we think permits an abstract solution.

Managed I/O means, as a web dev, you are no longer concerned with remote data fetching or coding HTTP backends. In fact there is hardly any "web programming" left at all. But managed I/O is not the point. The point is: *what does managed I/O make possible that wasn't possible before?* 

# Dependency coordinates — Todo

    [com.hyperfiddle/hyperfiddle "0.0.0"]
