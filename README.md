# hyperfiddle.cljc — a Hypermedia Function <http://www.hyperfiddle.net/>

Hyperfiddle isolates your web clients from I/O, so your code can stay pure. If React.js is managed DOM,
 Hyperfiddle is managed network and database. This enables a new kind of composable primitive for constructing web software, with paradigm-changing implications.

## Dependency coordinates — Todo

    [com.hyperfiddle/hyperfiddle "0.0.0"]

## Motivation

**The hardest part of web dev is I/O:** data sync between database, various services, UI and then back to the database. An async, slow, failure-prone concern wired throughout the full stack, and the reason why we all code the same web boilerplate over and over again, year after year – a collosal, industry-wide failure to abstract.

Hyperfiddle makes data-sync invisible with immutability— If React.js is managed DOM, Hyperfiddle is managed database and network.

* Fiddle graph captures service inter-dependencies as data (query X depends on queries Y and Z)
* Fiddle graph captures application essense (API, UI, database) as one concern – no frontend/backend dichotemy
* Managed data sync – Userland is not concerned with effects, async, errors or latency
* Transport layer independence – swap transport stratgies (e.g. REST, websocket) without changing your app
* Optimizing I/O runtime – dynamically adjust transport strategies to balance caching and latency
* Platform independence – run on any platform (e.g. browser, Node, mobile) without changing your app

**Framework or library?** Neither: Hyperfiddle is more like Apache or Nginx, it's part of your infra stack.
There is a server library for making custom servers (e.g. integrations and control over data sync) and a
client library ("browser") for talking to it.

## How does it work?

The Cognitect stack has the right primitives to build "functional" data sync by orienting the entire stack
around values and immutability.

* Clojure/CLJC - Value-oriented programming on any platform
* EDN - Extensible notation for values
* Transit - Efficient value interchange between foreign systems
* Datomic - Database as a value
* (supplemented with React.js/Reagent – View as a value)

Hyperfiddle uses the Cognitect stack as a basis to abstract over client/server data sync for APIs, by extending
Datomic's immutable database semantics to the API. Unlike REST/GraphQL/whatever, Hyperfiddle's data sync *composes*.

Managed I/O means, as a web dev, you are no longer concerned with remote data fetching or coding HTTP backends.
In fact there is hardly any "web programming" left at all. But managed I/O is not the point. The point is:
*what does managed I/O make possible that wasn't possible before?*
