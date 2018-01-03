# Hyperfiddle—data-driven CRUD applications

This is the open source library powering <http://www.hyperfiddle.net>.

## Dependency coordinates—Todo

    [com.hyperfiddle/hyperfiddle "0.0.0"]

## Community

We are eager to talk to you so please reach out!

* <https://www.reddit.com/r/hyperfiddle/>
* Slack: #Hyperfiddle @ [clojurians](http://clojurians.net/)
* <https://groups.google.com/forum/#!forum/hyperfiddle>

## Philosophy

Hyperfiddle is built in layers

* Low level I/O runtime for client/server data sync, such that userland is pure functions (**app-as-a-function**)
* High level data-driven interpreter function (**app-as-a-value**)

You can code at either level. Both data and functions compose properly (it is just Clojure functions and Clojure data).

## App-as-a-Function

UI, at its essense, is about two concerns:

* a view (pixels) — a function returning the virtual dom
* the data backing it — a function returning the query

Hyperfiddle UI components work like this. The functions are pure.

Here is a request function:

    (def datomic-samples-blog #uri "datomic:free://datomic:4334/samples-blog")
    
    (def race-registration-query
      '[:find (pull ?e [:db/id :reg/email :reg/gender :reg/shirt-size])
        :in $ :where [?e :reg/email]])
    
    (def gender-options-query
      '[:find (pull ?e [:db/id :reg.gender/ident])
        :in $ :where [?e :reg.gender/ident]])
    
    (defn request [state peer]
      (let [$ (hc/db peer datomic-samples-blog nil)]            ; nil is HEAD (related to time-basis)
        [(->QueryRequest race-registration-query {"$" $})
         (->QueryRequest gender-options-query {"$" $})]))

Here is the corresponding view function, it is simple and just prettyprints the resultset into a :pre:

    (defn view [state peer]
      (let [$ (hc/db peer datomic-samples-blog nil)]
        [:ul
         (->> @(hc/hydrate peer (request state $))              ; synchronous and reactive
              (map (fn [relation]
                     (let [e (get relation "?e")]
                       [:li {:key (:db/id e)}
                        [:pre (with-out-str (pprint e))]]))))]))
                        
Notes about these functions
* Request fns return seq; you can hydrate many queries in bulk.
* Both are CLJC, runs in both JVM and browser
* I/O runtime manages these fns to load data and then render views
* Request fn is called repeatedly until its return value stabalizes
* This permits queries that depend on the results of earlier queries  
* Data loop typically runs in JVM Datomic Peer, so no network hops or N+1 problem
* Sometimes the runtime chooses to loop in the browser, i.e. a little, incremental 
data load in response to a UI state change

This is how Hyperfiddle insulates the application programmer from I/O. No browser/service round trips like 
REST, no service/database round trips like SQL. No object/relational impedance mismatch. App-as-a-fn is a 
wonderful, functional way to write your web dashboards, better than anything else that exists today.

Managed I/O permits many **optimizations that human-coded I/O cannot do:**
* Automatic I/O partitioning and batching, optimized for cache hits
* all requests have a time-basis, all responses are immutable
* Integrated Hyperfiddle-aware CDN (serve APIs like static sites)
* Built-in server side rendering
* Works with browser javascript disabled

Now that I/O is solved, we can start building *real, composable abstractions:*

## App-as-a-Value

Here is the above functions, represented as a Hyperfiddle EDN value:

    ; race registrations table
    {:db/id 17592186045418,
     :fiddle/query "[:find (pull ?e [:db/id :reg/email :reg/gender :reg/shirt-size])
                     :in $ :where [?e :reg/email]]",
     :fiddle/type :query,
     :fiddle/links #{{:db/id 17592186045434,
                      :link/rel :options,
                      :link/fiddle #:db{:id 17592186045435},
                      :link/render-inline? true,
                      :link/dependent? true,
                      :link/path "0 :reg/shirt-size",
                      :link/formula "(fn [ctx]\n{:request-params\n  {\"?gender\" (get-in ctx [:cell-data :reg/gender])}})"}
                     {:db/id 17592186045427,
                      :link/rel :options,
                      :link/fiddle #:db{:id 17592186045428},
                      :link/render-inline? true,
                      :link/path "0 :reg/gender"}
                     {:db/id 17592186045481, :link/rel :sys-new-?e, :link/fiddle #:db{:id 17592186045482}}}}
     
    ; gender options for select
    {:db/id 17592186045428,
     :fiddle/links #{{:db/id 17592186045474, :link/rel :sys-new-?e, :link/fiddle #:db{:id 17592186045475}}},
     :fiddle/name "gender/size genders",
     :fiddle/query "[:find (pull ?e [:db/id :reg.gender/ident])\n:in $\n:where [?e :reg.gender/ident]]",
     :fiddle/type :query}

Actually these values do a lot more than the above functions do. Data is more information dense than code, kind of like 
how a picture is worth 1000 words.

> [![](https://i.imgur.com/iwOvJzA.png)](http://dustingetz.hyperfiddle.net/ezpjb2RlLWRhdGFiYXNlICJzYW5kYm94IiwgOmxpbmstaWQgMTc1OTIxODYwNDU0MTh9)
>
> *Above EDN values are about half of the data that defines the [gender/shirt-size demo](http://dustingetz.hyperfiddle.site/ezpjb2RlLWRhdGFiYXNlICJzYW5kYm94IiwgOmxpbmstaWQgMTc1OTIxODYwNDU0MTh9).*

Obviously, data is better in every possible way (structural tooling, decoupled from platform, decoupled from 
performance, tiny surface area for bugs, better tasting kool-aid, etc).

Like all Hyperfiddle applications, hyperfiddle EDN values are interpreted by two functions. In this case, 
these two functions are provided by Hyperfiddle, together they comprise the *Hyperfiddle Browser*. The Browser 
is a generic app-as-a-function which interprets hyperfiddle app-values.

*web browser : HTML documents :: hyperfiddle browser :: hyperfiddle EDN values*

Hyperfiddles are graphs, not documents, so they are stored in databases. Databases storing hyperfiddles are like 
git repos storing the "source code" (data) of your hyperfiddles. Like git, there is no central database.

## How far will it scale?

Without immutability in the database, all efforts to abstract higher will fail; because to achieve the 
necessary performance, requires writing imperative optimizations to imperative platforms (object-relational 
impedance mismatch, N+1 problems, deeply nested JOINs, caching vs batching tradeoffs). Hyperfiddle cannot 
be built on RDBMS or Neo4J; their imperative nature leaks through all attempts to abstract. This is why all 4GLs 
historically fail. Immutability, as always, to the rescue.

How high can we abstract? We aren't sure yet. It will scale until Datomic's app-as-a-value abstraction breaks down, 
at which point programmers will manually optimize their Datomic services. Datomic Peer vs Client vs Cloud makes a 
difference here as they all have different stories for code/data locality. We think it will scale a lot farther 
than imperative systems.
