# hypercrud.client

Hypercrud is a fully immutable application architecture (database through UI), **focused on form-heavy CRUD**. Our goal is to **separate the _UI as an expression_ from an _I/O runtime_**, where the UI expression declares data dependencies, and the I/O runtime loads data dependencies, such that all load-time I/O is handled by the runtime and the UI expression can remain as pure as possible.

Hypercrud is built on Datomic and is similar to [The Web After Tomorrow](http://tonsky.me/blog/the-web-after-tomorrow/). Hypercrud does not use DataScript on the client.

Hypercrud has three major parts:
 
- data driven UI as an expression (hypermedia)
- Transaction system for atomic graph updates
- UI Runtime for abstracting over I/O of graph data fetching
 
Data driven UI is the primary goal of this project and all other parts are driven by this goal.

Separating the UI Expression from data-fetching I/O means we can implement a truly composable UI system where UI components are functions. This is different from using React.js with a REST backend, as the **React UI components will need to do ajax** or other data fetching. **Hypercrud UI components do not load data; all I/O is done by the Hypercrud runtime**. Hypercrud UI components are not async, they have no error surface area, and they do not require thinking about network performance.

## technical details

Hypercrud has two layers of abstraction: a lower-level I/O runtime enabling separation of UI from I/O; and a higher-level data-driven hypermedia layer built on top of UI expressions.

The lower layer is roughly analogous to a Datomic peer running in the browser. This is implemented as the Hypercrud protocol:

![hypercrud protocol snippet](.readme-assets/defprotocol-hypercrud.png)

This protocol may be implemented on top of HTTP, websockets or any other network layer, the implementation is responsible for answering queries out of cache as well as coordinating with the hypercrud service to populate caches. Implementations of this protocol are complicated, run in various contexts (e.g. browser or nodejs). It is provided as a library dependency.

The hypermedia layer is a set of data-driven UI components. Since this is implemented on top of the hypercrud runtime, these UI components are just functions which can be composed into a UI expression.

For the time being we are focusing on the data driven hypermedia layer, which is driving progress in the lower level I/O runtime. We are not sure how useful it is to use just the lower level UI runtime as the basis for other layers, though they are decoupled. In the future perhaps these two layers will be split into separate projects; but for now they remain unified.

The hypercrud runtime will run the UI expression on a web server to help decide which data to send to the browser. The execution of the UI expression is just one input into the algorithm that chooses which data to inline; other signals like http referrer header, device and user are also used. The hypercrud runtime will also use the UI expression to transparently provide server side rendering - many hypercrud applications work with javascript disabled.

The basis of the hypermedia layer is the Hypercrud Browser, a general hypermedia client as a library. Hypercrud Browser is analogous to a HTML web browser, which is fully general and can interpret all possible HTML expressions. [Hypercrud Browser Demo](http://seattle.hypercrud.com/browser/). Hypercrud Browser is composed of several hypermedia data driven UI functions such as this hypermedia form:

![data driven form](.readme-assets/data-driven-form.png)

Here is the full code for this form:

![form source code snippet](.readme-assets/form-source-code.png)

- `form` is hypermedia metadata for rendering the form, 
- `local-datoms` is ui state for uncommitted form updates - this is a timeseries
- `(hc/with client @local-datoms)` is how we make our local updates locally available to down-tree graph queries; this reference is rebuilt when a local-datoms atom change causes a render
- `local-transact!` is how the form appends more updates for each user action
- `server-transact!` is how we send our local-datoms to the server as a transaction

This hypermedia form is backed by the hypercrud graph, and can navigate recursively into the graph.

![data driven form recursion](.readme-assets/data-driven-form-recursion.png)

This hypermedia form function does have component local state but that is an implementation detail; hypercrud is currently built on React via Reagent so you can use whatever state model you like. We suggest avoiding local state for maximizing the utility of server side rendering, though the hypercrud runtime should be smart enough to discover and optimize data dependencies even in the presence of local state.
