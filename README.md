# Hypercrud Browser

> navigate EDN app-values like a web browser navigates HTML

Hypercrud is a Clojure and ClojureScript system for building sophisticated CRUD apps. The key is Datomic: by leveraging immutability from database to frontend, we can build a fully general data server which is correct, consistent yet also performant. A general purpose data server permits us to escape the backend-for-frontend pattern, such that we can service N different frontends all with from the same backend. The only trusted backend code is a tiny security kernel; the rest of the code need not be trusted, and thus all domain logic is moved to the client.

Hypercrud Client is an I/O runtime that handles efficient client-server data sync with Hypercrud Server. Inspired by Om Next, the userland interface is two functions: a request function to specify data dependencies, and a view function (React.js expression). The runtime will fetch the data dependencies as specified by the request function, and then pass that value to the view. By separating I/O, we are left with a composable programming model of pure functions against local data. Userland experiences no async, no failures, no latency. Sheltering userland from I/O permits us to model the app as an EDN value.

Hypercrud Browser navigates app-values like a web browser navigates HTML. App-values define Pages, each Page declares his data dependencies, and Links to other Pages. Pages compose by composing their data dependencies therein (like an iframe), and are thus a scalable model for building UIs. Hypercrud Browser is HATEOAS.

![](http://i.imgur.com/4mKpHhw.png)

[Hyperfiddle](http://hyperfiddle.net/) is a WYSIWYG editor for building Hypercrud app-values, and optionally, a cloud platform to host them in.

![](http://i.imgur.com/lhGmOqX.png)
