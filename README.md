# Hypercrud Browser

> navigate EDN app-values like a web browser navigates HTML

Hypercrud is a Clojure and ClojureScript system for building sophisticated CRUD apps. The key is Datomic: by leveraging immutability from database to frontend, we can build a fully general data server which is correct, consistent yet also performant. A general purpose data server permits us to escape the backend-for-frontend pattern, such that we can service N different frontends all with from the same backend. The only trusted backend code is a tiny security kernel; the rest of the code need not be trusted, and thus "service" code need no longer be trusted and can be moved to the client.

**Hypercrud Client is an I/O runtime for  efficient client-server data sync** with Hypercrud Server. Inspired by Om Next, the userland interface is two functions: a request function to specify data dependencies, and a view function (React.js expression). The runtime will fetch the data dependencies as specified by the request function, and then pass that value to the view. By sequestering I/O to the fringe of the system, we are left with a composable programming model of pure functions against local data. Userland code experiences no async, no failures, no latency. This permits "service" business logic to be a pure function that runs in the client, and since the client drives all decision making, it is fully general, like a browser.

```clojure
(def request-blog
  (->QueryRequest '[:find ?e :where [?e :post/title]]
                  {"$" #DbVal[:samples-blog]}
                  {"?e" [#DbVal[:samples-blog] ['*]]}))

(defn view [state peer dispatch!]
  (-> (mlet [result (hc/hydrate peer request-blog)]
        [:ul
         (->> result ; [{"?e" {:db/id #DbId[17592186045429 17592186045786], :post/title "Fourth blog post"}} ...]
              (map (fn [relation]
                     (let [post (get relation "?e")]
                       [:li {:key (:db/id post)}
                        (:post/title post)]))))])
      (exception/extract [:h1 "Loading..."])))

(defn request [state peer]
  [request-blog])
```

![](http://i.imgur.com/zwoGq2I.png)

A general client, plus sequestering of I/O to a runtime, lets us push all "service" code, like your database queries, into the client. The client programming experience is that of composing functions and values, which makes it a straightforward exercise to model the app as an EDN value.

**Hypercrud Browser navigates app-values like a web browser navigates HTML.** **All the things we generally have to write code for - security, performance, async, and error handling - are solved generally, the core business of an application is now simple enough to model as a value.** App-values define Pages, each Page declares his data dependencies, and Links to other Pages.

![](http://i.imgur.com/f1ngGLt.png)  
![](http://i.imgur.com/4WlmuW8.png)

```clojure
(def app-value { ... })

(defn view [state peer dispatch!]
  [browser/safe-ui app-value (:route state) {:display-mode :xray :dispatch! dispatch!}])

(defn request [state peer]
  ; code your own data dependencies, or let the browser figure it out from an app-value
  (browser/request app-value (:route state) {:display-mode :xray}))
```

Pages compose by composing their data dependencies therein (like an iframe), and are thus a scalable model for building UIs. Hypercrud Browser is HATEOAS.

![](http://i.imgur.com/4mKpHhw.png)

It is natural to want to store app-values in a database, which leads us to:

# Hyperfiddle

[Hyperfiddle](http://hyperfiddle.net/) is a WYSIWYG editor for building Hypercrud app-values. It is also a better Datomic Console - an interactive query builder, entity explorer and can be attached to an arbitrary Datomic database without changing it. It heavily leans on d/with as a transaction staging area, including a notion of branching and discard.

![](http://i.imgur.com/v3cmewv.png)