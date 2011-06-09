Hurricane
=========

Hurricane, a [Tornado](http://www.tornadoweb.org) inspired Warp powered web framework written in Haskell.

__The information below will be moved into the wiki.__

### Why Hurricane?

Dynamic languages are popular on the web because they are easy to use. We're forced to use 
Javascript on the client, so why not just put up with it on the server too? 

Most people will tell you to use static languages for things like correctness, proofs, 
and security. Our reasoning is simple. Programming in a dynamic language requires actually 
running the code to deduce simple errors like typos and type mismatches. Static code tells
you these things before you get to actually run it.

Of course, imposing things on the programmer can be both aggravating and time-inefficient. However,
silly mistakes will always produce errors or undefined behavior (ew) when you run the code. 
Dealing with them at compile-time is smarter, but it usually comes at a cost.
With Haskell, we get concise code, but it's tough to understand. The goal of Hurricane is to abstract 
the confusing stuff away and make it easy for you to work in a world where "code that compiles just works."

The other significant advantage is speed. Haskell is powered by Warp, which has some 
[impressive benchmarks](http://www.yesodweb.com/blog/2011/3/preliminary-warp-cross-language-benchmarks).
With GHC 7 and Iteratees, Haskell has all the tools to be the next big concurrent language. 
Of course, this stuff is pretty tough (at least for us) to understand. Hurricane makes
this power available, but with the ease of programming in something like Tornado.

Finally, people argue that dynamic code is easier to iterate. This is true in the short short term. But
we believe that knowing the type of something in advance gives you a little bit of a guarantee
about what it does. You know ahead of time what you're breaking when you change something, so you
can try out a new idea, and then know exactly what you have to fix to make everything
work again. In a dynamic setting, you have to wait to run a bunch of different scenarios before 
finding this out.

### Design Principles

1. Code should not favor performance over ease-of-use. Performance is important, and that's why we're using
   Warp, but nowadays iterating over ideas is the top priority, and this relies on readability.
2. Code should not be concise at the expense of readability. Hurricane was written 
   by people relatively new to the language, and we want you to be able to understand our code
   without wondering why things typecheck and work.
3. You should not have to generate files or code to use Hurricane. To us, Tornado stands because out it 
   doesn't impose a design paradigm or force you to organize your project with a specific convention. 
   It stays out of the way for the most part, and most of its functionality lies in the library tools it provides. 
   We want the same for Hurricane.
4. If it compiles it works... almost. Certain pieces of a webapp are suited to parsing at runtime,
   like routes ("/foo/bar" gets this handler) and templates (programming logic injected into html). 
   But, any such extension should be parsed when the application starts up. In the case of an error, 
   it should prevent the application from serving traffic and clearly notify what the problem is.
