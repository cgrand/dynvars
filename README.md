# dynvars

This library is another proposal at extending the Clojurescript dynamic vars story. Especially binding conveying. Due to the differences between JS and the JVM porting `bound-fn` is not the solution.

The JVM ecosystem is preponderantly relying on synchronous calls. Even when you register a listener it's a relatively rare event: once invoked the listener will generally performs only synchronous calls. In such setting, `bound-fn` is here to bridge the rare asynchronous gap.

However in the JS world there's no choice and as soon as some code may perform IO it has to be transformed in CPS code. It would imply using `bound-fn` everywhere and even in this case it wouldnt' be good. Because of the manual CPS transform, we would like to have try/catch or bindings span accross the gap. 

This proposal has two layers:
 * the first layer is about implementing primitives like `push-frame!`, `pop-frame!` in a relatively cheap manner,
 * the second layer offers a new set of macros targeted to JS usages. (experimental)

## Primitives

This approach builds on top of existing dynamic variables support.

## License

Copyright © 2017 Christophe Grand

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
