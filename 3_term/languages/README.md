# C++

### Proxy server
Course project. Consists of several parts:
- `utils`. Helper structures for multithreading, logging and operations with strings.
- `network`. Low-level RAII wrappers for different socket types and wrapper for epoll with a higher-level interface.
- `network::http`. Parsing of HTTP requests and DNS-resolving.
- `proxy.cpp`. Proxy server based on those libraries.

### Optional
My implementation of ```optional``` class.

```optional<T>``` is a class, which either stores a value of type ```T```, or stores "nothing".

##### See also
- [```boost::optional```](https://github.com/boostorg/optional)
- [```Data.Maybe``` in Haskell](https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Maybe.html)
