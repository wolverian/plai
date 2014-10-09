# PLAI in Typed Racket

This is an implementation of the code of the
[Programming Languages: Application and Interpretation][plai],
[2012 version][].

The implementation language in the textbook is Racket, with additional
static typing. You can install that language with `raco pkg install
plai-typed`.

This implementation uses [Typed Racket][tr] instead. This includes
pattern matching with `match`. There is some custom sugar for defining
abstract data types.

[2012 version]: http://cs.brown.edu/courses/cs173/2012/book/

[plai]: https://en.wikipedia.org/wiki/Programming_Languages%3A_Application_and_Interpretation

[tr]: http://docs.racket-lang.org/ts-guide/index.html "Typed Racket Guide"

## See also:

- The newer versions of the textbook, which use Pyret as the
  implementation language: <http://papl.cs.brown.edu>.
