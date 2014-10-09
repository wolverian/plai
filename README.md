# PLAI in Typed Racket

This is an ongoing implementation of the code of the
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

## Instructions

### Running

Load `plai-typed-racket.rkt` into Racket.

### Tests

The tests run immediately when you load the program into Racket.

## See also

- The [newer versions][new] of the textbook, which use [Pyret][pyret]
  as the implementation language.

[new]: http://papl.cs.brown.edu "Programming and Programming Languages"

[pyret]: http://www.pyret.org "Pyret"

## License

This software is available under the ISC license. See the LICENSE file
for more information.
