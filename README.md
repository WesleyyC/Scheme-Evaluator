# Scheme-Evaluator-Compiler
This is a modified version of compiler and evaluator for Scheme based on the SICP book.

##### Scheme
Scheme is a minimalist, multi-paradigm dialect of Lisp. It's **simple and elegant**.

##### Install
- There is a version provided by [GNU](http://www.gnu.org/software/mit-scheme/)
- Or you can install [DrRacket](http://racket-lang.org/)

### The Evaluator: mceval.scm 
This is a modifeid version of the Scheme interpreter written in Scheme which support lazy evaluation.

#### How to Run
To run the evaluator:
```shell
scheme -load mceval.scm
```

#### The Lazy Evaluation
In Scheme, if we define the following ``try``
```Scheme
(define (try a b)
  (cond ((= a 0) 1)
  (else b)))
```
If we run ``(try 0 (/ 1 0))``, the system will throw me an error because it will evaluate ``(/ 1 0)``, even though it is not used in the ``try`` function.

However, in my modified version, user can choose to delay some of the argument which can improve computation efficiency and provide more flexibility in coding. For example, if we define ``try`` in this way in the new evaluator:
```Scheme
(define (try a (delayed b))
  (cond ((= a 0) 1)
  (else b)))
```
and now if we run ``(try 0 (/ 1 0))`` again, the system will simply return ``1`` instead of throwing an error.

#### A Different Version of Stream
In Scheme, there is a built in ``stream`` which is similar to ``cons`` but with a delayed second argument. Now with the new ``dealyed`` tag, we can build a ``new-cons`` which inherits the built in ``cons`` but add a ``delayed`` tag for the second argument.

Usage: 
```Scheme
(define (integers-from n)
  (new-cons n (integers-from (+ n 1))))
```
which will give us a function that can return a infinite list of integers starting from n.

This is a great example of how delayed argument can provide better computation efficiency.

#### Other functionality
Besides the ``delayed`` tag, there are other improvement from the original meta-circular evaluator introduced in SICP.
- ``let`` which is the same as the built in ``let``
- ``let*`` which is the same as the built in ``let*``
- ``make-unbound!``which will unbound the variable in the current scope with its associated value

