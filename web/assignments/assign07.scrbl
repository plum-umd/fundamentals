#lang scribble/manual
@(require "../utils.rkt"
	  "../unnumbered.rkt"
          (for-label (except-in class/2 empty cons first rest list-ref length e check-expect))
          (for-label (only-in lang/htdp-intermediate-lambda check-expect))
	  (for-label class/universe))

@title[#:tag "assign08"]{2/29: Quick Visits}

Due: 2/29.

Language: @racketmodname[class/2]

@section{Quick Visits}

This problem builds on the @emph{quick lists} problem from last week.

Here was the interface you should have implemented for lists using the
@emph{quick list} data structure that supports a fast
@racket[list-ref] method:

@codeblock{
;; A [List X] implements
;; - cons : X -> [List X]
;;   Cons given element on to this list.
;; - first : -> X
;;   Get the first element of this list 
;;   (only defined on non-empty lists).
;; - rest : -> [List X]
;;   Get the rest of this 
;;   (only defined on non-empty lists).
;; - list-ref : Natural -> X
;;   Get the ith element of this list 
;;   (only defined for lists of i+1 or more elements).
;; - length : -> Natural
;;   Compute the number of elements in this list.

;; empty is a [List X] for any X.
}

Make sure your quick list implementation is working and place it into
a file named @racket{quick-lists.rkt}.  That file should provide one
name, @racket[empty], by including the following at the top of the
file:

@codeblock{
(provide empty)
}

In a file named @racket{slow-lists.rkt} @emph{re-}develop an
implementation of the list interface, but in the usual way as a
recursive union of @racket[mt%] and @racket[cons%] classes.  That file
should also provide @racket[empty] by including the same line above at
the top.

Finally, start a third file called @racket{use-lists.rkt} that will
make use of both kinds of lists by including the following at the top
of the file:

@codeblock{
(require (prefix-in q: "quick-lists.rkt"))
(require (prefix-in s: "slow-lists.rkt"))
}

You now have two lists: @racket[q:empty] and @racket[s:empty];
both are represented in very different ways, but so long as you use
them accoring to the list interface, they should be indistinguishable.

Let's now revise the @tt{[List X]} interface to include support for
visitors:

@codeblock{
;; A [List X] implements ...
;; - accept : [ListVisitor X Y]
;;   Accept given visitor and visit this list's data.

;; A [ListVisitor X Y] implements
;; - visit-mt : -> Y
;;   Visit an empty list.
;; - visit-cons : X [Listof X] -> Y
;;   Visit a cons lists.
}

Implement the revised @tt{[List X]} interface in both
@racket{quick-lists.rkt} and @racket{slow-lists.rkt}.

In @racket{use-lists.rkt} you should be able to define particular
visitors and have it work on @emph{both} representations of lists.  As
an example, here is a list visitor that computes the length of a list:

@codeblock[#:keep-lang-line? #f]{
#lang class/2
;; A (new length%) implements [ListVisitor X Natural].
;; List visitor for computing the length of a list.
(define-class length%
  (define (visit-mt) 0)
  (define (visit-cons x r)
    (add1 (r . accept this))))

(define len (new length%))

(check-expect (q:empty . accept len) 0)
(check-expect (s:empty . accept len) 0)
(check-expect (q:empty . cons 'c . cons 'b . cons 'a . accept len) 3)
(check-expect (s:empty . cons 'c . cons 'b . cons 'a . accept len) 3)
}

And here's one for the sum of a list of numbers:

@codeblock[#:keep-lang-line? #f]{
#lang class/2
;; A (new sum%) implements [ListVisitor Number Number].
;; List visitor for computing the sum of a list of numbers.
(define-class sum%
  (define (visit-mt) 0)
  (define (visit-cons n r)
    (+ n (r . accept this))))

(define sum (new sum%))

(check-expect (q:empty . accept sum) 0)
(check-expect (s:empty . accept sum) 0)
(check-expect (q:empty . cons 3 . cons 4 . cons 7 . accept sum) 14)
(check-expect (s:empty . cons 3 . cons 4 . cons 7 . accept sum) 14)
}

Implement a @tt{[ListVisitor X X]} named @racket[reverse%] that
reverses a list (note: you may need to implement a ``helper'' visitor
that corresponds to the helper function you'd write for the
@racket[reverse] function).  Note that this visitor will have to
commit to produces either a quick list or a slow list, but it really
doesn't really matter which... well, except for testing.  So for
example, let's say the reverse visitor produces slow lists.  Then we
would expect the following test to pass, assuming @racket[reverse%]
works as specified:

@codeblock[#:keep-lang-line? #f]{
#lang class/2
(define rev (new reverse%))

(check-expect (q:empty . accept rev) s:empty)
(check-expect (q:empty . cons 'c . cons 'b . cons 'a . accept rev) 
              (s:empty . cons 'a . cons 'b . cons 'c))
}

Of course, this isn't ideal since our @emph{test} is testing more than
is actually required of @racket[reverse%].  In particular, it should
be perfectly acceptable for @racket[reverse%] to produce quick lists
without tests failing.

What's happening here is that @racket[check-expect] is checking too
much because it is not treating the objects it compares solely
according to their interface.  We will see how to fix this problem by
defining an interface-respecting equality computation, but for now,
just test as shown above.

Now to build some larger pieces with visitors.  First, here's an
interface definition for functional objects that represent functions
from @tt{X}s to @tt{Y}s.  Such an object has a single method called
@racket[apply] that consumes an @tt{X} and produces a @tt{Y}:

@codeblock[#:keep-lang-line? #f]{
#lang class/2
;; A [Fun X Y] implements
;; - apply : X -> Y
;;   Apply this function to given x.
}

Here's an interface definition for functional objects that represent
predicates:

@codeblock[#:keep-lang-line? #f]{
#lang class/2
;; A [Question X] implements
;; - ask : X -> Boolean
;;   Ask if this predicate holds on x.
}

Now implement the following two visitors:

@codeblock[#:keep-lang-line? #f]{
#lang class/2
;; A (new filter% [Question X]) implements [ListVisitor X [List X]].
;; Filters visited list to produce a list of elements satisfying predicate.

;; A (new map% [Fun X Y]) implements [ListVisitor X [List Y]].
;; Maps visited list to produce a list of results of applying the function.
}

Implement at least one @tt{[Fun Number String]} and one @tt{[Question
String]} to use for testing @racket[filter%] and @racket[map%].


@subsection*{Folds vs Visitors}

We can also implements @emph{folds} over lists, in both for both kinds
of lists.  Extend your implementation of lists (both kinds) to support
the @racket[fold] method:
@codeblock{
;; A [List X] implements ...
;; - fold : [ListFold X Y]
;;   Accept given fold and process this list's data.

;; A [ListFold X Y] implements
;; - fold-mt : -> Y
;;   Process an empty list.
;; - fold-cons : X Y -> Y
;;   Process a cons lists.
}

Now revise your implementations of the @racket[filter%] and
@racket[map%] to implement folds as well as visitors.  Be sure to
specify what interfaces they implement.  

Finally, implement the class @racket[list-ref%]:
@codeblock[#:keep-lang-line? #f]{
#lang class/2
;; A (new list-ref% Number) implements [ListVisitor X X]
;; and [ListFold X X].
;; Retrieves the element at the specified index.
}

Which implementation, the fold or the visitor, is more elegant?  Which
was more elegant for @racket[map%] and @racket[filter%]?

@section{Universe Setup}
