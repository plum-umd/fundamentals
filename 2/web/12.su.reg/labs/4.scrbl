#lang scribble/manual

@(require "../lab.rkt"
          "../unnumbered.rkt"
          "../utils.rkt"
          scribble/eval)

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require lang/htdp-intermediate))
    the-eval))


@(define exercise (exercise-counter))

@title[#:tag "lab04"]{5/17: Accumulators and Parameterized Data Definitions}

The goal of this lab is to practice accumulator-based designs with
objects and to start working with parameterized data definitions
(often called ``generics'' in Java).

@lab:section{Accumulating knowledge}

In class, we developed a straightforward approach to reversing a list
using structural recursion.  Unfortunately, that approach, since it
involves a double recursion over the list, takes time proportional to
the square of the size of the list.  We then did much better by
developing an equivalent accumulator-based designed that accumulated
the elements we'd seen so far in reverse order.  That code, which we
developed in ISL, was:

@verbatim{
;; Reverse the given list of integers
;; LoI -> LoI
(define (my-reverse lon)
  (reverse/acc lon empty))

;; LoI LoI -> LoI
;; ACCUM: rev represents list of elements we've seen so far, in reverse order.
(define (reverse/acc lon rev)
  (cond [(empty? lon) rev]
        [(cons? lon)
         (reverse/acc (rest lon)
                      (cons (first lon) rev))]))
}

Now we'd like to translate this idea over to Java.

@exercise{Develop a @tt{reverse} method using an accumulator-based
design in Java.  You must support the original interface for
@tt{reverse}, namely that any @tt{LoI} has a @tt{reverse} method that
accepts no inputs and produces the list in reverse order.}

Once you've seen how to develop @tt{reverse}, you can apply the same
principles to design other accumulator-based methods.

@exercise{Develop a @tt{sum} method using an accumulator-based
design.}

@exercise{Develop an @tt{append} method using an accumulator-based
design.  The @tt{append} method should consume one argument which is a
@tt{LoI} and append this list and the given list together.}


@lab:section{Generalizing types}

Yesterday we saw how to generalize the list of integers data
definition to represent lists of any kind of elements.

@exercise{Develop a general @tt{List<X>} data definition and recreate
the methods from above.}

You should notice that the @tt{sum} method no longer type-checks.  Why
is that?  For the moment, just comment out the @tt{sum} method and
proceed with the development of @tt{reverse} and @tt{append}.  We'll
talk more about how to handle the @tt{sum} issue in class.

Lists are the only thing that generalize.  On today's exam, you
developed a data definition for binary trees of integers.  Now try
your hand at desigining a parameterized version of binary trees.

@exercise{Design a @tt{BT<X>} data definition for binary trees where
nodes contain data of type @tt{X}.}

@exercise{Design a @tt{height} and @tt{size} method for @tt{BT<X>}
that computes the height of a tree and the number of elements it
contains, respectively.}


@exercise{Design the @tt{mirror} method for @tt{BT<X>} that computes
the mirror image of the binary tree.}

@exercise{Using the @tt{Predicate<X>} interface we studied in class,
design an @tt{ormap} method for binary trees that computes whether any
element of the tree satisfies a given predicate.  Design an
analogous @tt{andmap} method.}

@exercise{Design a predicate on integers that produces true only for
even numbers.  Use this predicate to test your @tt{ormap} and
@tt{andmap} designs from above.}