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

@title[#:tag "lab05"]{5/22: Visitors}

The goal of this lab is to practice designing visitors and adding the
visitor pattern to inductively defined data.

@lab:section{Visiting data}

The visitor pattern is a design technique that decouples data from
functions.  Once a data definition implements the @emph{visitor
pattern}, it allows new computations to be designed without the need
to change the original definition.  This is particularly important for
parametric data definitions, since we can design type-specific
visitors, overcoming the inability to add type-specific methods.

In this lab, you'll practice adding the visitor pattern to various
data definitions and writing computations as visitors.

We saw in class the following data definition for parametric lists
with the visitor pattern:

@verbatim|{
// Represents a computation over a list of X, producing an R.
interface ListVisitor<X,R> {
  R visitEmpty();
  R visitCons(X first, List<X> rest);
}

// Represents a list of X
interface List<X> {
  // Accept visitor and compute.
  <R> R accept(ListVisitor<X,R> v);
}

class Empty<X> implements List<X> {
  Empty() {}

  public <R> R accept(ListVisitor<X,R> v) {
     return v.visitEmpty();
  }
}

class Cons<X> implements List<X> {
  X first;
  List<X> rest;
  Cons(X first, List<X> rest) {
    this.first = first;
    this.rest = rest;
  }

  public <R> R accept(ListVisitor<X,R> v) {
    return v.visitCons(this.first, this.rest);
  }
}
}|

To get started, let's construct some simple visitors.

@exercise{Develop an implementation of @tt{ListVisitor<String,Integer>}
that computes the aggregate length of a list of strings.}

@exercise{Develop an implementation of @tt{ListVisitor<String,Boolean>}
that computes whether a list of strings has an element @tt{"Boston"}.}

@exercise{Develop an implementation of @tt{ListVisitor<String,String>}
that a list of strings together.}

@exercise{Develop an implementation of
@tt{ListVisitor<Integer,Boolean>} that computes whether a list of
integers has an element greater than 50.}

Now let's do some more sophisticated ones:

@exercise{Develop an implementation of
@tt{ListVisitor<String,Boolean>} that computes whether a list of
strings has a @emph{given} element.}

@exercise{Develop an implementation of
@tt{ListVisitor<Integer,Boolean>} that computes whether a list of
integers has an element greater than a @emph{given} number.}

Now for some even more sophisticated ones (these may involve writing
helper visitors):

@exercise{Develop an implementation of
@tt{ListVisitor<Integer,Integer>} that computes the largest number in
a @emph{non-empty} list of integers.}

@exercise{Develop an implementation of
@tt{ListVisitor<Integer,Integer>} that computes the shortest string in
a @emph{non-empty} list of integers.}

@exercise{Develop an implementation of
@tt{ListVisitor<Integer,Integer>} that computes a number closest to a
@emph{given} number in a @emph{non-empty} list of integers.}





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