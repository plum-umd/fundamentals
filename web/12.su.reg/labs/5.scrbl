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

@lab:section{Visiting lists}

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

@lab:section{Visiting the tree of life}

Now you should try your hand at developing visitors for other kinds of
data.  Revisit your solution to ``The Tree of Life'' problem from
@seclink["lab02"]{Lab 2}.

@exercise{Implement the visitor pattern for your representation of
life scenarios.}

@exercise{Redevelop your solutions to exercise 10-14 as visitors.}

@exercise{Re-develop your solutions to exercise 16 and 17 as 
visitors that produce @tt{List<String>}s.}
