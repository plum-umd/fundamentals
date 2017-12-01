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

@title[#:tag "lab12"]{6/14: Full Binary Trees}

The goal of this lab is to make progress on implementing the tools
you'll need for the current assignment.

@lab:section{Withdrawn partner?}

Is anybody currently paired with a partner who withdrew from the
class?  If so raise your hand.  Look for someone else who raised their
hand.  Work together.  (Please email @tt{dvanhorn} if you're able to
find a new partner.)

@lab:section{Full Binary Trees}

One of the things you'll need for the current assignment is a library
for working with full binary trees.  Let's start with the interface:

@verbatim|{
// Represents a full binary tree
// A full binary tree is a binary tree where each node's left and
// right subtree have the same size.
interface FBT<X> {
   // Compute the size of this tree
   Integer size();

   // Compute the height of this tree
   Integer height();

   // Get the ith element of this tree
   // Element 0 is at the root
   // Element 1 is at the root of the left tree
   // Element (size()+1)/2 is at the root of the right tree
   // Assume i < size().
   X get(Integer i);
}
}|

@exercise{Design data definitions for node and leafs of full binary
trees.}

Your constructor for nodes may assume the given left and right
subtrees are of equal size.  Stuff will likely break if this
assumption is violated, but when you get to implementing quick lists,
you can ensure this never happens.

@exercise{Design the @tt{height} method.  Be sure to utilize the
invariant of full binary trees so that your method is fast.  In
particular it should be logarithmic in the size of the tree.}

@exercise{Design the @tt{size} method.  Be sure to utilize the
invariant of full binary trees so that your method is fast.  In
particular it should be logarithmic in the size of the tree.  Hint:
use @tt{height} and @emph{calculate} the size.}

@exercise{Design the @tt{get} method.  You may need to develop helper
methods to complete this design.}

Once you have a fully tested system, move on to quick lists.


