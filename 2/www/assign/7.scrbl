#lang scribble/manual
@(require scribble/core)
@(require "../utils.rkt")
@(define (colorize c . content)
  (elem #:style (style #f (list (color-property c)))
        content))

@assn-title[7]{Traversing Trees}

This is assignment is to be completed and submitted individually. You
may not work with anyone else.

@bold{Due}: Wednesday, May 8, 11:59:59 PM EST.

For this assignment, download the following zip file:
@link["Assign7.zip"]{Assign7.zip}.  Save the file somewhere on your
computer and unzip it.  This will create a directory called
@tt{Assign5} with an IntelliJ project inside.  It contains all the
libraries you will need and some code to get you started.


@section{Iterators over Trees}

You have seen how to build iterators over sequential data structures
like lists, where the representation of an iterator is pretty simple:
it is the remaining elements of the list that have yet to be iterated.

This assignment asks you to consider a more complicated task:
implement iterators for binary trees.

The elements of a binary tree could be iterated in several different
orders.  You will need to implement two of them: an in-order iterator
and pre-order iterator.  

In-order means that for a node, all the elements of the left subtree
are traversed, then the element, the all the elements of the right.

Post-order means that for a node, all the elements of the left subtree
are traversed, all the all the elements of the right are traversed,
then the element.  (Unlike NLR, which went node, left, right.)

For full credit, each of the operations (including the construction of
the traversal) should have a cost bounded by the height of the tree.
For partial credit, implement the traversal but ignore this
requirement.

Hint: make examples on pencil and paper to help guide you through what
information will need to be represented.  Spending a bit of time doing
this will save you loads of time coding.

You may add any classes or methods you find useful to complete this
assignment.

@section[#:style 'unnumbered #:tag "assign7:submit"]{Submission}

Use @tt{submit.cs.umd.edu} to submit your solution to the problems.
You should create a zip file called @tt{Assign7.zip} that contains the
IntelliJ project containing your solutions.
