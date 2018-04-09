#lang scribble/manual
@(require scribble/core)
@(require "../utils.rkt")
@(define (colorize c . content)
  (elem #:style (style #f (list (color-property c)))
        content))

@assn-title[6]{Tests, BSTs, and Efficient Maps with Comparable Keys}

UPDATES:

@itemlist[
@item{We have released the source code for assignment 5 tests so that you can see what tests you're failing on the submit server.}
@item{We removed a stub for the @tt{findComp} method that is not needed.}
]

This is assignment is to be completed and submitted with your new
@link["https://piazza.com/class/jcspfhewmdn41y?cid=108"]{partner}.  You
may not work with anyone other than your assigned partner.

@bold{Due}: Thursday, April 12, 11:59:59 PM EST.

@(define @Piazza @link["http://piazza.com/umd/spring2018/cmsc132a"]{Piazza})

@section{Revising a Library of Data structures}

For this assignment, you must revise your library of data structures
for maps, sets, and multisets.

@subsection{Passing the Test Suite}

On the submit server, you will now find a test suite that is run when
you submit your code.  You can see past results for assignment 5 and
you will see new results for every submission of assignment 6.  To get
full credit on this assignment, you must pass all of the test suite
tests (this is necessary for full credit, but not sufficient; you must
also submit well-designed code.)

@subsection{Adding @tt{BST}s and @tt{MapComp}s}

You must also implement a new data structure: binary search trees
(BSTs) and maps built using BSTs.

Add the following @link["BST.java"]{@tt{BST.java}} code to your
project and implement all of the methods in the interface using the
provided classes.  You may add any methods you like, but you may not
remove or change any of the signatures given in the interface.

Every BST object should satisfy the @emph{BST invariant} which is the
following property:

A BST b has the BST property if and only if: (a) it is a leaf or (b)
it is a node and for all values in the left subtree, they compare as
smaller or equal (using the Java built-in
@link["https://docs.oracle.com/javase/9/docs/api/java/lang/Comparable.html"]{Comparable}
interface) as the value in the node and for all values in the right
subtree, they compare as greater than the value in the node.

Once you have a working BST implementation, add the
@link["MapComp.java"]{@tt{MapComp.java}} code to your project.

The idea behind a @tt{MapComp} object is that it implements a map-like
data structure in that it maps unique keys to values, but it requires
the keys to be comparable.  Therefore, it can create a binary search
tree of pairs of keys and values and use this data structure to
implement some map operation much more efficiently than using, e.g. a
list.

You are given an interface and the start of a class that implements
the interface.  Your solution must represent a @tt{MapComp} using the
data representation given to you.  You may add any methods you need,
but again, do not change or remove anything from the interface.

Note that the @tt{map} and @tt{mapMono} methods will be discussed on
Monday, so you may want to hold off on attempting them.

@section[#:style 'unnumbered #:tag "assign6:submit"]{Submission}

Use @tt{submit.cs.umd.edu} to submit your solution to the problems.
You should create a zip file called @tt{Assign6.zip} that contains the
IntelliJ project containing your solutions.
