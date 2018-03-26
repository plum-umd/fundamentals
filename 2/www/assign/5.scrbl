#lang scribble/manual
@(require scribble/core)
@(require "../utils.rkt")
@(define (colorize c . content)
  (elem #:style (style #f (list (color-property c)))
        content))

@assn-title[5]{Maps, Sets, MultiSets}

This is assignment is to be completed and submitted with your new
@link["https://piazza.com/class/jcspfhewmdn41y?cid=108"]{partner}.  You
may not work with anyone other than your assigned partner.

@bold{Due}: Tuesday, April 3, 11:59:59 PM EST.

@(define @Piazza @link["http://piazza.com/umd/spring2018/cmsc132a"]{Piazza})

@section[#:style 'unnumbered #:tag "assign5:download"]{Download the assignment project}

For this assignment, download the following zip file:
@link["Assign5.zip"]{Assign5.zip}.  Save the file somewhere on your
computer and unzip it.  This will create a directory called
@tt{Assign5} with an IntelliJ project inside.  It contains all the
libraries you will need and some code to get you started.

Open IntelliJ and select "Open".  Navigate to the @tt{Assign5}
directory and select it.

This should open up the project and place you inside the
@tt{Assign5.java} file.  The first thing you should do is edit the
authors line (which should be selected when you open the project) to
be your and your partner's directory ID.

The project has a "Test" configuration in the top right corner.  Press
the "Run" (green triangle) button to run the test suite.  The test
suite comes with a few tests to get you started, but running the tests
will result in an null pointer error because it is missing
implementations.

@section{Implementing a Library of Data structures}

For this assignment, you must implement a library of data structures
for maps, sets, and multisets.

In the code given to you, you will find interfaces for all of the data
structures (plus lists, which are already implemented).  You must
implement classes that correctly implement all of these operations and
write a test suite to validate your design.

Maps represent finite functions between keys and an associated value
(similar to dictionaries on the midterm).  The idea of a map is that
keys are unique, values may or may not be.  Two maps are the same if
they map the same keys to the same values.  If you add a new key-value
to a map that already has an association for that key, the old one
effectively is lost.  The order of key-values in a map is unspecified.

Sets represent collections of unique elements.  Two sets are the same
if they contain the same elements.  Adding an element to a set that
already contains that element has no observable effect; it results in
the same set as you started with.  The order of elements in a set in
unspecified.

Multisets represent collections of elements, but without the
restriction that the elements are unique.  Adding an element to a
multiset that already contains that element result is an a multiset
containing one more instance of that element.  Two multi-sets are the
same when the have the same elements, i.e. the same elements including
the same number of duplicates of each unique element).  The order of
elements in a multi-set is unspecified.

You may add signatures for methods you need to implement the library,
but YOU MUST NOT CHANGE ANY OF THE GIVEN SIGNATURES.  Your code should
be well-designed and abstracted to avoid code duplication.

@section[#:style 'unnumbered #:tag "assign5:submit"]{Submission}

Use @tt{submit.cs.umd.edu} to submit your solution to the problems.
You should create a zip file called @tt{Assign5.zip} that contains the
IntelliJ project containing your solutions.
