#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner 2htdp/universe) "helper.rkt")
@(require "../utils.rkt")

@lab-title[21]{Java Collections: Hashtable}

@section[#:style 'unnumbered #:tag "lab21:intro"]{Intro}

You'll work in this lab with ad-hoc partners.

The two of you will work as a team to solve problems. At any time, one
of you will be the @bold{Head} and the other will be the
@bold{Hands}. The @bold{Head} does the thinking and the @bold{Hands}
does the typing. @bold{Hands} type only what the @bold{Head} tells
them to, but you're free to discuss any issues that pop up. You should
switch off during the lab to make sure each of you get practice
problem solving, dealing with syntax, and getting finger exercises on
the keyboard.

You should start this lab with @link["Lec2.zip"]{this project
skeleton}.

@section[#:style 'unnumbered #:tag "lab21:recall"]{Recall}

We've now seen how to build a real hash table data structure in class.
During the last lab, you were asked to evaluate the performance of your
hash table and your list implementation of the @tt{Table} interface.

In this lab, you'll compare your implementation against Java's own
@tt{Hashtable} class.

@section[#:style 'unnumbered #:tag "lab21:read"]{Read up on Hashtables}

To start, read the documentation for Java's
@link["https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Hashtable.html"]{@tt{Hashtable}
class}.  Make some examples and try it out.

@section[#:style 'unnumbered #:tag "lab21:wrapper"]{Wrapping a Hashtable as a Table}

Implement a "wrapper" class that implements your @tt{Table} interface.
That is: implement a class called @tt{WrapHashtable} that implements
@tt{Table}.  The class should have a single @tt{Hashtable} field and
all of the @tt{Table} methods should be implemented by using the
appropriate @tt{Hashtable} methods on this field.

Be sure to thoroughly test your @tt{WrapHashtable} class.

@section[#:style 'unnumbered #:tag "lab21:eval"]{Evaluation}

Using the code you wrote in Wednesday's lab, evaluate the performance
of this new table implementation.  How does it compare to your own
hand-rolled hash table?


@section[#:style 'unnumbered #:tag "lab21:submit"]{Submission}

Submit a zip file of your work at the end of lab.
