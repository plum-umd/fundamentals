#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner 2htdp/universe) "helper.rkt")
@(require "../utils.rkt")

@lab-title[20]{Resizing Hash tables}

@section[#:style 'unnumbered #:tag "lab20:intro"]{Intro}

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


@section[#:style 'unnumbered #:tag "lab20:recall"]{Recall}

We saw in lecture how to build a hash table implementation that
correctly deals with hash collisions.  Toward the end, we talked about
resizing hash tables to maintain a bound on the number of hash
collisions in a table.

Starting from the code from lecture, do the following.

@section[#:style 'unnumbered #:tag "lab20:bounds"]{Do we have a bound?}

Consider the code we wrote in class.  Is it true that the code as
written maintains an invariant that the size of every @tt{ListTable}
is less than the @tt{bound}?

If true, why is it true?  (Make an argument to convince your partner.)

If false, construct a counter-example: a program that will end up with more than
@tt{bound} entries in a @tt{ListTable}.

@section[#:style 'unnumbered #:tag "lab20:put"]{How long does a put take?}

How long does the @tt{put} method take when there is no need for a @tt{resize}?
How long does it take when there is a @tt{reset}?

Is @tt{put} guaranteed to terminate, i.e. can you write a program such
that doing a @tt{put} runs forever?  If yes, make an argument for why.
If no, make a program that runs forever when doing a @tt{put}.

@section[#:style 'unnumbered #:tag "lab20:timing"]{Evaluating performance}

Here's a crude way to measure the amount of time it takes to run a method:
@verbatim|{
long startTime = System.nanoTime();
methodToTime();
long endTime = System.nanoTime();

long duration = (endTime - startTime);  //divide by 1000000 to get milliseconds.
}|

Design a method that takes a table and inserts a given number of
randomly generated key value pairs.  (You can use
@tt{Random.nextInt()} to get a random integer.)

Use this method and the above approach to timing to compare the
performance of inserting and looking up elements in both
@tt{ListTable}s and @tt{HashTable}s with 100, 1000, 10000, and 1000000
elements.


@section[#:style 'unnumbered #:tag "lab20:submit"]{Submission}

Submit a zip file of your work at the end of lab.
