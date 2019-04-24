#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner 2htdp/universe) "helper.rkt")
@(require "../utils.rkt")

@lab-title[19]{Fixing Hash tables}

@section[#:style 'unnumbered #:tag "lab19:intro"]{Intro}

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


@section[#:style 'unnumbered #:tag "lab19:recall"]{Recall}

We saw in lecture that a hash table built out of an array list of optional key value pairs
has a bug when two entries have keys with the same @tt{hashCode()} modulo table size.
This is what's called a @emph{hash collision}.


There's a failing test case in the skeleton that demonstrates the bug.


@section[#:style 'unnumbered #:tag "lab19:buckets"]{Fixing collisions}

The idea for how to fix the problem is to change the representation to
use a @emph{list} of key value pairs for each entry in the array list.
Each of these elements has in common that their @tt{hashCode()} modulo
table size are the same.

When looking something up, just like before, you must compute the
appropriate index, but now you must scan through the list looking for
a key that is equal.

When put something in, just like before, you must compute the
appropriate index, but now you must scan through the list looking for
a key that is equal.  If you find one, you should update the value.
If you don't, you should grow the list with the new key value pair.

Revise the design of @tt{HashTable} to solve the collision problem.

@section[#:style 'unnumbered #:tag "lab19:submit"]{Submission}

Submit a zip file of your work at the end of lab.
