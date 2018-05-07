#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner 2htdp/universe) "helper.rkt")
@(require "../utils.rkt")

@lab-title[23]{Iterating Again & Again & Again ...}

@section[#:style 'unnumbered #:tag "lab23:intro"]{Intro}

You'll work in this lab with your
@link["https://piazza.com/class/jcspfhewmdn41y?cid=108"]{lab partners}.

The two of you will work as a team to solve problems. At any time, one
of you will be the @bold{Head} and the other will be the
@bold{Hands}. The @bold{Head} does the thinking and the @bold{Hands}
does the typing. @bold{Hands} type only what the @bold{Head} tells
them to, but you're free to discuss any issues that pop up. You should
switch off during the lab to make sure each of you get practice
problem solving, dealing with syntax, and getting finger exercises on
the keyboard.

You should start this lab with @link["Lab23.zip"]{this project
skeleton}.


@section[#:style 'unnumbered #:tag "lab23:recall"]{Recall}

We've seen many ways of iterating through data in this course. The
fundamental list function @tt{foldr} allows us to transform lists into
any type of data, element by element using recursion. Iterators make
it easy to check if a list is empty and mutably grab the next element,
making them amenable to work with in @tt{for-each}- and
@tt{while}-loops. The standard @tt{for}-loop, which we've used little
during the past semester, looks like this:

@verbatim|{
for (@emph{init} ; @emph{test} ; @emph{next}) {
  @emph{body ...}
}
}|

and is roughly equivalent to the following while loop:

@verbatim|{
@emph{init};
while (@emph{test}) {
  @emph{body ...}
  @emph{next}
}
}|

It can be used to print the first 100 natural numbers like so:

@verbatim|{
for (int i=0 ; i<100 ; i=i+1) {
  System.out.println(i);
}
}|

These iterating operators are all slightly different, but should be
able to be used to perform the same tasks. We'll test that out today!


@section[#:style 'unnumbered #:tag "lab23:sum"]{Summing a list of numbers}

Summing numbers is one of the simplest tasks we've performed this
semester, so it'll be a nice warm-up for us.

@bold{Ex 1a}: Use @tt{Listof.foldr} to calculate the sum of a list of
numbers.

@bold{Ex 1b}: Use a @tt{for-each} loop to calculate the sum of a list of
numbers.

@bold{Ex 1c}: Use a @tt{for} loop to calculate the sum of a list of
numbers. @bold{Hint}: You may want to use an integer counter as an
index via @tt{Listof.nth}.

@bold{Ex 1d}: Use a @tt{while} loop to calculate the sum of a list of
numbers.


@section[#:style 'unnumbered #:tag "lab23:exists"]{Does such a string exist?}

We also can check for the existence of a string satisfying some
predicate inside a list of strings. The @tt{exists} method already
does this, but we can do this with iterating constructs as well!

We've provided two simple predicates in the test file. Use those to
test for the existence of strings inside the list @tt{labc}.

@bold{Ex 2a}: Use @tt{Listof.foldr} to test for the existence of
strings satisfying the given predicates.

@bold{Ex 2b}: Use a @tt{for-each} loop to test for the existence of
strings satisfying the given predicates.

@bold{Ex 2c}: Use a @tt{for} loop to test for the existence of strings
satisfying the given predicates.

@bold{Ex 2d}: Use a @tt{while} loop to test for the existence of strings
satisfying the given predicates.


@section[#:style 'unnumbered #:tag "lab23:rev"]{Reversing a List}

We can reverse a list with the method @tt{reverse}, or we can use
loops and temporary variables.

@bold{Ex 3a}: Reverse the example list using a @tt{for-each} loop.

@bold{Ex 3b}: Reverse the example list using a @tt{for} loop.

@bold{Ex 3c}: Reverse the example list using a @tt{while} loop.

@bold{Ex 3d}: It's more difficult (in a number of ways) to reverse a
list using @tt{foldr} than it is using the looping constructs. Why is
that? Is there another list abstraction that would work better?


@section[#:style 'unnumbered #:tag "lab23:avg"]{Average of a List}

Calculating an average using loops is a bit tricker, since we need to
update the average for each number as we go.

@bold{Ex 4a}: Use @tt{Listof.foldr} to calculate the average of a list
of numbers.

@bold{Ex 4b}: Use a @tt{for-each} loop to calculate the moving average
of a list of numbers.

@bold{Ex 4c}: Use a @tt{for} loop to calculate the moving average
of a list of numbers.

@bold{Ex 4d}: Use a @tt{while} loop to calculate the moving average
of a list of numbers.


@section[#:style 'unnumbered #:tag "lab23:fizzbuzz"]{Fizz Buzz}

Fizz Buzz is a classic programming problem: the goal is to create a
list of numbers from 1 to 100, where each number divisible by 3 is
replaced with "fizz" and each number divisible by 5 is replaced with
"buzz".

@bold{Ex 5a}: Use @tt{Listof.foldr} to create the fizz buzz list from
1 to 100.

@bold{Ex 5b}: Use a @tt{for-each} loop to create the fizz buzz list
from 1 to 100.

@bold{Ex 5c}: Use a @tt{for} loop to create the fizz buzz list from 1
to 100.

@bold{Ex 5d}: Use a @tt{while} loop to create the fizz buzz list from
1 to 100.
