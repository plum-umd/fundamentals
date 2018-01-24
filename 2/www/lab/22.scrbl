#lang scribble/manual
@(require scribble/core (for-label lang/htdp-intermediate) "helper.rkt")

@title[#:style 'unnumbered #:tag "lab22"]{Lab 22: Generating Numbers}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/intermediate-lam.html"]{Intermediate
Student Language with Lambda}.

Make sure you follow
@link["https://cs.umd.edu/class/fall2017/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab22:gen"]{Special Numbers}

Each number @emph{Fib@subscript{n}} in the Fibonacci sequence is the sum of the
two previous numbers @emph{Fib@subscript{n-1}} and
@emph{Fib@subscript{n-2}}. The first two Fibonacci numbers
@emph{Fib@subscript{0}} and @emph{Fib@subscript{1}} are 0 and 1.

@bold{Ex 1}: Design the function @tt{fib} that, given a number @tt{n}, returns
@emph{Fib@subscript{n}}.

@bold{Ex 2}: Design a function @tt{fibs} that given a natural @tt{n}, returns
the list of all Fibonacci numbers less than or equal to @tt{n}.


A @emph{Natural} @tt{n} is prime if it has no divisors other than 1 and itself.

@bold{Ex 3}: Design a function @tt{prime?} that returns @racket[#true] only if
the given natural number @tt{n} is prime. To do so, build a list of all naturals
greater than 1 and less than @tt{n} and test each to see if any numbers in the
list is a factor of @tt{n}.

@bold{Ex 4}: Re-implement your @tt{prime?} function to take advantage of the
fact that only trial factors less than @racket[(sqrt n)] need be considered.

@bold{Ex 5}: Design a function @tt{primes} that given a natural @tt{n}, returns
the list of all prime numbers less than or equal to @tt{n}.


@section[#:style 'unnumbered #:tag "lab22:term"]{Out, Out, Brief Candle}

Below is a description of a simple function @tt{a} implemented with addition and
subtraction. It is not implemented with the standard structural recursion over
the naturals.

@verbatim|{
a(m, n) = n+1,                when m=0
        = a(m-1, 1),          when m>0 and n=0
        = a(m-1, a(m, n-1)),  when m>0 and n>0

a(0, 0) = 1,    a(1, 0) = 2,
a(2, 2) = 7,    a(3, 2) = 29
a(4, 0) = 13,   a(4, 1) = 65533
}|

@bold{Ex 6}: Design the function @tt{a}. Keep your tests small: the result of
@racket[(a 4 2)] is a number with 19,729 digits and will take a bit too long to
calculate during this lab.

@bold{Ex 7}: Does the function @tt{a} terminate on all possible inputs? Discuss
this with your partner and come up with an argument in one direction or the
other.

@colorize["red"]{@bold{Hint}}: Structural recursion always terminates because
we're always making progress toward the base case. With generative recursion,
progress can be a bit more obscure. Does the function @tt{a} always make
progress toward some base case when called recursively?


@section[#:style 'unnumbered #:tag "lab22:early"]{If you finish early...}

Try to improve the speed of @tt{prime?} by implementing a faster
@link["https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes"]{prime number test}.
