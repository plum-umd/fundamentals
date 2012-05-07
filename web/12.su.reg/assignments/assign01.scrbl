#lang scribble/manual
@(require "../utils.rkt")

@title[#:tag "assign01"]{5/7: Functions on lists}

Due: 5/7, midnight by email to @tt{dvanhorn}.

Language: any.

You must complete this assignment individually and should not discuss
solutions with anyone but the course staff.

@section{Designing functions on lists of numbers}

Design the following programs:

@itemlist[
@item{@tt{sum} - sums a list of numbers.}

@item{@tt{sum-sqrs} - sums the squares of a list of numbers.}

@item{@tt{interleave} - from two lists of equal length, produces a
single list of interleaved elements.  For example, if given @tt{1},
@tt{2}, @tt{3} and @tt{4}, @tt{5}, @tt{6}, it produces @tt{1}, @tt{4},
@tt{2}, @tt{5}, @tt{3}, @tt{6}.}

@item{@tt{closest-to-four} - find the number closest to @tt{4} in
a given non-empty list of numbers.}

@item{@tt{arg-zero} - given a function on numbers and a non-empty list
of numbers, produce an element of the list for which the function
gives a result closest to @tt{0} when applied to that element.  In
other words, if given function @tt{f} and list @tt{x_0}, @tt{x_1},
..., @tt{x_n}, then @tt{arg-zero} should produce @tt{x_i} (for some
@tt{i} in @tt{0..n}) where @tt{f(x_i)} is as close to zero as any
@tt{f(x_j)}, for all @tt{j} in @tt{0..n}.}
]

You may use any programming language you'd like, but your programs
must be well-designed according to the design recipe.

@section{Submissions}

This is the only assignment that will be submitted via email.  Please
send an email with the following form:

@verbatim{
   Subject: 2510 Assignment 1 CCIS-username

   CCIS-username
   Full name

   Attach: source code
}

where @tt{CCIS-username} is your CCIS username.  If you do not have a
CCIS username or do not know what it is, please see the Systems group
on the third floor of WVH immediately.  Your source code should
include your full name and CCIS username as well.


