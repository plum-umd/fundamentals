#lang scribble/manual
@(require "../utils.rkt")

@title[#:tag "assign01"]{5/7: Admin and lists}

Due: 5/7, midnight by email to @tt{dvanhorn}.

Language: any.

@section{Functions on lists of numbers}

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
]

You may use any programming language you'd like, but your programs
must be well-designed according to the design recipe.
