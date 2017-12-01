#lang scribble/manual
@(require "unnumbered.rkt")

@title*[#:tag "style"]{The Style}

In addition to following the design recipe, all code written for this class
should adhere to the following basic style guidelines for your programs:

@itemlist[#:style 'ordered

@item{No line should span more than 80 characters. See bottom right of
DrRacket. Break lines at those points suggested by HtDP and this web page.}

@item{No function should span more than five to eight lines for
now. If it does, reconsider your interpretation of the "one task, one
function" guideline.}

@item{Use names that make sense with respect to the problem.}

@item{Start the file with globally useful data definitions and
constant definitions. Then arrange to place the most important
function near the top of the file and the less important ones near the
bottom. NOTE: You don't have to develop the functions in this order,
you just have to arrange the program this way.}

@item{Separate distinct sections of your program with dashed lines
that are exactly 80 columns wide.}

@item{Programs use the parentheses style displayed in HtDP and this
web page: 

@bold{Good}:

@verbatim{
(define (f l)
  (cond [(empty? l) 0]
        [else (add1 (f (rest l)))]))
}

@bold{Bad}:

@verbatim{
(define (f l)
  (cond [(empty? 1) 0]
        [else (add1 (f (rest l)))]
   )
  )
}

These dangling parentheses in the second code excerpt are considered
@bold{extremely bad style}. You will lose @bold{all} style points for
using it even once.  }

@item{Do not embed images in the primary source files of your
projects.  Doing so defeats the benefits of using revision control
software such as @tt{svn} since it requires the entire file to be
encoded in a non-human readable format.  Instead, if you need to embed
images, create a file @tt{image-constants.rkt} which embeds all the
images you need and gives them names.  At the top of this file,
write @racket[(provide (all-defined-out))].  There should be no
other code in the file other than the image definitions.  Then in the
files that need to @emph{use} the images, add the following to the
top: @racket[(require "image-constants.rkt")].  You should then be
able to use any of the names defined in @tt{image-constants.rkt}.}

@item{Place short and informative test cases between the method
signature and implementation.  (A single test written between the
signature and code should not be longer than 3 lines.) Place longer
and more complicated test cases at the bottom of your program.  The
test suite should be organized around which classes and behaviors that
are being tested and there should be comments that make it easy to
discern the organization of the test suite.}]

Not observing these very basic guidelines leads to unreadable code and
to loss of points.

