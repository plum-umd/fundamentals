#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")

@title[#:style '(unnumbered hidden toc-hidden) #:tag "lab5"]{Lab 5: More and More Design}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/beginner.html"]{Beginning Student
Language}. Require the HtDP2e
@link["https://docs.racket-lang.org/teachpack/2htdpimage.html"]{image} and
@link["https://docs.racket-lang.org/teachpack/2htdpuniverse.html"]{universe}
libraries at the top of your definitions: @racketblock[(require 2htdp/image)
(require 2htdp/universe)]

Make sure you follow
@link["https://cs.umd.edu/class/fall2017/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab5:data"]{Data and Structure Defintions}

Programs operate on data. For anything but the simplest data, built-in types of
values are insufficient. @bold{Data definitions} tell us how to create a certain
type of value, while @bold{templates} tell us how to tear apart that type of
value.

Recall the data definition and template for @tt{Posn}s:

@#reader scribble/comment-reader (racketblock
;; A @tt{Posn} is a (make-posn x y), where
;; x is a Number and
;; y is a Number.

;; posn-template : @tt{Posn} -> ???
(define (posn-template p)
  ... (posn-x p) ... (posn-y p) ...)
;; where (posn-x (make-posn x y)) == x
;;   and (posn-y (make-posn x y)) == y
)

A @tt{Posn} is a built-in data structure with the fields @tt{x} and @tt{y}. Note
that the function @racket[make-posn] accepts two values of any type. Per the
above data definition, when we refer to @tt{Posn} in function signatures or
purpose statements, the @tt{x} and @tt{y} must be Numbers. These constraints are
not enforced by the BSL; the data defintion is a contract that we programmers
agree to obey while designing functions that make or use @tt{Posn}s.

The BSL provides the functions @racket{make-posn}, @racket{posn-x},
@racket{posn-y}, and @racket{posn?}. If these were not predefined, we could
define our own Posn2 with @racket[define-struct]:

@#reader scribble/comment-reader (racketblock
(define-struct posn2 (x y))
;; A 2dPosn is a (make-posn2 Number Number).

;; posn2-template : 2dPosn -> ???
(define (posn2-template p)
  ... (posn2-x p) ... (posn2-y p) ... )

> 
)

But what if we want to manipulate @tt{3dPosn}s? We can create our
own composite data structures with @racket[define-struct].

@#reader scribble/comment-reader (racketblock
(define-struct posn3 (x y z))
;; A 3dPosn is a (make-posn3 Number Number Number).

;; posn3-template : 3dPosn -> ???
(define (posn3-template p)
  ... (posn3-x p) ... (posn3-y p) ... (posn3-z p))
)

@larger{@bold{Ex 1}}: Read the BSL documentation for @racket[define-struct]. In
a comment, write down the names and signatures of all functions defined by the
expression @racket[(define-struct posn3 (x y z))].

@colorize["red"]{Hint}: Most, but not all of the functions are shown in the
above data definition and template for @tt{3dPosn}s.

@larger{@bold{Ex 2}}: In general, for any structure definition with @tt{N}
fields, how many functions are defined by @racket[define-struct]? Write your
answer in a comment.

@larger{@bold{Ex 3}}: Here are some data definitions:
@#reader scribble/comment-reader (racketblock
(define-struct item (tag price))
;; An Item is (make-item String PositiveNumber)
 
(define-struct TA (name field pay-rate))
;; An TA is (make-TA String Field PositiveNumber)
;; A Field is one of:
;; - "biology"
;; - "english"
;; - "computer science"
;; - "business"
)

Create at least two examples of values of each data definition.

@larger{@bold{Ex 4}}: Construct a template for functions that process
@tt{Item}s.

@larger{@bold{Ex 5}}: Construct a template for functions that process @tt{TA}s.


@section[#:style 'unnumbered #:tag "lab5:funs"]{Templates to Functions}

Swap @bold{Head} and @bold{Hands}!

@larger{@bold{Ex 6}}: Design a function @tt{pay-raise}. It consumes two pieces
of data: a @tt{TA} and a number. The result is a new @tt{TA} whose @tt{pay-rate}
is multiplied by the given number.

@larger{@bold{Ex 7}}: Design the function @tt{bonus}. It consumes one piece of
data: a @tt{TA}. The result is a number which is twice the pay rate if the
@tt{TA} is in English or Biology, three times the pay rate if the @tt{TA} is in
Business, and four times the pay rate if the @tt{TA} is in computer science.


@section[#:style 'unnumbered #:tag "lab5:shapes"]{Functions on Shapes}

@larger{@bold{Ex 8}}: A Shape may be either a box (a square), a pointy shape (a
triangle), or a round shape (a circle). Design the data definitions @tt{box},
@tt{pointy}, @tt{round} for Shapes, using structure definitions to represent
squares, triangles, and circles. Each of these needs information about how big
it is, what color it is, and what position it is on the screen. Make data
examples for each kind of shape.

@larger{@bold{Ex 9}}: Design a function that takes a @tt{Shape} as input and
produces a picture where the shape has been drawn on a white background. Use
your data examples in the examples when designing this function.

@larger{@bold{Ex 10}}: Design a function that, given an integer width, draws a
@tt{Shape} of that width on an empty scene. What will happen when you
@racket[animate] that function?

