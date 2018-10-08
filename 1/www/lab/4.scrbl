#lang scribble/manual
@(require scribble/core "helper.rkt")

@title[#:style 'unnumbered #:tag "lab4"]{Lab 4: Design and Composites}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/beginner.html"]{Beginning Student
Language}. Require the HtDP2e
@link["https://docs.racket-lang.org/teachpack/2htdpimage.html"]{image} and
@link["https://docs.racket-lang.org/teachpack/2htdpuniverse.html"]{universe}
libraries at the top of your definitions: @racketblock[(require 2htdp/image)
(require 2htdp/universe)]

Make sure you follow
@link["https://cs.umd.edu/class/fall2018/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab4:design"]{How to Design a Function}

In your previous labs we've asked you to @bold{define} various functions. From
now on, we're going to ask you to @bold{design} a function. You must follow the
@link["https://htdp.org/2018-01-06/Book/part_preface.html"]{design
recipe} to @bold{design} a function.

Before you begin any implementation, you should have written down any relevant
data definition, a function signature, header, and purpose statement, as well as
input/output examples covering both common and edge cases.

@#reader scribble/comment-reader (racketblock
;; what-temp : Number -> String                                  <- Signature
;; Given a temperature (in degrees Fahrenheit), return a string  <- Purpose 
;; string description of the comfort level of that temperature.     Statement
(define (what-temp df) "")                    ; <- Stub
(check-expect (what-temp 32)       "cold")    ; <- Examples/Tests
(check-expect (what-temp 45.0)     "comfy")
(check-expect (what-temp 68)       "comfy")
(check-expect (what-temp 74.99999) "comfy")
(check-expect (what-temp 75)       "hot")
(check-expect (what-temp 100)      "hot")
)

@colorize["red"]{@bold{Note}}: From now on, the first response you get from any
TA will be "What step of the design recipe are you on?"

@larger{@bold{Ex 1}}: Design a function @tt{simulated-ta} that, given any
string, returns the proper TA response: "What step of the design recipe are you
on?" Before asking any questions in this or future labs, first ask your
@tt{simulated-ta}.


@section[#:style 'unnumbered #:tag "lab4:posn"]{Positions}

Programs manipulate data. We describe how to create data with @bold{data
defintions} and describe how to tear apart data for use in functions with
@bold{templates}.

Consider @tt{Posn}s--a composite data structure provided by BSL:

@#reader scribble/comment-reader (racketblock
;; A Posn is a (make-posn x y), where
;; x is a Number and
;; y is a Number.
)

A @tt{Posn} represents a two-dimensional position. The @bold{data definition}
describes how to make a @tt{Posn}: give the function @tt{make-posn} two numbers,
such as @tt{(make-posn x y)}. We can pass that value @tt{(make-posn x y)} to any
function that expects a @tt{Posn} as its input.

This @bold{template} describes the data available in any function that expects a
@tt{Posn} as its input:

@#reader scribble/comment-reader (racketblock
;; posn-template : Posn -> ???
(define (posn-template p)
  (... (posn-x p) ... (posn-y p) ...))
;; where (posn-x (make-posn x y)) == x
;;   and (posn-y (make-posn x y)) == y.
)

While each of the following functions have different signatures, purpose
statements, stubs, and examples/tests (function-specific parts of the design
recipe), the @tt{Posn} data definition and template remain the same
(data-specific parts of the design recipe).

@larger{@bold{Ex 2}}: Design a function @tt{posn-distance} that given two posns,
returns the scalar distance between the two posns.

@larger{@bold{Ex 3}}: Design a function @tt{place-circle} that consumes a posn
and produces a 300×300 scene with a red circle of radius 10 at the position
represented by the posn. For instance, if given @tt{(make-posn 10 290)} it
should produce a scene with a red circle in the lower left corner.

Swap @bold{Head} and @bold{Hands}.

@larger{@bold{Ex 4}}: Design a function @tt{in-circle?} that consumes two posns
and a positive integer @tt{a}. The first posn may be any point, while the second
represents the center of a circle with the area @tt{a}. The function
@tt{in-circle?} should return @tt{#true} only if the first point is inside the
circle, and @tt{#false} otherwise.

@colorize["red"]{@bold{Hint}}: You may want to define a helper-function
@tt{area->radius} in your solution to exercise 4.

@larger{@bold{Ex 5}}: Design a function @tt{in-rectangle?} that consumes two
posns and two positive integers @tt{w}, @tt{h}. The first posn may be any point,
while the second represents the center of a rectangle with the width @tt{w} and
height @tt{h}. The function @tt{in-rectangle?} should return @tt{#true} only if
the first point is inside the rectangle, and @tt{#false} otherwise.
