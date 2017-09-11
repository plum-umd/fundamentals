#lang scribble/manual
@(require scribble/core)
@(define (colorize c . content)
  (elem #:style (style #f (list (color-property c)))
        content))

@title[#:style '(unnumbered hidden toc-hidden) #:tag "lab4"]{Lab 4: Design and Composites}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/beginner.html"]{Beginning Student
Language}. Require the HtDP2e
@link["https://docs.racket-lang.org/teachpack/2htdpimage.html"]{image} and
@link["https://docs.racket-lang.org/teachpack/2htdpuniverse.html"]{universe}
libraries at the top of your definitions: @racketblock[(require 2htdp/image)
(require 2htdp/universe)]

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab4:design"]{How to Design a Function}

In your previous labs we've asked you to @bold{define} various functions. From
now on, we're going to ask you to @bold{design} a function. You must follow the
@link["http://www.ccs.neu.edu/home/matthias/HtDP2e/part_preface.html"]{design
recipe} to @bold{design} a function. Before you begin any implementation, you
should have written down any relevant data definition, a function signature,
header, and purpose statement, as well as input/output examples covering both
common and edge cases.

@#reader scribble/comment-reader (racketblock
;; what-temp : Number -> String
;; Given a temperature (in degrees Fahrenheit) return a string description
;; of the comfort level of that temperature.
(define (what-temp df) "")
;; (what-temp 32)       => "cold"
;; (what-temp 45.0)     => "comfy"
;; (what-temp 68)       => "comfy"
;; (what-temp 74.99999) => "comfy"
;; (what-temp 75)       => "hot"
;; (what-temp 100)      => "hot"
)

@colorize["red"]{@bold{Note}}: From now on, the first response you get from any
TA will be "What step of the design recipe are you on?"

@larger{@bold{Ex 1}}: Design a function @tt{simulated-ta} that, given any
string, returns the proper TA response: "What step of the design recipe are you
on?" Before asking any questions in this or future labs, first ask your
@tt{simulated-ta}.


@section[#:style 'unnumbered #:tag "lab4:posn"]{Positions}

Posns are a kind of data provided by BSL that represent a position on a
plane. As you would expect, a position on a plane is represented using two
numbers, one for the value of the x axis, and the other for the y axis.

The @tt{make-posn} function creates a posn. Use the documentation to find out
how to use it. Once you have a posn, you will likely want to know what its x and
y values are--to pass them to another function, for example. To do so, use the
@tt{posn-x} and @tt{posn-y} functions, which give you the x and y values of the
posn, respectively.

A good data definition for posns looks like this:

@#reader scribble/comment-reader (racketblock
;; A Posn is a (make-posn x y), where
;; x is a Number,
;; y is a Number,
;; (posn-x (make-posn x y)) => x, and
;; (posn-y (make-posn x y)) => y
)

@larger{@bold{Ex 2}}: Design a function @tt{posn-distance} that given two posns,
returns the scalar distance between the two posns.

@larger{@bold{Ex 3}}: Design a function @tt{place-circle} that consumes a posn
and produces a 300×300 scene with a red circle of radius 10 at the position
represented by the posn. For instance, if given @tt{(make-posn 10 290)} it
should produce a scene with a red circle in the lower left corner.

Swap @bold{Head} and @bold{Hands}.

@larger{@bold{Ex 4}}: Design a function @tt{in-circle?} that consumes two posns
and a positive integer @tt{r}. The first posn may be any point, while the second
represents the center of a circle with the radius @tt{r}. The function
@tt{in-circle?} should return @tt{#true} only if the second point is inside the
circle, and @tt{#false} otherwise.

@colorize["red"]{@bold{Note}}: You may want to use the constant
@link["https://docs.racket-lang.org/htdp-langs/beginner.html#(def._htdp-beginner._((lib._lang%2Fhtdp-beginner..rkt)._pi))"]{@tt{pi}}
in exercise 4.

@larger{@bold{Ex 5}}: Design a function @tt{in-rectangle?} that consumes two
posns and two positive integers @tt{w}, @tt{h}. The first posn may be any point,
while the second represents the center of a rectangle with the width @tt{w} and
height @tt{h}. The function @tt{in-rectangle?} should return @tt{#true} only if
the second point is inside the rectangle, and @tt{#false} otherwise.
