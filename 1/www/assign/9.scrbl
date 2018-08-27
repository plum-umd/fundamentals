#lang scribble/manual
@(provide readings)
@(require (for-label (except-in lang/htdp-intermediate-lambda image?) 2htdp/image 2htdp/universe))

@title[#:style 'unnumbered #:tag "assign9"]{Assignment 9: Plotting functions}

@bold{Due:} Monday, November 20, 11:59:59 PM EST.

@(define-syntax-rule (bslblock . body)
  (codeblock #:keep-lang-line? #f "#lang htdp/bsl" "\n" . body))

The following should be completed in cooperation with your latest
assigned partner, @bold{which were newly assigned on Oct 23}. (Partner
assignments are listed on
@link["https://piazza.com/class/j474gwnsd3619n?cid=294"]{Piazza}.)
You may not share code for this assignment with anyone but your
partner.

You must use the design recipe and @secref{style} guidelines to
receive full credit.

@section[#:tag "assign9:prep"]{Preparation}

@(define readings
   @elem{all of
     @link["https://htdp.org/2018-01-06/Book/part_four.html"]{Part IV}
     and up through Chapter 27 of
     @link["https://htdp.org/2018-01-06/Book/part_five.html"]{Part V}})

Make sure you have read @readings of HtDP2e.

@section[#:tag "assign9:graph"]{Plotting}

Edit a file named @tt{graph.rkt} for this part of the
assignment. Add your information to the standard file header at the
top of the file.

In this assignment, you will develop a program which allows the user to
interact with a visual representation of a given
function. See @link["https://en.wikipedia.org/wiki/Graph_of_a_function"]{Graph of a function}.

In this assignment, the word @emph{graph} will refer to a list of
posns obtained from a given function @racket[f]. The @racket[x] field
of each posn will store an @emph{x}-value. The @racket[y] field of
each posn will store the value of @racket[f] evaluated at the number
stored in the @racket[x] field. By drawing or
plotting, we mean producing an image from the graph of a
function.

The data contained in a graph will be interpreted as points in a
@link["https://en.wikipedia.org/wiki/Cartesian_coordinate_system"]{Cartesian
coordinate system}. A big part of this assignment will be to
relate lists of posns interpreted in a Cartesian coordinate system to
lists of posns interpreted in the coordinate system of a DrRacket
image.
In this way, we will not just show a fixed image, but
let the user move and zoom the view of
the function.

@bold{Exercise 1} Define constants @racket[WIDTH] and @racket[HEIGHT]
which will be used to determine the width and height of the scene.
Using @racket[build-list], define @racket[my-points] to be a
long list of posns of your making.

@bold{Exercise 2} Design a function @racket[draw-lop] which takes a
list of posns and returns an image. The purpose of @racket[draw-lop]
is to represent each posn as a dot of some shape and color on a
scene. Use @racket[foldr]. Apply @racket[draw-lop] to
@racket[my-points] and store the result in a constant
@racket[my-plot].

@bold{Exercise 3} Design a function @racket[center-origin] which takes
a list of posns and returns a list of posns. The input list of posns
is interpreted with respect to an origin in the middle of your
scene. Furthermore, the positive @emph{y}-direction is interpreted to
point upward. The purpose of @racket[center-origin] is to produce the
corresponding list of posns in the coordinate system of a DrRacket
image. For example, if the scene is 400 by 400, then the input
@bslblock{
(list (make-posn 0 0) (make-posn -50 200))
}
should yield to the output
@bslblock{
(list (make-posn 200 200) (make-posn 150 0))
}
For @racket[center-origin], use @racket[map].

Apply @racket[draw-lop] to the result of applying
@racket[center-origin] to @racket[my-points] and store the final image
as @racket[my-centered-plot].

@bold{Exercise 4} Define a struct named @racket[plane] whose four
fields are @racket[xscale], @racket[yscale], @racket[x0] and
@racket[y0]. For the @racket[big-bang] programs in this assignment, a world will
be a @racket[(make-plane Number Number Number Number)].

@bold{Exercise 5} Design a function @racket[image-coords] which takes
a @racket[plane] and a list of posns and returns a list of
posns. The new function @racket[image-coords] is a more general version of
@racket[center-origin].  Like @racket[center-origin], the new function
@racket[image-coords] should produce the corresponding list of posns in the
coordinate system of a DrRacket image.  Also like @racket[center-origin],
the new function @racket[image-coords] should interpret the positive
@emph{y}-direction in its input to point upward.  But instead
of interpreting the origin of the input list of posns to be the center
of scene, interpret it to be given by the fields @racket[x0] and
@racket[y0] of the input @racket[plane].
So the fields @racket[x0] and @racket[y0] themselves are
in the coordinate system of a DrRacket image.  Finally,
interpret @racket[xscale] and @racket[yscale] to be the number of
pixels in an input @emph{x}-unit and @emph{y}-unit. For example, if
@racket[xscale] is 100, then an input posn @racket[(make-posn -2 0)]
is 200 pixels to the left of the origin. If @racket[yscale] is 5, then
an input posn @racket[(make-posn 0 5)] is 25 pixels directly above the
origin. Again, use @racket[map].
Also, feel free to use @racket[center-origin].

@bold{Exercise 6} Design a function @racket[navigate] to be used as a
@racket[big-bang] key-event handler. Thus, @racket[navigate] takes a
@racket[plane] and a key event and returns a
@racket[plane]. @racket[navigate] enables 9 distinct keyboard-driven
behaviors (aside from nothing at all). The arrow keys will allow the
user to move in four different directions in the scene: they will
change values stored in the @racket[x0] and @racket[y0] fields of the
input @racket[plane] according to a globally defined
@racket[move-speed].  The keys w, a, s and d will allow the user to
zoom in four ways: the w key will zoom in just with respect to the
@emph{y}-axis and the s key will do the opposite; the d key will zoom
in just with respect to the @emph{x}-axis and the a key will do the
opposite. The w, a, s and d keys should then (at least) update the
@racket[xscale] and @racket[yscale] fields by a globally defined
@racket[zoom-factor]. Finally, the r key will reset the world
to the globally defined @racket[init-plane].

@bold{Exercise 7} Define a @racket[big-bang] program using the functions
defined in Exercises 2, 5 and 6 that allows you to navigate a plot of
@racket[my-points]. Define a constant @racket[my-interactive-plot]
that stores the result of this @racket[big-bang] program.

@bold{Exercise 8} Design a function @racket[xticks] which takes three
numbers @racket[xscale], @racket[x0] and @racket[n] and produces an
increasing list of @racket[n] numbers starting at @racket[(- 0 (/ x0
xscale))]. Each number is @racket[(/ 1 xscale)] larger than its
predessor. The output list will be interpreted as @emph{x}-values in
the Cartesian coordinate system.

If @racket[n] is taken to be @racket[(add1 WIDTH)], the output numbers
will range across the visible portion of the plot. For example, if
@racket[WIDTH] is set to 400, then
@bslblock{
(xticks 100 200 (add1 WIDTH))
}
will produce a list of 401 evenly distributed numbers
starting with -2 and ending with 2:
@codeblock{
(list -2 -1.99 -1.98 ... 1.98 1.99 2)
}

You may use @racket[build-list] or not.

@bold{Exercise 9} Design a function @racket[graph] which takes a
function @racket[Number -> Number] and a list of numbers and produces
a list of posns. The output posns contain a number from the input list
paired with the value of the input function applied to that
number. For example,
@bslblock{(graph sqr (list 0 2))}
would give
@bslblock{(list (make-posn 0 0) (make-posn 2 4))}

@bold{Exercise 10} Design a function @racket[draw-graph] which takes a
@racket[plane] and a function @racket[Number -> Number] and produces
its plot. @racket[draw-graph] should be the composite of
@racket[draw-lop], @racket[image-coords], @racket[graph] and
@racket[xticks]. Pass @racket[(add1 WIDTH)] to @racket[xticks] as the
number of ticks to generate.

@bold{Exercise 11} Design a @racket[big-bang] program with @racket[draw-graph]
and @racket[navigate] to visualize interactively the @racket[sin]
function. Store the result in a constant named
@racket[interactive-sin].

@bold{Extra fun} (Challenging) The zoom behavior we have defined in
@racket[navigate] is simple to define but not exactly what we
want. Can you see what is going wrong?  Typically when we zoom, we
expect the center point of the scene to remain fixed during the
zoom. However, in your current implementation, unless the center of
the scene is aligned with the origin, the center point of the scene
shifts during a zoom. This suggests that @racket[x0] and @racket[y0]
should @emph{also} be adjusted while zooming in @racket[navigate]. Can
you see how? Hint: Determine the image coordinates of the
center of the scene. Use these values and @racket[zoom-factor] to
update @racket[x0] and @racket[y0] so that the center point remains
fixed.
