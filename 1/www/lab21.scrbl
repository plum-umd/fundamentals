#lang scribble/manual
@(require scribble/core (for-label lang/htdp-intermediate) "helper.rkt")

@title[#:style 'unnumbered #:tag "lab21"]{Lab 21: Generating Fractals}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/intermediate-lam.html"]{Intermediate
Student Language with Lambda}.

Make sure you follow
@link["https://cs.umd.edu/class/fall2017/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.

Choose the initial @bold{Head} and @bold{Hands}, and get started!

@section[#:style 'unnumbered #:tag "lab21:circles"]{Warm-up with Circles and Spirals}

Let's begin by drawing some circles. Before we start, here is a bit of setup
code:

@#reader scribble/comment-reader (racketblock
(require 2htdp/image)
(require 2htdp/universe)
 
;; Constants
(define WIDTH 300)
(define HEIGHT 160)
(define MT (empty-scene WIDTH HEIGHT))
 
;; next-size : Number -> Number
(define (next-size s)
  (/ s 2))
)

This is our first target image.

@image{lab21-circles-1.png}

The circles get smaller as they move to the right.

@bold{Ex 1}: Design a function @tt{circles} that takes two numbers @tt{x},
@tt{size}, and an image @tt{img}. If @tt{size} is less-than or equal to 2, then
your function returns @tt{img} unchanged (base case). Otherwise, your function
should place a @racket[circle] on the recursive result where @tt{x} is shifted
by @tt{(+ size (next-size size))}, @tt{size} is @tt{(next-size size)}, and
the original @tt{img}.

@bold{Ex 2}: Adjust @tt{next-size} so that it divides by 3/2 (or something a bit
smaller), instead of 2. Modify @tt{WIDTH} so you get something like the image
below.

@image{lab21-circles-2.png}

@bold{Ex 3}: Define a similar function @tt{spiral} that takes four numbers and
an image @tt{img}. In addition to @tt{x} and @tt{size}, the function also takes
@tt{y} and @tt{ang}, which represent the center @tt{y} coordinate of a circle,
and the current angle (in radians).

@colorize["red"]{Hint}: In your recursive call to @tt{spiral}, you must update
@tt{x} and @tt{y} based on your (or your partner's) knowledge of trigonometry:

@itemlist[
  @item{@tt{next-x = x + (size+nextsize) * cos(ang)},}
  @item{@tt{next-y = y + (size+nextsize) * sin(ang)}.}
]

You can also add to the @tt{ang}. Try @racket[(/ pi 10)]. Using that as the
initial angle should give you something like the image below.

@image{lab21-circles-3.png}

@bold{Ex 4}: Modify the various parameters to your function to get interesting
results. For example, if you modify the function to be structurally recursive
(using @racket[sub1] instead of @tt{next-size}), and draw the same size circles
each time (be sure the function terminates!), you might get something like the
image below.

@image{lab21-spiral-1.png}


@section[#:style 'unnumbered #:tag "lab21:tree"]{Trees}

Swap @bold{Head} and @bold{Hands}!

Now that we're warmed up, let's implement a few generative fractals. First up is
a simple "tree" fractal.

Here's some helper code to get you started:

@#reader scribble/comment-reader (racketblock
;; put-line : Number Number Number Number String Image -> Image
;; Put a line on the image starting at (x, y) len distance in the given
;; direction with the given color.
(define (put-line x y ang len color scn)
  (place-image (line (* (cos ang) len)
                     (* (sin ang) len) color)
               (+ x (* (cos ang) (/ len 2)))
               (+ y (* (sin ang) (/ len 2))) scn))
)

@bold{Ex 5}: Design the function @tt{tree} that takes four numbers, @tt{x},
@tt{y}, @tt{ang}, and @tt{len} and draws a tree on a given image. If the
@tt{len} is less than 3, then the function just puts a line at
@tt{x}/@tt{y}/@tt{ang}/@tt{len} of some color (like @racket{green}) into the
image.

Otherwise the function puts a @racket{brown} line at
@tt{x}/@tt{y}/@tt{ang}/@tt{len}, and recursively calls itself twice: once with
@tt{x}/@tt{y} placed one third up the trunk at an angle off to the left, and
another with @tt{x}/@tt{y} placed two thirds up the trunk at an angle off to the
right. The length should be cut in half.

@colorize["red"]{Hint}: One of the recursive calls should be calculated as
follows:

@itemlist[
  @item{@tt{next-x = x + len/3 * cos(ang)},}
  @item{@tt{next-y = y + len/3 * sin(ang)},}
  @item{@tt{next-ang = ang + pi/3},}
  @item{@tt{next-len = len/2}.}
]

The other recursive call should use @tt{ang - pi/3}, at @tt{2*len/3} away.

You should be able to easily modify the parameters to get various images.

@image{lab21-tree-2.png}
@image{lab21-tree-3.png}
 

@section[#:style 'unnumbered #:tag "lab21:koch"]{Koch Snowflake}

Swap @bold{Head} and @bold{Hands}!

@bold{Ex 6} Similar to the tree fractal, we can do interesting things by
changing recursive calls. The
@link["https://en.wikipedia.org/wiki/Koch_snowflake"]{Koch snowflake} is an
interesting recursive fractal. Design the function @tt{koch} that takes the same
arguments as @tt{tree}, with an additional @tt{iter} parameter. Like in the last
lab, @tt{iter} tracks the number of remaining iterations.

For each iteration you cut the line into three pieces and make four recursive
calls. The key insight is that you should only draw a line when the number of
remaining iterations reaches 0. The images below show the first 4 iterations and
a more elaborate version (bigger, with 6 iterations), of a "snowflake" variation
that (I think) looks pretty cool. See if you can emulate it, or come up with
something better!

@image{lab21-koch-0.png}
@image{lab21-koch-1.png}
@image{lab21-koch-2.png}
@image{lab21-koch-3.png}
@image{lab21-koch-4.png}
