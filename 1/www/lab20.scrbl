#lang scribble/manual
@(require scribble/core (for-label lang/htdp-intermediate) "helper.rkt")

@title[#:style 'unnumbered #:tag "lab20"]{Lab 20: The Dragon Fractal}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/intermediate-lam.html"]{Intermediate
Student Language with Lambda}.

Make sure you follow
@link["https://cs.umd.edu/class/fall2017/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.

Choose the initial @bold{Head} and @bold{Hands}, and get started!

@section[#:style 'unnumbered #:tag "lab20:divergence"]{A Bit Different}

Almost every function we've written to-date follows some template based on the
structure of the input data. Today we're going to design a program that is
structured based on the generated output.

Today you will design functions to draw the iterations of an interesting fractal
curve known as the
@link["https://en.wikipedia.org/wiki/Dragon_curve"]{Dragon}. The shape was
featured in the headings of chapters in the book
@link["https://xkcd.com/87/"]{Jurassic Park}.

We'll start off by building a simple line drawing program. Then we'll combine
pieces into the fractal's @emph{generative} recursion.

@image{dragon.gif}

@section[#:style 'unnumbered #:tag "lab20:dd"]{Lines}

@#reader scribble/comment-reader (racketblock
;; A Dir is one of "left", "right", "up", or "down".
;; Interp: A direction in which a line may be drawn.
)

@bold{Ex 1}: Design the function @tt{rotate-dir} that returns the 90°
counter-clockwise rotatation of the given @emph{Dir}.

@bold{Ex 2}: Design the function @tt{rotate-dirs} that rotates all the
@emph{Dir}s in a given @emph{[Listof Dir]}

@bold{Ex 3}: Design the function @tt{move-posn : @emph{Number} @emph{Number}
@emph{Dir} @emph{Number} -> @emph{Posn}} that returns a @emph{Posn} that is the
result of moving the given @tt{x} and @tt{y} in the given direction, by the
given @tt{amount}.

@bold{Ex 4}: Design the function @tt{draw-dirs} that adds lines to the given
image in the given list of directions (in order), starting at the given x and y
onto the given image.

@#reader scribble/comment-reader (racketblock
;; draw-dirs : [Listof Dir] Number Number Image -> Image 
;; Draw lines of any color on the given image, following the given directions
;; starting at (x, y).
)

@colorize["red"]{Hint}: Use structural recursion over the directions here, and
choose some constant amount for move-posn (say 5).

Here's a small set of definitions to help you test your functions. Use the arrow
keys to create a path (a @emph{[Listof Dir]}). You can hit r to rotate all the
points to the left.

@#reader scribble/comment-reader (racketblock
;; Screen Size...
(define W 400) (define H 400)
 
;; Draw wrapper
(define (draw w)
  (local [(define lst (reverse w))]
    (draw-dirs lst (/ W 2) (/ H 2) (empty-scene W H))))
 
;; Key handler
(define (key w ke)
  (cond [(ormap (λ (d) (key=? ke d)) '("up" "down" "left" "right"))
         (cons ke w)]
        [(key=? ke "r") (rotate-dirs w)]
        [else w]))
 
(big-bang '()
          (to-draw draw)
          (on-key key))
)

@section[#:style 'unnumbered #:tag "lab20:drawit"]{Drawing the Fractal}

Swap @bold{Head} and @bold{Hands}!

Now we need to generate our fractal! The function @tt{dragon} takes a
@emph{[Listof Dir]} and a @emph{Natural} @tt{iters}, which counts the number of
remaining iterations. The number @tt{iters} will count down to 0; the list of
directions will double in size after each iteration.

@bold{Ex 5}: Based on the information in the last pararaph, why must we give a
non-empty list of directions as the initial directions given to @tt{dragon}?

@#reader scribble/comment-reader (racketblock
;; dragon : [Listof Dir] Number -> [Listof Dir]
;; Compute the next iteration of the Dragon Fractal.
(define (dragon lod iters) '()) ; <- stub
 
(check-expect (dragon '("down") 0) '("down"))
(check-expect (dragon '("down") 1) '("down" "right"))
(check-expect (dragon '("down") 2) '("down" "right" "up" "right"))
(check-expect (dragon '("down") 3)
              '("down" "right" "up" "right" "up" "left" "up" "right"))
)

@bold{Ex 6}: Design the function @tt{dragon}. If there are no iterations
remaining return the given list of directions. Otherwise, return a new list of
directions such that

@itemlist[
  @item{all of the directions in the given list are rotated
        counter-clockwise,}
  @item{that rotated list of directions is reversed,}
  @item{that rotated/reversed list of directions is appended to the end of the
        given directions, and then}
  @item{recur on the whole list of directions with one fewer remaining
        @tt{iters}.}
]

Now we can remove (or comment out) our old @racket[big-bang] code and replace it
with this:

@#reader scribble/comment-reader (racketblock
;; draw : Natural -> Image
(define (draw w)
  (local [(define lst (dragon '(down) w))]
    (draw-dirs lst (/ W 2) (/ H 2) (empty-scene W H))))
 
;; key : Natural KeyEvent -> Natural
(define (key w ke)
  (cond [(key=? ke "up") (add1 w)]
        [(and (key=? ke "down") (> w 1))
         (sub1 w)]
        [else w]))
 
;; Let's make this fractal!
(big-bang 0
          (to-draw draw)
          (on-key key))
)

You can hit the up or down arrows to increase or decrease the number of
iterations of the generated fractal.
