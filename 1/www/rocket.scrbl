#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner))

@title[#:style '(unnumbered non-toc) #:tag "rocket"]{Landing a RKT in BSL}

We're trying to land a rocket (@tt{RKT}) safely on the ground. (See
@link["http://www.ccs.neu.edu/home/matthias/HtDP2e/part_prologue.html"]{HtDP2e's
prologue} for the original treatment.) We're doing that with
@tt{2htdp/{image,universe}} in the Beginning Student Language (BSL).


@section[#:style 'unnumbered #:tag "rocket:draw"]{Drawing a RKT}

First, we'll draw a small rocket and name it @tt{RKT}.

@#reader scribble/comment-reader (racketblock
;; A nifty rocket
(define RKT
  (overlay/offset (overlay/offset (triangle 18 'solid "red")
                                  0 27
                                  (ellipse 24 60 'solid "gray"))
                  0 10
                  (isosceles-triangle 60 24 'solid "black")))
)

The identifier uses all capitals to indicate that @tt{RKT} is a defined
constant. Let's draw a few example frames from our animation by placing @tt{RKT}
on an empty scene 100px wide and 200px high.

@racketinput[(place-image RKT 0 0 (empty-scene 100 200))]

The library function @racket[place-image] requires the X/Y pixel offset from the
first image to the second image, but placing @tt{RKT} at 0,0 cuts off 3/4 of the
rocket. Let's move the rocket to the center of the scene by changing the X
offset to 50.

@racketinput[(place-image RKT 50 0 (empty-scene 100 200))]

Now we'll try to find the proper Y offset to place the rocket at the bottom of
the scene.

@#reader scribble/comment-reader (racketinput
(place-image RKT 50 200 (empty-scene 100 200))  ; too far
)

@#reader scribble/comment-reader (racketinput
(place-image RKT 50 150 (empty-scene 100 200))  ; not far enough
)

@#reader scribble/comment-reader (racketinput
(place-image RKT 50 166 (empty-scene 100 200))  ; looks OK
)

If we adjust where we place @tt{RKT} by -34px (from 200 to 166), the edge looks
just about right. Copying and pasting is getting annoying, so let's make a
function that will do most of the work for us. We want to define a function that
can draw our rocket for use in @racket[animate], so let's call it
@tt{draw-rocket}.


@section[#:style 'unnumbered #:tag "rocket:where"]{Drawing RKT, Where?}

We will land our rocket with @racket[animate]. The signature of @racket[animate]
looks a little more complex than we're used to seeing.

@#reader scribble/comment-reader (racketblock
;; (animate create-image) → natural-number/c
;;   create-image : (-> natural-number/c scene?)
)

We can write this in a slightly more familiar form:

@#reader scribble/comment-reader (racketblock
;; A Natural is a non-negative integer: one of { 0, 1, 2, ... }.
;; animate : (Natural -> Image) -> Natural
)

The function @racket[animate] expects a function as its input. The given
function must have the signature @tt{Natural -> Image}. So, we need to implement
a function @tt{land-rocket} with that signature.

What should we do with the natural number?  We're generalizing the expression
@racket[(place-image RKT 50 0 (empty-scene 100 200))] and there are four obvious
places we could use the natural number @tt{n}: the X/Y offsets and the scene
width/height.

We want the rocket to land--move from the top of the scene to bottom--which
means changing where we place @tt{RKT} vertically on the empty scene. We can do
that by changing the Y offset given to @racket[place-image] and test our example
frames.

@#reader scribble/comment-reader (racketblock
;; land-rocket : Natural -> Image
;; Draw a rocket on an empty scene with the y-offset N.
(define (land-rocket n)
  (place-image RKT 50 n (empty-scene 100 200)))
)

@racketinput[(land-rocket 0)]
@racketinput[(land-rocket 66)]
@racketinput[(land-rocket 166)]

The first example with @tt{n} equal to 0 has the rocket partially on the scene, but
our landing animation should not start with any of the rocket visible. We should
be able to use the same adjustment as earlier (-34px) to draw the rocket just
above the top of the scene.

@racketinput[(land-rocket -34)]

Hmm, that left a few pixels hanging, so we must have gotten the original
adjustment wrong. After a bit more trial and error, adjusting by -36px seems to
be correct.

@racketinput[(land-rocket -36)]

There are two problems here.

First, and despite the fact that no errors occurred and images were returned, we
broke @tt{land-rocket}'s contract by applying it to negative integers. 

Second, it would be tedious to perform this pixel adjustment each time we apply
the function @tt{land-rocket}. Instead, we can perform that adjustment inside the
definition of @tt{land-rocket/v2} (and again test our examples).

@#reader scribble/comment-reader (racketblock
;; land-rocket/v2 : Natural -> Image
;; Draw a rocket on an empty scene with the adjusted y-offset N.
(define (land-rocket/v2 n)
  (place-image RKT 50 (- n 36) (empty-scene 100 200)))
(land-rocket/v2 0)
(land-rocket/v2 100)
(land-rocket/v2 200)
)

Fantastic, let's see how the animation looks!

@section[#:style 'unnumbered #:tag "rocket:land"]{Landing the RKT}

@racketinput[(animate land-rocket/v2)]

Uh-oh. The rocket starts off correctly, but plows right through the bottom of
the scene. After about (/ 200 28) seconds, the rocket begins to pass below the
scene's bottom edge.

So, how can we fix this?

A1: Is there some a @tt{stop-animate} function we can call to stop the animation
    after the right amount of time?
   
R1: No, and it's not obvious where to use such a function even if there
    were. Per its documentation, starting at 0 @racket[animate] applies
    @tt{land-rocket/v2} to each successive natural number 28 times per
    second. Only after we close the animation's window does the application of
    the @racket[animate] function return the most recent (and highest) natural
    number. So, any @racket[(stop-animate (/ 200 28))] following our application
    of @racket[animate] would not be evaluated until after the animation has
    ended.

A2: Couldn't we call the hypothetical @racket[stop-animate] inside of
    @tt{land-rocket/v2}?

R2: Yes, but such a function would have to be performing @italic{side effects},
    which we'll be discussing in more depth late this semester and next
    semester. It would require indirect communication with the @racket[animate]
    function through some shared state (e.g. the state of the window manager).

    Rather than solving the problem indirectly, let's just stop the rocket from
    going past the bottom of the scene in @tt{land-rocket/v3}.

How do we know when to stop the rocket? The fun answer is to try to stop the
animation right when it hits the bottom, but its correctness depends on our
reflexes. Reviewing our examples for @tt{land-rocket/v2}, the rocket is flush at
the bottom when @tt{n} is 200. This makes sense, since the scene height is 200px
and the rocket moves exactly the length of the scene.

We only want to place the rocket as we did in @tt{land-rocket/v2} while @tt{n}
is less than or equal to 200, otherwise we place the rocket at the same location
as 200.

We can produce different outputs conditionally with a @racket[cond] expression.

@racketinput[(check-expect (cond [(number? "foo") "a number?"]
                                 [(string? "foo") "a string!"]
                                 [(= (+ 1 1) 2) "1+1=2"])
                           "a string!")]

Each clause in a @racket[cond] has two parts wrapped in square brackets: the
question expression on the left and the result expression on the right. If any
of the questions return @racket[#true], the corresponding result is returned.

The last clause may use @racket[else] as a question that always succeeds.

@racketinput[(check-expect (cond [(string? 42) "a string?"]
                                 [else "not a string!"])
                           "not a string!")]

If no test succeeds, @racket[cond] results in an error.

@racketinput[(check-error (cond [#false "neither this one,"]
                                [(= 1 2) "nor this one"])
                          "cond: all question results were false")]

We can implement @tt{land-rocket/v3} using @racket[cond] to stop moving the rocket when
@tt{n} is larger than 200.

@#reader scribble/comment-reader (racketblock
;; land-rocket/v3 : Natural -> Image
;; Draw a rocket on an empty scene with the adjusted y-offset N.
(define (land-rocket/v3 n)
  (cond [(<= n 200) 
         (place-image RKT 50 (- n 36) (empty-scene 100 200))]
        [(> n 200)
         (place-image RKT 50 (- 200 36) (empty-scene 100 200))]))
(land-rocket/v3 0)
(land-rocket/v3 100)
(land-rocket/v3 200)
(land-rocket/v3 300)
)

Our last test shows the rocket still on the ground. We can confirm this by
running the animation.

@racketinput[(animate land-rocket/v3)]

This is pretty cool, but I'm not happy with our rocket anymore. (Not nearly
exciting enough.) Instead, let's land a UFO!


@section[#:style 'unnumbered #:tag "rocket:ufo"]{Landing a UFO}

@racketblock[
(define UFO
  (overlay/xy (ellipse 30 22 'solid 'black)
              -26 5
              (overlay (ellipse 72 22 'solid 'gray)
                       (ellipse 76 26 'solid 'green)
                       (ellipse 80 30 'solid 'black))))
]

We can implement @tt{land-ufo} by replacing all uses of @tt{RKT} in
@tt{land-rocket/v3} with @tt{UFO}, right?

@#reader scribble/comment-reader (racketblock
;; land-ufo : Natural -> Image
;; Draw a ufo on an empty scene with the adjusted y-offset N.
(define (land-ufo n)
  (cond [(<= n 200) 
         (place-image UFO 50 (- n 36) (empty-scene 100 200))]
        [(> n 200)
         (place-image UFO 50 (- 200 36) (empty-scene 100 200))]))
(land-ufo 0)
(land-ufo 100)
(land-ufo 200)
(land-ufo 300)
)

Hmm, the @tt{UFO} doesn't quite make it to the bottom of the scene. The
adjustment we used for @tt{RKT} doesn't work for @tt{UFO}. So that adjustment
do we need?

Recall why we made an adjustment in the first place. Our original
@tt{land-rocket} placed the rocket with the bottom 36px on the scene.

@racketinput[(land-rocket 0)]

What happens if we move it another 36px?

@racketinput[(land-rocket 36)]

It looks like it's entirely on the scene. Judging this by eye got us in trouble
earlier, but we can check this manually with the function @racket[image-height].

@racketinput[(check-expect (image-height RKT) 72)]

So, @racket[place-image] measures its X/Y offsets from the @bold{center} of RKT
to the upper-left corner of the scene (and the documentation confirms this).

We can correct the adjustment in @tt{land-ufo/v2}, using @racket[image-height]
to calculate the proper adjustment.

@#reader scribble/comment-reader (racketblock
;; land-ufo/v2 : Natural -> Image
;; Draw a ufo on an empty scene with the adjusted y-offset N.
(define (land-ufo/v2 n)
  (cond [(<= n 200) 
         (place-image UFO
                      50 (- n (/ (image-height UFO) 2))
                      (empty-scene 100 200))]
        [(> n 200)
         (place-image UFO
                      50 (- 200 (/ (image-height UFO) 2))
                      (empty-scene 100 200))]))
(land-ufo/v2 0)
(land-ufo/v2 100)
(land-ufo/v2 200)
(land-ufo/v2 300)
)


@section[#:style 'unnumbered #:tag "rocket:magic"]{No More Magic}

The UFO lands exactly as expected! The constant 36 that we subtracted from the
given natural number is known as a @italic{magic constant}. We found it by trial
and error, ignoring the reason why that particular adjustment was correct. Once
we changed the conditions that made 36 the correct adjustment (by using an image
with a different height) the magic went away.

Instead of blindly adjusting by 36 pixels, @tt{land-ufo/v2} moves the image up
by half its height.

Are there any other @italic{magic constants} in @tt{land-ufo/v2}?

Answer 1: What about the 2 in each division that replaced 36?

Response 1: Not quite, @racket[place-image] always measures from the center of
            the first image, so the 2 should only change if we stop using
            @racket[place-image], in which case we'd have to change all the
            arguments anyway.

A2: What about 50, the X offset given to @racket[place-image]?

R2: Yes, we used 50 because it placed the image in the center of the scene. If
    we changed the width of the scene from 100, we would have to adjust the X
    offset as well.

A3: Isn't the 200 in each of the @racket[cond] questions is based on the height of the
    scene?

R3: Yep, those values are tied together too.

We can explicitly link these values together by naming the scene dimensions and
referencing those constants inside @tt{land-ufo/v3}.

@#reader scribble/comment-reader (racketblock
;; land-ufo/v3 : Natural -> Image
;; Draw a ufo on an empty scene with
;; its bottom N pixels from the top.
;; Constants:
(define W 100)
(define H 200)
;;
(define (land-ufo/v3 n)
  (cond [(<= n H) 
         (place-image UFO
                      (/ W 2)
                      (- n (/ (image-height UFO) 2))
                      (empty-scene W H))]
        [(> n H)
         (place-image UFO
                      (/ W 2)
                      (- H (/ (image-height UFO) 2))
                      (empty-scene W H))]))
(land-ufo/v3 0)
(land-ufo/v3 100)
(land-ufo/v3 200)
(land-ufo/v3 300)
)


@section[#:style 'unnumbered #:tag "rocket:general"]{Generalization}

Q: Could we pass the scene width and height in as arguments to the function
   instead of referencing those constants?

A: We can, let's do that instead, but we may run into another issue down the
   road.

Inside @tt{land-ufo/v3}, we refer to the constants @tt{W} and @tt{H} that were
defined outside the function. As suggested, we can instead generalize our
function over the scene dimensions by adding them as arguments. This way we can
draw the UFO on a scene of any size, as shown in @tt{land-ufo-on-scene}.

@#reader scribble/comment-reader (racketblock
;; land-ufo-on-scene : Natural Natural Natural -> Image
;; Draw a ufo on an empty scene of size WxH
;; with its bottom N pixels from the top.
(define (land-ufo-on-scene n w h)
  (cond [(<= n h) 
         (place-image UFO
                      (/ w 2)
                      (- n (/ (image-height UFO) 2))
                      (empty-scene w h))]
        [(> n h)
         (place-image UFO
                      (/ w 2)
                      (- h (/ (image-height UFO) 2))
                      (empty-scene w h))]))
(land-ufo-on-scene   0 100 200)
(land-ufo-on-scene 100 120  80)
(land-ufo-on-scene 200  80 120)
(land-ufo-on-scene 300 100 500)
)

Since @tt{land-ufo-on-scene} takes three arguments rather than one, it breaks
@racket[animate]'s contract and would cause an error. But we can define
size-specific helpers in terms of @tt{land-ufo-on-scene} that can be used with
@racket[animate].

@racketblock[
  (define (land-ufo/100x200 n) (land-ufo-on-scene n 100 200))
  (define (land-ufo/200x400 n) (land-ufo-on-scene n 200 400))
  (define (land-ufo/80x40 n) (land-ufo-on-scene n 80 40))
  (animate land-ufo/100x200)
  (animate land-ufo/200x400)
  (animate land-ufo/80x40)]


We can even take one more step to generalize over the image as well, shown in
@tt{land-image-on-scene}. As before, we can define specialized helpers that can
be passed to @racket[animate].

@#reader scribble/comment-reader (racketblock
;; land-image-on-scene : Natural Natural Natural -> Image
;; Draw IMG on an empty scene of size WxH
;; with its bottom N pixels from the top.
(define (land-image-on-scene img n w h)
  (cond [(<= n h) 
         (place-image img
                      (/ w 2)
                      (- n (/ (image-height img) 2))
                      (empty-scene w h))]
        [(> n h)
         (place-image img
                      (/ w 2)
                      (- h (/ (image-height img) 2))
                      (empty-scene w h))]))

(define (land-rkt/100x200 n) (land-image-on-scene RKT n 100 200))
(define (land-ufo/80x80 n) (land-image-on-scene UFO n 80 80))

(land-rkt/100x200   0)  (land-rkt/100x200 100)
(land-rkt/100x200 200)  (land-rkt/100x200 300)

(land-ufo/80x80     0)  (land-ufo/80x80   100)
(land-ufo/80x80   200)  (land-ufo/80x80   300)
(animate land-rkt/100x200)
(animate land-ufo/80x80)
)

Q: Can't we use @racket[min] to get rid of the @racket[cond]?

A: That's clever, and correct! 

@#reader scribble/comment-reader (racketblock
;; land-image-on-scene/min : Natural Natural Natural -> Image
;; Draw IMG on an empty scene of size WxH with its
;; bottom N pixels from the top (without a cond).
(define (land-image-on-scene/min img n w h)
  (place-image img
               (/ w 2)
               (- (min n h) (/ (image-height img) 2))
               (empty-scene w h)))

(define (land-rkt/min n) (land-image-on-scene RKT n 100 200))
(land-rkt/min   0)  (land-rkt/min 100)
(land-rkt/min 200)  (land-rkt/min 300)
(animate land-rkt/min)
)

But, should we? To me, it's less obvious that @tt{land-image-on-scene/min} has
different behavior when the arguments N and H satisfy certain conditions
(relative to @tt{land-image-on-scene}). While the use of @racket[min] makes the
function smaller and removes repeated expressions, it may not make the function
easier to understand. With all else equal, the better implementation is the one
that is easiest to understand. Anyone looking at your code years down the line
will thank you.
