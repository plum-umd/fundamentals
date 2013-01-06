#lang scribble/manual
@(require "../utils.rkt"
          (for-label class/0 class/universe))


@title[#:tag "assign04"]{1/30: Playing Animations}

Due: 1/30.

@section{Animation player}

Language: @racketmodname[class/0].

In this assignment, we'll build a video player for animations.  The
player will support play, pause, fast-forward, and rewind.  A player
will interact with an @racket[Animation] solely through the following
interface: 

@verbatim{
;; An Animation implements:
;; next : -> Animation
;; prev : -> Animation
;; render : -> Scene
}

The @racket[next] and @racket[prev] methods work on @emph{all}
@racket[Animation]s.  In particular, the @racket[prev] method on the
first frame of an animation should stay at the first frame, and
@racket[next] of the last frame should stay on the last frame.


Here is an example implementation of an @racket[Animation] that
displays a sequence of numbers:

@codeblock{#lang class/0
(define WIDTH 200) ; Animation dimension in PX.
(define-class count-animation%
 (fields n)
 (define (next)
   (new count-animation% (add1 (send this n))))
 (define (prev)
   (new count-animation% (max 0 (sub1 (send this n)))))
 (define (render)
   (overlay (text (number->string (send this n)) (quotient WIDTH 4) "black")
            (empty-scene 200 200))))
}


@itemlist[#:style 'ordered
@item{@bold{The Player user interface}

The user can interact with a player through the keyboard: @racket["p"]
plays, @racket["f"] fast-forwards, @racket["r"] rewinds, and
@racket["s"] stops playing.

The fast-forward mode should skip every other frame of the animation.
The rewind should go back through the animation.  }

@item{@bold{Animations}

An animation is anything that implements the above interface.  You
need to implement several ways to construct animations.

@itemlist[  
@item{First, animations that are constructed from lists of @racket[Scene]s.} 

@item{Second, animations that consists of a function from a number to
a frame.  For example, when given the following function, your
animation creator should produce the animation as the example above:
@racketblock[
(lambda (i) (overlay (text (number->string i) 50 "black")
                     (empty-scene 200 200)))
]}

@item{Third, you should
create an implementation of @racket[Animation] that wraps an object
with @racket[on-tick] and @racket[to-draw] methods.  That is, the
constructor should take an object that supports the following
@racket[World] interface:
@codeblock{
;; A World implements
;;  to-draw : -> Scene
;;  on-tick : -> World
}
and it should render the @racket[Scene]s that the @racket[World]
produces.  } ]

Finally, you should use the third implementation to play an animation
of a playing animation.  You should demonstrate this in your solution
with one of your other animations.  }]

@;{


#lang class/0
(require 2htdp/image)
(require class/universe)

(define WIDTH 200) ; Animation dimension in PX.
(define HEIGHT 200)

(define TRIANGLE-SIZE (/ WIDTH 10))
(define play (rotate -90 (triangle TRIANGLE-SIZE "solid" "blue")))
(define (bar c) (rectangle (/ TRIANGLE-SIZE 4) TRIANGLE-SIZE "solid" c))
(define pause (beside play (bar "blue") (bar "white") (bar "blue")))
(define (mk-button img)
 (overlay img (rectangle (/ WIDTH 3) (/ WIDTH 3) "solid" "white")))

(define PLAY-IMAGE (mk-button play))
(define PAUSE-IMAGE (mk-button pause))
(define FF-IMAGE (mk-button (beside play play)))
(define FR-IMAGE (mk-button (beside (rotate 180 play) (rotate 180 play))))

;; An Animation implements:
;; next : -> Animation
;; prev : -> Animation
;; render : -> Scene

(define-class count-animation%
 (fields n)
 (define (next)
   (new count-animation% (add1 (send this n))))
 (define (prev)
   (new count-animation% (max 0 (sub1 (send this n)))))
 (define (render)
   (overlay (text (number->string (send this n)) (quotient WIDTH 4) "black")
            (empty-scene WIDTH HEIGHT))))

;; A Player is implements
;; -  > : -> Player
;; - << : -> Player
;; - >> : -> Player
;; - render : -> Scene
;; - next : -> Player


(define-class play% ; implements Player
 (fields anim)
 (define (render)
   (above (send (send this anim) render)
          (beside FR-IMAGE PAUSE-IMAGE FF-IMAGE)))
 (define (next) (new play% (send (send this anim) next)))
 (define (>)  (new pause% (send this anim)))
 (define (<<) (new fr% (send this anim) 2))
 (define (>>) (new ff% (send this anim) 2)))

(define-class pause% ; implements Player
 (fields anim)
 (define (render)
   (above (send (send this anim) render)
          (beside FR-IMAGE PLAY-IMAGE FF-IMAGE)))
 (define (next) this)
 (define (>)  (new play% (send this anim)))
 (define (<<) (new fr% (send this anim) 2))
 (define (>>) (new ff% (send this anim) 2)))

(define-class ff% ; implement Player
 (fields anim factor)
 (define (play)
   (new play% (send this anim)))
 (define (render)
   (above (send (send this anim) render)
          (beside FR-IMAGE PLAY-IMAGE FF-IMAGE)))
 (define (next)
   (send this jump (send this factor)))

 (define (jump n)
   (cond [(zero? n) this]
         [else
          (send (new ff%
                     (send (send this anim) next)
                     (send this factor))
                jump
                (sub1 n))]))

 (define (>) (new play% (send this anim)))
 (define (>>) (new ff% (send this anim) (* 2 (send this factor))))
 (define (<<) (new fr% (send this anim) 2)))

(define-class fr% ; implements Player
 (fields anim factor)
 (define (render)
   (above (send (send this anim) render)
          (beside FR-IMAGE PLAY-IMAGE FF-IMAGE)))
 (define (next)
   (send this jump (send this factor)))

 (define (jump n)
   (cond [(zero? n) this]
         [else
          (send (new fr%
                     (send (send this anim) prev)
                     (send this factor))
                jump
                (sub1 n))]))

 (define (>) (new play% (send this anim)))
 (define (>>) (new ff% (send this anim) 2))
 (define (<<) (new fr% (send this anim) (* 2 (send this factor)))))


(define-class world%
 (fields player)
 (define (on-tick) (new world% (send (send this player) next)))
 (define (to-draw) (send (send this player) render))
 (define (on-key ke)
   (cond [(key=? ke "p") (new world% (send (send this player) >))]
         [(key=? ke ">") (new world% (send (send this player) >>))]
         [(key=? ke "<") (new world% (send (send this player) <<))]
         [else this])))

(define w0 (new world% (new play% (new count-animation% 20))))
(big-bang w0)
					
}

@;{
@title[#:tag "assign03"]{1/26: Zombie, redux}

Due: 1/26.

@itemlist[#:style 'ordered 
 @item{@bold{Zombie!}

 Language: @racketmodname[class0].

 Revise your previous design of the Zombie! game.  Make sure to fix
 any problems that remained when you submitted last week.  Eliminate
 any duplicated code by using the functional abstraction design
 recipe.

 This part of the assignment must be completed in @racketmodname[class0],
 so you cannot use inheritance.

 If you are totally satisfied with your previous submission, you may
 simply submit your previous version of Zombie for this portion of the
 assignment.

 You will likely not have grader feedback in time to incorporate
 tutors' comments into your re-design, so you should revise your
 program as you see fit.  This portion of the assignment @emph{must}
 be based on your previous solution.  If you throw out all of your
 code and submit the solution provided on the web page, you will
 receive no credit for this portion of the assignment.}

 @item{@bold{Super Zombie!}

 Language: @racketmodname[class1]. 

 Revise your design of the Zombie game to include a @racket[zombie<%>]
 and @racket[player<%>] interface.  Implement a @racket[live-zombie%]
 and @racket[dead-zombie%] class that both implement your
 @racket[zombie<%>] interface; implement a @racket[player%] class that
 implements your @racket[player<%>] interface.

 Do not use the functional abstraction recipe.  Instead, if you notice
 code that could be shared between the various classes you've
 designed, design super classes and use inheritance to abstract the
 duplicated code.

 Design a @racket[world%] class for playing the Zombie game that
 interacts with the zombie and player objects only according to the
 interfaces you've designed, i.e. the @racket[world%] class should
 work for @emph{any} objects that correctly implement your zombie and 
 player interfaces.}

 @item{@bold{Modulo Zombie!}

 Language: @racketmodname[class1]. 

 Using your interface design from the previous problem, design a 
 @racket[modulo-player%] class and a @racket[modulo-live-zombie%]
 class that implement the @racket[player<%>] and @racket[zombie<%>]
 interfaces, respectively.

 These alternative implementations should behave as follows: the
 player and the zombies may now "wrap around" on the screen.  If a
 player goes off the top of the screen, they should re-appear on the
 bottom; if the go off of the left side, they should appear on the
 right, etc., and likewise for the zombies.  When calculating in which
 direction they should go, the player and zombies should take into
 account the possibility of wrapping around the screen.  So for
 example, if the player is on the far right side and there is a zombie
 on the far left side, the zombie should head left to wrap around the
 screen and quickly arrive upon the player and feast upon his or her
 brains.  Similarly if the mouse is on the very top and the player is
 on the very bottom, the player should move down to get to the top
 quickly.

 If you need to make changes to your interface design to accommodate
 these new game requirements, you must re-implement your solution to
 problem 2 in order to satisfy the revised interfaces.  In the end,
 the interfaces used and your implementation of the @racket[world%]
 class be the same in both problem 2 and 3.}

 @item{@bold{Mixed Zombie!}

 Language: @racketmodname[class1]. 

 Experiment with different combinations of your classes from part 2
 and 3 (only the player can wrap around; only the zombies can wrap
 around; some of the zombies and the player; some of the zombies, but
 not the player, etc.) until you find a combination you like best.
 Write down an expression that launches the game using this
 combination.}

 @item{@bold{Finger exercises: parametric lists}

 Language: @racketmodname[class1]. 

 Consider the parametric data definition for lists we studied last
 semester:

 @#reader scribble/comment-reader
(racketblock 
 ;; A [Listof X] is one of:
 ;; - empty
 ;; - (cons X [Listof X])
)

Design an analogous class-based representation of parametric lists.
Design a @racket[list<%>] interface that includes @racket[cons],
@racket[empty], @racket[length], @racket[append], @racket[reverse],
@racket[map], @racket[filter], @racket[foldl], and @racket[foldr].

Implement that interface in two ways:
@itemlist[

 @item{Using the recipe for a recursive union represented using
objects, i.e.  similar to the way you developed lists of numbers last
week.}

 @item{Using a "wrapper class", i.e. design a class that has a single
field which contains a "real" list---one built out of @racket[cons]
and @racket[empty].}

]

Any program that interacts with either of these representations
according to the interface should not be able to tell them apart.

Use inheritance to lift method definitions to a super class to the
full extent possible.  (@emph{Hint}: it will help if you realize that many of
these methods may be expressed in terms of a few "core" methods.)  If
possible, have both the recursive union representation and the wrapper
representation share a common super class.

The @racket[cons] and @racket[empty] methods have been added to
facilitate opportunities for abstraction.  You might find them useful
to use when you lift methods to a common super class so that the right
kind of list (either a wrapped or a recursive union list) is
constructed.

Another hint: the names of methods we have chosen overlap with the
name of some standard values that you may like to use when defining
methods, especially in the wrapped list case.  If you refer to these
names within a class, you refer to the method rather than the built-in
value.  If you would like to refer to the built-in value, an easy
work-around is to do something like this:

@#reader scribble/comment-reader
(racketblock
  (define ls:cons cons) ; etc.
)

Now when you want to refer to the @racket[cons] @emph{function} instead
of the @racket[cons] @emph{method}, you can use the name @racket[ls:cons].

}]

}