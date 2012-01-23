#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          "../lab.rkt"
          "../unnumbered.rkt"
          "../utils.rkt"
          (for-label (except-in class/0 define-struct check-expect)
                     (except-in 2htdp/image color)
                     (only-in lang/htdp-intermediate-lambda define-struct check-expect)
                     class/universe))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require class/0))
    (the-eval '(require 2htdp/image))
    ;(the-eval '(require class/universe))
    the-eval))

@(define exercise (exercise-counter))

@title[#:tag "lab03"]{1/23: Interfaces in Space!}

@lab:section{Update your class system}

Throughout the course the @racket[class] languages will be under constant
development, so it's important that you keep up to date. In fact, we've already
updated it since the last lab.

@exercise{
  Update your class system. In DrRacket, go to @tt{File → Install .plt File... →
  Web} and copy in this URL:

  @indented{
    @tt[class-system-latest]
  }
}

The URL above will always point to the latest version of the class system. Since
DrRacket remembers the last URL you used in the @tt{Install .plt File...}
dialog, you should usually only have to open the dialog and click @tt{Ok} to
update to the latest version.

Information about the latest version is always available at @secref["class"].

@lab:section{Space!}

In this lab, we're going to clean up all the junk in space, both
asteroids and satellites, which are littering the galaxy.

Let's start with a basic animation of this problem. A @racket[World]
consists of @racket[Junk], each of which has a location (@racket[x],
@racket[y]), can @racket[draw] itself, and can @racket[step] itself through an
animation. An @racket[Object], for now, is either a @racket[Asteroid] or a
@racket[Satellite]. @racket[Satellite]s are round, and @racket[Asteroid]s are rectangular.

@#reader scribble/comment-reader
(racketmod
class/0
(require 2htdp/image)
(require class/universe)

(define WIDTH  500)
(define HEIGHT 500)

; A World is a (new world% [Listof Object])
(define-class world%
  (fields objects)

  ; -> World
  ; Advance the World
  (define (on-tick)
    (new world% (map (λ (c) (send c step)) (send this objects))))

  ; -> Image
  ; Draw the World
  (define (to-draw)
    (foldr (λ (c scn) (place-image (send c draw)
                                   (send c x)
                                   (send c y)
                                   scn))
           (empty-scene WIDTH HEIGHT)
           (send this objects))))

; An Object is one of:
;  - Asteroid
;  - Satellite

; A Location is a Complex where (x,y) is represented as the complex number x+yi
; A Velocity is a Complex where (x,y) is represented as the complex number x+yi
)

---complex numbers? We're going to use a trick for working with 2D geometry:
represent the location @math{(x,y)} with the
@link["http://docs.racket-lang.org/reference/generic-numbers.html?q=complex+numbers#(part._.Complex_.Numbers)"]{complex
number} @math{x+yi}. Common geometric transformations can now be done simply
with addition and subtraction.

For instance, if you're at location @math{(2,5)} and want to translate by
@math{(-1,2)}---1 step left and 2 steps down, in screen coordinates---to get to
location @math{(1,7)}, then you can @emph{add} your location @math{2+5i} to the
translation @math{-1+2i} to get your new location @math{1+7i}.

Or consider the opposite situation: if you're at @math{(2,5)} and want to know
what you must translate by to get to @math{(1,7)}, you can @emph{subtract}
@math{1+7i - 2+5i = -1+2i} to find out that @math{(-1,2)} is the translation
from @math{(2,5)} to @math{(1,7)}.

There's much more to say about the relationship between 2D geometry and complex
arithmetic, but we can get a long way with just the basics. Now where were we?

@#reader scribble/comment-reader
(racketblock
; A Non-Negative is a non-negative Number
; A Color is a String
; A Satellite is a (new satellite% Non-Negative Color Location Velocity)
(define-class satellite%
  (fields radius color location velocity)

  ; -> Number
  ; The x-coordinate of the Satellite
  (define (x)
    (real-part (send this location)))

  ; -> Number
  ; The y-coordinate of the Satellite
  (define (y)
    (imag-part (send this location)))

  ; -> Image
  ; The image representing the Satellite
  (define (draw)
    (circle (send this radius) "solid" (send this color)))

  ; -> Satellite
  ; The next Satellite in the animation sequence
  (define (step)
    (new satellite%
         (send this radius)
         (send this color)
         (+ (send this velocity) (send this location))
	 (send this velocity))))

(check-expect (send (new satellite% 5 "red" 50+10i 0+1i) x) 50)
(check-expect (send (new satellite% 5 "red" 50-10i 0+1i) x) 50)
(check-expect (send (new satellite% 5 "red" 50+10i 0+1i) y) 10)
(check-expect (send (new satellite% 5 "red" 50-10i 0+1i) y) -10)
(check-expect (send (new satellite% 5 "red" 50+10i 0+1i) draw)
              (circle 5 "solid" "red"))
(check-expect (send (new satellite% 5 "red" 50+10i 0+1i) step)
              (new satellite% 5 "red" 50+11i 0+1i))
(check-expect (send (new satellite% 5 "red" 50-10i 0+1i) step)
              (new satellite% 5 "red" 50-9i 0+1i))
(check-expect (send (new satellite% 5 "red" 50-10i 1+0i) step)
              (new satellite% 5 "red" 51-10i 1+0i))

; A Asteroid is a (new asteroid% Location Non-Negative Non-Negative Color Velocity)
(define-class asteroid%
  (fields width height color location velocity)

  ; -> Number
  ; The x-coordinate of the Asteroid
  (define (x)
    (real-part (send this location)))

  ; -> Number
  ; The y-coordinate of the Asteroid
  (define (y)
    (imag-part (send this location)))

  ; -> Image
  ; The image representing the Asteroid
  (define (draw)
    (rectangle (send this width) (send this height) "solid" (send this color)))

  ; -> Asteroid
  ; The next Asteroid in the animation sequence
  (define (step)
    (new asteroid%
         (send this width)
         (send this height)
         (send this color)
         (+ (send this velocity) (send this location))
	 (send this velocity))))

(check-expect (send (new asteroid% 10 20 "blue"  50+60i 0+1i) x) 50)
(check-expect (send (new asteroid% 10 20 "blue" -50+60i 0+1i) x) -50)
(check-expect (send (new asteroid% 10 20 "blue"  50+60i 0+1i) y) 60)
(check-expect (send (new asteroid% 10 20 "blue" -50+60i 0+1i) y) 60)
(check-expect (send (new asteroid% 10 20 "blue" -50+60i 0+1i) draw)
              (rectangle 10 20 "solid" "blue"))
(check-expect (send (new asteroid% 10 20 "blue" 50+60i 0+1i) step)
              (new asteroid% 10 20 "blue" 50+61i 0+1i))
(check-expect (send (new asteroid% 10 20 "blue" -50+60i 0+1i) step)
              (new asteroid% 10 20 "blue" -50+61i 0+1i))

(big-bang (new world% (list (new satellite%   5    "red"   50+10i 0+1i)
                            (new satellite%  10    "red"  150+10i 1+1i)
                            (new satellite%  20    "red"  250+10i 1-1i)
                            (new satellite%  10    "red"  350+10i 1+0i)
                            (new satellite%   5    "red"  450+10i 2+1i)
                            (new asteroid% 30 20 "blue"  50+60i 1-1i)
                            (new asteroid% 15 10 "blue" 150+60i 2+1i)
                            (new asteroid%  5  5 "blue" 250+60i 2-1i)
                            (new asteroid% 15 10 "blue" 350+60i 0+2i)
                            (new asteroid% 30 20 "blue" 450+60i 1+0i))))
)

When you run this, you should see satellites and asteroids moving. (Simple
beginnings...)

@exercise{
Write a function that produces a random initial world, and runs the animation.
}

As we said above, an Junk is something that has a @racket[location] along
with convenience methods for its @racket[x] and @racket[y] coordinates, can
@racket[draw] itself, and can @racket[step] itself through an animation.

@exercise{
  Add a description of the interface of @racket[Junk] for the
  behaviors common to both @racket[satellite%] and @racket[asteroid%].
}

@;{
The convenience methods @racket[x] and @racket[y] are useful in the World's
@racket[to-draw] method where it needs to use the x- and y-coordinates
separately, but notice that the methods are implemented in the same way for
Satellites and Asteroids. Also notice that both classes have a @racket[location] field.

@exercise{
  Use delegation to abstract the @racket[location] field and the @racket[x] and
  @racket[y] methods into a common class, @racket[point%].
}


Also, the two @racket[step] methods for Satellites and Asteroids are trying to do the
same thing---move one step with the appropriate direction---but they aren't expressed in a way
that we can abstract.
}

@;{
@exercise{
  Refactor the @racket[step] methods. In each of @racket[satellite%] and
  @racket[asteroid%], define a @racket[move] method that takes a location
  @racket[dz] and updates the Satellite's (or Asteroid's) @racket[location] by
  translating by @racket[dz].

  This way, the @racket[satellite%] @racket[move] method will know how to say
  @racket[(new satellite% ...)] and the @racket[asteroid%] @racket[move] method will
  know how to say @racket[(new asteroid% ...)], and @racket[step] can remain
  oblivious to which kind of object it's working on.

  @; Abstract @racket[step] into racket[junk%].

  (Aside: the letter @math{z} is typically used for complex numbers, just as
  @math{x} and @math{y} are typically used for real numbers.)
}
}

@lab:section{Escape}

@exercise{
  End the game when all the space junk escapes. Use an @racket[escaped?]
  method on Junk to test whether the Junk has it outside of the
  screen, and end the game when all of it has done so.
}

When all the Junk escapes the screen, the game is over---but for this to be
any fun, we need a way to win! Next we will add a spaceship to the bottom of the
screen to destroy the space junk:

@elem[@image["labs/3/spaceship.png"] #:style "center"]

The ship won't do much: it just updates its position to the horizontal position
of the mouse. It's vertical position stays fixed.

@exercise{
  Add a Ship to the game. Fix its vertical position near the bottom of the
  screen and keep its horizontal position aligned with the position of the
  mouse.

  Suggestion: Define a @racket[ship%] class that understands @racket[location],
  @racket[x], @racket[y], and @racket[draw]. (They can be either methods or
  fields.)
}

How much did you have to change your @racket[to-draw] method in @racket[world%]?
If your answer isn't ``very little'', then pause and reconsider your Ship
design.

A @racket[Ship] isn't a @racket[Junk] since it doesn't @racket[step] and never needs to answer
@racket[escaped?], but it does @racket[draw] and have an @racket[x], @racket[y],
and @racket[location]. Moreover, @racket[to-draw] in @racket[world%] only relies
on these last four behaviors---but we currently lack an interface to codify it.

@exercise{
  Split the @racket[Junk] interface into two: a @racket[Drawable]
  interface to support the needs of @racket[to-draw], and a simpler
  @racket[Junk] interface that just includes @racket[step] and
  @racket[escaped?].

  Of your classes, which should implement which interfaces?---note that a class
  might implement multiple interfaces.
}

@;{
Now compare your @racket[ship%] class with your @racket[junk%] class:
@racket[junk%] has a @racket[location] field with @racket[x] and @racket[y]
methods reading from it, and the @racket[ship%] class you just defined should
look very similar.

@exercise{
  Abstract the @racket[location] field and the @racket[x] and @racket[y] methods
  out of @racket[ship%] and @racket[junk%] and into a common superclass,
  @racket[drawable%].
}
}

@exercise{
  Now you have a variety of interfaces and classes.
  Take a moment to sanity check each of them. Which classes should implement
  which interfaces?
}

@lab:section{Surviving invasion}

To clean up the junk, the Ship must shoot some kind of projectile at the
Junk: bullets, lasers, bananas---your choice.

@exercise{
  Define a class for your projectile. For the sake of concreteness, I'll assume
  you chose Banana. Bananas must (1) have a location, (2) know how to draw
  themselves, and (3) step upward over time.

  Which of the classes can you reuse from to save yourself work? Which
  aren't appropriate to reuse?

  Which interfaces do Bananas implement?
}

@exercise{
  Add a list of Bananas to the World. Draw them when the World draws, and step
  them when the World ticks.
}

But whence Bananas?

@exercise{
  Add a @racket[shoot] method to @racket[ship%] that creates a new Banana at the
  Ship's location. Also add a @racket[shoot] method to @racket[world%] that asks
  the Ship to shoot and begins tracking its newly fired Banana.

  Fire Bananas when the user clicks the mouse.
}

Too many Bananas!

@exercise{
  Remove Bananas from the World after they leave the screen. Add an
  @racket[on-screen?] method to @racket[banana%], and remove off-screen Bananas
  on each World tick.
}

If a Banana falls in a forest...

@exercise{
  Add a @racket[contains?] method to the @racket[Junk] interface that
  takes a Location and computes whether the location is within the spatial
  extent of the @racket[Junk].

  Implement @racket[contains?] appropriately for each of @racket[satellite%] and
  @racket[asteroid%]. Note that circles and rectangles occupy different parts of
  space...
}

@exercise{
  Add a @racket[zapped?] method to @racket[Junk] @; and @racket[junk%]
  that takes a list of Bananas as input and tests whether the Junk has been
  hit by any of them. Each tick, remove all Junk from the World that are
  being zapped by a Banana.

  For the purposes of detecting a collision, just test to see if the center of
  the Banana is contained in the Junk. (For added realism, try drawing your
  Bananas as very small dots. Or if you really want to try proper shape
  intersection, try something shaped not like a banana.)
}

@exercise{
Finally, we can win the game!  Add a counter to your world, which
counts how many seconds it takes until all the junk is gone from your
screen.  The goal of the game is to clear the junk in the smallest
amount of time.  

Note that by default, there are 28 ticks per second.
}

@lab:section{Abstraction and Helper Classes}

You've probably noticed at this point that a number of your classes
have some very similar code.  For example, @racket[Asteroid] and
@racket[Satellite] contain very similar @racket[move] methods.  

One technique for refactoring this is to create helper classes that
encapsulate a certain kind of behavior.  For example, we can create
a @racket[Location] data defintion and @racket[location%] class to
encapsulate points that can move.  


@exercise{
The convenience methods @racket[x] and @racket[y] are useful in the World's
@racket[to-draw] method where it needs to use the x- and y-coordinates
separately, but notice that the methods are implemented in the same way for
Satellites and Asteroids. Also notice that both classes have a @racket[location] field.

  Use delegation to abstract the @racket[location] field and the @racket[x] and
  @racket[y] methods into a common class, @racket[location%].
}


@exercise{
Similarly, various kinds of things are drawable.  Perform further
refactoring to remove duplicated drawing code.
}

@lab:section{Go, banana}

@exercise{@bold{(Open ended)}
  Now that we've laid the groundwork for our new hit iPhone game, all that's
  left is a few splashes of creativity. Below are a few ideas---and remember: a
  little randomness can substitute for a lot of complexity.
  @itemlist[
    @item{Add new shapes of Junk}
    @item{Give Junk more complicated movement}
    @item{Let Junk shoot back}
    @item{Increase the difficulty by using keyboard instead of mouse control}
    @item{Spawn new Junk over time}
  ]
}
