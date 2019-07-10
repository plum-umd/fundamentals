#lang scribble/manual
@(require scribble/core 
          scribble/examples
          racket/sandbox
          (for-label lang/htdp-beginner) 
          (for-label (except-in 2htdp/image image?))
          (for-label 2htdp/universe)
          ;"helper.rkt" 
	  "../utils.rkt"
          "../defns.rkt")

@(define-syntax-rule (result e) 
   @examples[#:eval the-eval #:result-only e])


@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (only-in lang/htdp-intermediate check-expect)))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
the-eval))


@examples[
  #:eval the-eval
  #:hidden  

(define CHIP1 (bitmap "img/chip0-trans.png"))
(define CHIP2 (bitmap "img/chip1-trans.png"))
(define CHIP3 (bitmap "img/chip2-trans.png"))
(define CHIP4 (bitmap "img/chip3-trans.png"))

]




@title[#:style 'unnumbered #:tag "ex3"]{Exercise 3}

@bold{Due}: Wednesday, July 10, 11:59:59 PM EST.

@(define ex (make-exerciser "Problem"))

Implement these exercises with the
@link["https://docs.racket-lang.org/htdp-langs/beginner.html"]{Beginning
Student Language}. Require the HtDP2e image and universe libraries at
the top of your definitions: 
@racketblock[
(require 2htdp/image)
(require 2htdp/universe)]



@section[#:tag "ex3:submit"]{Directions for submitting}

Please read and follow these intructions carefully.  You should submit
a @bold{single} file named @tt{ex3.rkt} on @link[elms-url]{ELMS}.  You
may submit many times, but each submission should be one file called
@tt{ex3.rkt}.  You may lose points for not following these
instructions.

Make sure the top of the file contains the following, with your name filled in:
@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; Exercise 3
;; Name: ...your name here...
}|

@section[#:tag "ex3:overview"]{Oveview}

The goal of this exercise is to practice
@itemlist[
@item{(1) making an interactive,
event-driven program,}
@item{(2) using compound data (i.e. structures),}
@item{(3) using the ``design recipe'' for
systematic problem solving.}
]

The goal of this exercise set is to make an interactive program that
let's the user control a small avatar that can move horizontally on a
screen.  The avatar, which we will affectionately refer to as "Chip,
the Cheap Sheep," or simply "Chip."  @image[#:scale 3/4 #:style
float-right]{img/chip.gif} Chip moves horizontally, either left or
right.  Chip's direction is controlled by the left and right arrow
keys on the keyboard.  Chip can also go faster or slower (in the
current direction of travel) with the up and down arrow keys.
Conceptually, Chip can run arbitrarily far in either direction, but
only a small window is shown, which Chip can enter or exit.

@section{Design of a Chip}

You should now have seen the basic elements of the design recipe, both
from your readings in HtDP, and from lecture.

In this exercise, we're going to be following the design recipe
process.  Many of the most important steps of the process are going to
be done for you.  As we progress through the course, you will be
responsible for doing more of these steps yourself, so it's important
to follow along in the design process that's given to you.

When designing an interactive program such as this, we should take an
inventory of the @emph{information} that we will need to model.

Take a look at the GIF recording of Chip above.  Think about what
information is needed to reconstruct any moment of the Chip program.
At any given moment, we need to know:
@itemlist[
@item{Chip's horizontal position,}
@item{Chip's direction, and}
@item{Chip's speed.}
]

We will need to represent this information as BSL @emph{data}, which
brings us to the design of our data definition:

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; A Chip is a (make-chip Integer Boolean PositiveInteger)
(define-struct chip (x right? speed))
;; Interpretation: 
;; x: distance from center of Chip to left side of screen (in px)
;; right?: whether Chip is going right
;; speed: rate of Chip's speed in px/tick
}|

So, for example, @racket[(make-chip 10 #true 3)] represents a Chip
that is 10 pixels from the left side of the screen and moving right at
a rate of 3px/tick.

Based on the @racket[big-bang] event system, we will need functions to:

@itemlist[
@item{display a Chip on a scene}
@item{move a Chip according to its direction and speed}
@item{respond to keyboard events by changing Chip's direction or speed}
]

These tasks can help us generate a wishlist of function signatures:

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; chip-display : Chip -> Image
;; Display a Chip on a scene
(define (chip-display c) ...)

;; chip-move : Chip -> Chip
;; Move a Chip according to its direction and speed
(define (chip-move c) ...)

;; chip-key : Chip KeyEvent -> Chip
;; Respond to keyboard events by changing Chip’s direction or speed
(define (chip-key c ke) ...)
}|

Which we can use to write a @racket[main] function that launches a
Chip program.  Here, we've chosen to have it consume an x-coordinate
for the initial placement of Chip.

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; main : Integer -> Chip
;; Launch Chip at given position, moving right slowly
(define (main x)
  (big-bang (make-chip x #true 1)
    [on-tick chip-move]
    [to-draw chip-draw]
    [on-key chip-key]))
}|

Thinking more about the tasks involved in the above functions, we can
identify a number of other tasks that will come up.  We will need to:

@itemlist[
@item{decrease a Chip's speed}
@item{increase a Chip's speed}
@item{change a Chip's direction to go right}
@item{change a Chip's direction to go left}
]

From this list of tasks, we can generate another wishlist of function
signatures:

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; chip-speed-down : Chip -> Chip
;; Decrease a Chip's speed
(define (chip-speed-down c) ...)

;; chip-speed-up : Chip -> Chip
;; Increase a Chip's speed
(define (chip-speed-up c) ...)

;; chip-go-right : Chip -> Chip
;; Change a Chip's direction to go right
(define (chip-go-right c) ...)

;; chip-go-left : Chip -> Chip
;; Change a Chip's direction to go left
(define (chip-go-left c) ...)
}|

We now have a pretty good design sketch.  To flesh it out, we can work
through each of the functions above, applying the design recipe for
functions to systematically solve for the code.  Let's work our way
backwards.

@ex[@elem{Examples of @racket[chip-go-left] and @racket[chip-go-right]}]{

Make four examples of using @racket[chip-go-left].  For each, write
what answer the function @emph{should} compute.  Turn each example
into a test using @racket[check-expect].  Place these tests between
the function definition and purpose statement.

Apply the same steps as problem 1 to @racket[chip-go-right].

}

@ex[@elem{Solve for @racket[chip-go-left] and @racket[chip-go-right]}]{

Using your examples to help guide you, fill out the @racket[...] in
@racket[chip-go-left] and @racket[chip-go-right] with definitions that
will work for the examples you wrote.

Run your program to confirm the correcntess of your function against
the tests you've written.

}

The @racket[chip-speed-up] function should increase the given Chip's
speed by 4 px/tick.  There's no limit on how fast a Chip can go.

@ex[@elem{Examples of @racket[chip-speed-up]}]{

Make four examples of using @racket[chip-speed-up].  For each, write
what answer the function @emph{should} compute.  Turn each example
into a test using @racket[check-expect].  Place these tests between
the function definition and purpose statement.

}

Now write the function.

@ex[@elem{Solve for @racket[chip-speed-up]}]{

Using your examples to help guide you, fill out the @racket[...] in
@racket[chip-speed-up] with definitions that will work for the
examples you wrote.

Run your program to confirm the correcntess of your function against
the tests you've written.

}

The @racket[chip-slow-down] function is slightly more involved
compared to @racket[chip-speed-up].  The @racket[chip-slow-down]
function should decrease the given Chip's speed by 4 px/tick,
@bold{but} Chip's speed should never drop below 1 px/tick.

@ex[@elem{Examples of @racket[chip-speed-down]}]{

Make four examples of using @racket[chip-speed-down].  For each, write
what answer the function @emph{should} compute.  Turn each example
into a test using @racket[check-expect].  Place these tests between
the function definition and purpose statement.

}
Now write the function.

@ex[@elem{Solve for @racket[chip-speed-down]}]{

Using your examples to help guide you, fill out the @racket[...] in
@racket[chip-speed-down] with definitions that will work for the
examples you wrote.

Run your program to confirm the correcntess of your function against
the tests you've written.

}

Now let's move further up our wishlist.

The @racket[chip-key] function is responsible for taking a Chip and a
@link["https://docs.racket-lang.org/teachpack/2htdpuniverse.html?q=keyevent#%28tech._world._keyevent%29"]{KeyEvent}
representing a key the user pressed and computing an appropriately
changed Chip.

There are 5 cases to consider:
@itemlist[
@item{the user pressed @racket["up"],}
@item{the user pressed @racket["down"],}
@item{the user pressed @racket["left"],}
@item{the user pressed @racket["right"], or}
@item{the user pressed something else.}
]

This suggests a function structure that uses @racket[cond] to
discriminate which case has occurred based on the @racket[ke] input.
(A helpful operation here is @racket[key=?], which takes two KeyEvents
and determines if they are the same.)

@ex[@elem{Examples of @racket[chip-key]}]{

Make six examples of using @racket[chip-key].  For each, write what
answer the function @emph{should} compute.  Turn each example into a
test using @racket[check-expect].  Place these tests between the
function definition and purpose statement.

}

@ex[@elem{Solve for @racket[chip-key]}]{

Using your examples to help guide you, fill out the @racket[...] in
@racket[chip-key] with definitions that will work for the
examples you wrote.

Run your program to confirm the correcntess of your function against
the tests you've written.

}

Next, do @racket[chip-move], which should change a Chip's position
based on its direction and speed.  The computed Chip's position should
reflect where Chip is after 1 tick of time.


@ex[@elem{Examples of @racket[chip-move]}]{

Make four examples of using @racket[chip-move].  For each, write what
answer the function @emph{should} compute.  Turn each example into a
test using @racket[check-expect].  Place these tests between the
function definition and purpose statement.

}

@ex[@elem{Solve for @racket[chip-move]}]{

Using your examples to help guide you, fill out the @racket[...] in
@racket[chip-move] with definitions that will work for the
examples you wrote.

Run your program to confirm the correcntess of your function against
the tests you've written.

}

The final task to consider is @racket[chip-draw].  For this function,
let's first engage in what's called @emph{iterative refinement}, a
fundamental engineering principle in which we first make a working
solution for a simplified problem.  Once in place, we can revise our
problem description to be more sophisticated and refine our simple
solution to the revised task.  We can do this successively until
arriving at a solution to our original problem.

There are a few aspects that complicate @racket[chip-draw].  The first
is that what to draw depends on the direction of Chip: if Chip is
going left, the image of Chip should face left; if Chip is going
right, the image should face right.  The second is that, in order to
make Chip appear animated, we actually choose different images of Chip
in moments of movement based on Chip's location.

Let's start by simplifying away both of these complications and
instead simply draw a circle to represent Chip.

@ex[@elem{Examples of @racket[chip-draw], version 1}]{

Make four examples of using the simplified version of
@racket[chip-draw].  For each, write what answer the function
@emph{should} compute.  Turn each example into a test using
@racket[check-expect].  Place these tests between the function
definition and purpose statement.

}

@ex[@elem{Solve for @racket[chip-draw], version 1}]{

Using your examples to help guide you, fill out the @racket[...] in
@racket[chip-draw] with definitions that will work for the
examples you wrote.

Run your program to confirm the correcntess of your function against
the tests you've written.

}

You should now have a working version of (a simplified version of) the
Chip program.  Try it out by running @racket[(main 0)].

Now let's refine @racket[chip-draw] slightly.  Let's re-introduce the
first complication: the image of Chip should reflect Chip's
directionality.  We will still ignore the complication of making Chip
appear to run.

Use the following image for Chip: @result[CHIP1].  In order to show
Chip moving in the other direction, use @racket[flip-horizontal] to
compute a right-facing Chip.


Comment out your first version of @racket[chip-draw] and redesign this
more sophisticated version.

@ex[@elem{Examples of @racket[chip-draw], version 2}]{

Make four examples of using the more sophisticated version of
@racket[chip-draw].  For each, write what answer the function
@emph{should} compute.  Turn each example into a test using
@racket[check-expect].  Place these tests between the function
definition and purpose statement.

}

@ex[@elem{Solve for @racket[chip-draw], version 2}]{

Using your examples to help guide you, fill out the @racket[...] in
@racket[chip-draw] with definitions that will work for the
examples you wrote.

Run your program to confirm the correcntess of your function against
the tests you've written.

}

Try out your refined program with @racket[(main 0)].

Finally, let's finish the exercise by removing our simplifying
assumptions.  In order to make Chip appear animated, use the following
images:
@itemlist[ 
@item{@result[CHIP1]}
@item{@result[CHIP2]}
@item{@result[CHIP3]}
@item{@result[CHIP4]}]

If Chip is running right, we can again use @racket[flip-horizontal].
In order to determine which image to use, you can use Chip's position
to select one of the four images.  The basic idea is compute the
position @emph{modulo} 4.  The @racket[modulo] operation will help
here.  Try out some examples of @racket[modulo] to get a sense of it.

We can now identify a couple small tasks that we can use to design
functions:

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; chip-choose : Natural in [0,3] -> Image
;; Given either 0, 1, 2 or 3, pick a corresponding image of (leftward) Chip
(define (chip-choose c) ...)

;; chip-image : Integer -> Image
;; Pick one a (leftward) Chip image based on given position
(define (chip-image c) ...)
}|

@ex[@elem{Design @racket[chip-choose] and @racket[chip-image]}]{

Make four examples (each) of using of @racket[chip-choose] and
@racket[chip-image].  For each, write what answer the function
@emph{should} compute.  Turn each example into a test using
@racket[check-expect].  Place these tests between the function
definitions and purpose statements.

Using your examples to help guide you, fill out the @racket[...] in
@racket[chip-choose] and @racket[chip-image] with definitions that
will work for the examples you wrote.

Run your program to confirm the correcntess of your function against
the tests you've written.

}

Now it should be pretty straightforward to finish the final version of
@racket[chip-draw].


@ex[@elem{Examples of @racket[chip-draw], final version}]{

Make four examples of using the full version of
@racket[chip-draw].  For each, write what answer the function
@emph{should} compute.  Turn each example into a test using
@racket[check-expect].  Place these tests between the function
definition and purpose statement.

}

@ex[@elem{Solve for @racket[chip-draw], final version}]{

Using your examples to help guide you, fill out the @racket[...] in
@racket[chip-draw] with definitions that will work for the
examples you wrote.

Run your program to confirm the correcntess of your function against
the tests you've written.

}

Give Chip the Cheap Sheep a shot with @racket[(main 0)].
