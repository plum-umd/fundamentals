#lang scribble/manual
@(require scribble/core)
@(require "../utils.rkt")
@(define (colorize c . content)
  (elem #:style (style #f (list (color-property c)))
        content))

@assn-title[2]{The Object of the Game}

This is assignment is to be completed and submitted with your
@link["https://piazza.com/class/jcspfhewmdn41y?cid=27"]{partner}.  You
may not work with anyone other than your assigned partner.

@bold{Due}: Tuesday, February 13, 11:59:59 PM EST.

For this assignment, you must re-write a significant @racket[big-bang]
game from last semester to use the @racket[class/0] language.  For
full credit, your solution should be designed using classes and
objects.

The game you will re-design is the simplified Space Invaders game as
written by Austin and David.  The code is
@link["http://www.cs.umd.edu/class/fall2017/cmsc131A/invader-shots-dvanhorn-abourg.rkt"]{here}.
The code should be all you need, but there is a
@link["http://www.cs.umd.edu/class/fall2017/cmsc131A/Notes.html#%28part._.Pair_programming_.Space_.Invaders_with_shots%29"]{video}
of the code being written should you find it useful.

Here are a few suggestions:
@itemlist[
@item{You should use the @racket[class/universe] library instead of @racket[2htdp/universe].}

@item{You do not need to implement any of the "main" functions other
than @racket[game-main] (although implementing the others may be
helpful to try out simplified versions of the game on your way to a
complete solution).}

@item{Your final code should not have any structure or function
definitions and should intead have class definitions.}

@item{You will need to re-locate some constant definitions.  If they
involve calling constructors, they must appear below the relevant
class definition.  (But you can use these constant definitions within
any method or test cases that appear in a class definition, even if
above the constant definition.)}

@item{For large test cases, you may write them outside of the class
definition and put tests in their own section.}

@item{When porting functions to methods, you may rename the methods so
that they work with @racket[big-bang].}

]


@section[#:style 'unnumbered #:tag "assign1:submit"]{Submission}

Use @tt{submit.cs.umd.edu} to submit your solution as a single file
called @tt{assign2.rkt}.  Either partner may submit a solution and we
do not need both partners to submit.  We will grade the latest
submission by either partner that is submitted before the deadline.
