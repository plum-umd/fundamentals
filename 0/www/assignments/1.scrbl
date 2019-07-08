#lang scribble/manual
@(require scribble/core "../utils.rkt")
@(require (for-label (except-in lang/htdp-beginner posn) 2htdp/universe (except-in 2htdp/image image? color)))
@(define (colorize c . content)
  (elem #:style (style #f (list (color-property c)))
        content))

@title[#:style 'unnumbered #:tag "assign1"]{Assignment 1: Bug}


@bold{Due}: Friday, July 12, 11:59:59 PM EST.

@image[#:scale 1/2 #:style float-right]{img/bug-fast.gif}

In this assignment, you are tasked with designing a simplified variant
of the classic game @bold{Snake}.  In this version, which we call
@bold{Bug}, the play controls a small bug on a screen.  The bug is
able to move up, down, left, or right.  There is also a single peice
of food on the screen.  If the bug is moved to where the food is, it
eats the food, and another food appears somewhere on the screen.

The bug cannot move off the screen.  There is no end to the game.  The
bug simply moves around collecting food until the player is too bored
to continue.

You must use BSL to design and implement Bug.  It will require use of
the @racketmodname[2htdp/image] and @racketmodname[2htdp/universe]
libraries.

@section{Design sketch}

To get started, save @link["assign1.rkt"]{@tt{assign1.rkt}} to your
computer and open it in DrRacket.

This code gives a starting skeleton for the design of Bug.  Make sure
to put your name at the top of the file.

@subsection{Main}

The provided code consists of several "sections" of code marked with
lines of semicolons and section headings.  This program files a
top-down organization.  So the first section describes the
@racket[main] function, which is used to launch the Bug program.

This function is defined as:

@#reader scribble/comment-reader
(racketblock
;; main : Number -> Game
;; Launches a game of "Bug" played at given tick rate
;; Example: (main 1/10)
(define (main r)
  (big-bang G0
    [to-draw game-draw]
    [on-tick game-advance r]
    [on-key game-handle-key]))
)

This program uses the @racket[big-bang] system for making interactive
programs.  If you have not read up on using @racket[big-bang] be sure
to read the relevant sections of @emph{HtDP} and documentation.

This @racket[big-bang] program starts in an initial state @racket[G0],
whose definition we will see shortly.

There are three event handling functions.  Draw events use the
@racket[game-draw] function, tick events use the @racket[game-advance]
function, and keyboard events use the @racket[game-handle-key]
function.  You will have to design and implement these functions to
make the game.

The @racket[main] function takes a single argument: the speed
(measured in ticks per second) at which the game will play.

@subsection{Data Definitions}

The data definitions section is the most important section.  It
defines how all of the information that occurs in the Bug game will be
represented as data in BSL.  It contains the following definitions:

@#reader scribble/comment-reader
(racketblock
;; A Game is a (make-game Bug Food)
(define-struct game (bug food))

;; A Bug is a (make-bug Dir Posn)
(define-struct bug (dir posn))

;; A Food is a Posn

;; A Posn is a (make-posn Integer Integer)

;; A Dir is one of:
;; - "left"
;; - "right"
;; - "up"
;; - "down"
)

These definitions define classes of values used in the Bug program.
The first class of values are Games.  A Game is an instance of a game
structure, which has a Bug and a Food.  A Bug is an instance of a bug
structure with a Dir and a Posn.  A Food is an instance of a posn
structure with two Integers.  A Direction is an enumeration of strings
representing a direction.

These definition will be used throughtout the signatures in the rest
of program and will drive the design of our functions.

@subsection{Defined Constants}

The defined constants gives names (by convention, we use uppercase for
constant names) to values that will be used repeatedly in the rest of
the code. 

The first three constants concern the visual rendering of the Bug
game.  The game is played on a @racket[WIDTH] by @racket[HEIGHT] grid.
Here we're saying it's a @math{20 x 20} grid.  Each unit of the grid
measures @racket[PX/U] pixels; in this case @math{20}.

@#reader scribble/comment-reader
(racketblock
(define PX/U 20) ; pixels per unit
(define WIDTH 20)  ; units
(define HEIGHT 20) ; units
)


Changing the definition of these constants will change the sizing of
the game.  For example, changing @racket[PX/U] to @racket[10] will
cause the screen to shrink by half.  The game will still logically
play on a @math{20 x 20} grid, but it will just be scaled down.  On
the other hand, changing @racket[WIDTH] to @racket[100] will change
the game to play on a @math{100 x 20} grid.

The main motivation for making defined constants is so that there is a
single point of control for these parameters of the program.  A
well-designed program can have constants change and continue to work
as expected.

The next defined constant is @racket[G0], the initial Game state used
in the @racket[main] function.  It creates an instance of a Game with
a Bug located in the center of the board, moving up, and a Food placed
at position @math{(0,0)}.

@#reader scribble/comment-reader
(racketblock
(define G0 ; an initial game state
  (make-game (make-bug "up"
                       (make-posn (quotient WIDTH 2)
                                  (quotient HEIGHT 2)))
             (make-posn 0 0)))
)

@subsection{Game functions}

This section contains the functions relevant to computing with data
represented as a Game.

There were three functions used in the @racket[main] function and they
are stubbed out here:

@#reader scribble/comment-reader
(racketblock
;; game-handle-key : Game KeyEvent -> Game
;; Handle a key event in this game
(define (game-handle-key g ke)
  ; stub
  g)

;; game-advance : Game -> Game
;; Adance the bug, maybe eating the food
(define (game-advance g)
  ; stub
  g)

;; game-draw : Game -> Scene
;; Render the game as a scene
(define (game-draw g)
  ; stub
  (empty-scene (* WIDTH PX/U)
               (* HEIGHT PX/U)))
)

You will need to design these functions.  Each of them will likely
require breaking things down into smaller problems to solve.  For each
of those subproblems, you will need to design a function.  Those
functions should be added to sections of the program that contain all
the functions relevant to the same kind of data (e.g. Bug functions
should be in a section together, Posn function should be in a section
together, etc.)

@subsection{Bug functions}

This section contains functions relevant to bugs.  A couple of
functions that you @emph{may} find useful are stubbed here.

@#reader scribble/comment-reader
(racketblock
;; bug-change-dir : Bug Dir -> Bug
;; Change bug's direction to given one
(define (bug-change-dir b d)
  ; stub
  b)

;; bug-advance : Bug -> Bug
;; Advance bug in its current direction, but not past board boundaries
(define (bug-advance b)
  ; stub
  b)
)

@subsection{Posn functions}

This section contains functions relevant to posns.  Several functions
that you may find useful are stubbed here.

@#reader scribble/comment-reader
(racketblock
;; posn-advance : Posn Dir -> Posn
;; Advance the posn in given direction, but not past boundaries
(define (posn-advance p d)
  ; stub
  p)

;; posn-advance-left : Posn -> Posn
;; Advance the posn toward left, but not past left boundary
(define (posn-advance-left p)
  ; stub
  p)

;; posn-advance-right : Posn -> Posn
;; Advance the posn toward right, but not past right boundary
(define (posn-advance-right p)
  ; stub
  p)

;; posn-advance-up : Posn -> Posn
;; Advance the posn toward top, but not past top boundary
(define (posn-advance-up p)
  ; stub
  p)

;; posn-advance-down : Posn -> Posn
;; Advance the posn toward bottom, but not past bottom boundary
(define (posn-advance-down p)
  ; stub
  p)

;; posn=? : Posn Posn -> Boolean
;; Are the two posns at the same position?
(define (posn=? p1 p2)
  ; stub
  #false)
)

Finally, a function is given for drawing a circle of a given color at
a grid position on a scene:

@#reader scribble/comment-reader
(racketblock
;; posn-draw-on : Posn Color Scene -> Scene
;; Draw a colored circled at given posn on scene
(define (posn-draw-on p color scn)
  (place-image (circle (* 1/2 PX/U) "solid" color)
               (+ (* (posn-x p) PX/U) (* 1/2 PX/U))
               (+ (* (posn-y p) PX/U) (* 1/2 PX/U))
               scn))
)

You can use this (if it's helpful) in the @racket[game-draw] function,
or anytime you want to visualize where a position on the grid should
appear.  Try it out.

@section{How to approach}

Despite being a (painfully) simple game, solving this assignment will
take some time and effort.  Be sure to finish the first week of
readings from @emph{HtDP} on the course schedule.  This will be the
biggest help in being able to successfully complete this assignment.

Parts of this assignment will require knowledge of topics that will
only occur toward the end of the week.  Try to keep this assignment in
mind as we work through material in lecture.

You can probably complete the stubbed Posn functions early.  So you
might start there (after Tuesday's lecture).

If you're feeling lost, spend time with a TA or post to the discussion
forum on ELMS.  Don't stay stuck.

@section{Submit}

Submit your @tt{assign1.rkt} file to assignment 1 on ELMS.  You can
submit early and often.  We will grade the last submission before the
deadline.

