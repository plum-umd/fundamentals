#lang scribble/manual
@(require scribble/core "../utils.rkt")
@(require (for-label (except-in lang/htdp-beginner posn) 2htdp/universe (except-in 2htdp/image image? color)))
@(define (colorize c . content)
  (elem #:style (style #f (list (color-property c)))
        content))

@title[#:style 'unnumbered #:tag "assign2"]{Assignment 2: Snek}


@bold{Due}: Sunday, July 21, 11:59:59 PM EST.

@;image[#:scale 1/2 #:style float-right]{img/bug-fast.gif}

In this assignment, you are tasked with designing a variant of the
classic game @bold{Snake}.  In this version, which we call
@bold{Snek}, the player controls a small snake on a screen.  The snake
is able to move up, down, left, or right.  There is also a single
peice of food on the screen.  If the snake is moved to where the food
is, it eats the food, grows one segment longer, and another food
appears somewhere on the screen.

The snake dies and the game is over if it ever crosses the boundaries
of the screen or collides with itself.

You must use BSL to design and implement Snek.  It will require use of
the @racketmodname[2htdp/image] and @racketmodname[2htdp/universe]
libraries.

@section[#:tag-prefix "assign2"]{Design sketch}

To get started, save a copy of your solution to @secref{assign1} as
@tt{assign2.rkt} and open it in DrRacket.  You will need to adapt your
Bug program as follows.

@subsection[#:tag-prefix "assign2"]{Main}

The provided code consists of several "sections" of code marked with
lines of semicolons and section headings.  This program files a
top-down organization.  So the first section describes the
@racket[main] function, which is used to launch the Snek program.

This function is defined as:

@#reader scribble/comment-reader
(racketblock
;; main : Number -> Game
;; Launches a game of "Snek" played at given tick rate
;; Example: (main 1/10)
(define (main r)
  (big-bang G0
    [to-draw game-draw]
    [on-tick game-advance r]
    [on-key game-handle-key]
    [stop-when game-over?]))
)

This @racket[big-bang] program starts in an initial state @racket[G0],
whose definition we will see shortly.

There are three event handling functions.  Draw events use the
@racket[game-draw] function, tick events use the @racket[game-advance]
function, and keyboard events use the @racket[game-handle-key]
function.  You will have to design and implement these functions to
make the game.

The @racket[main] function takes a single argument: the speed
(measured in ticks per second) at which the game will play.

@subsection[#:tag-prefix "assign2"]{Data Definitions}

The data definitions section is the most important section.  It
defines how all of the information that occurs in the Bug game will be
represented as data in BSL.  It contains the following definitions:

@#reader scribble/comment-reader
(racketblock
;; A Game is a (make-game Snake Food)
(define-struct game (snake food))

;; A Snake is a (make-snake Dir Posn LoPosn)
(define-struct snake (dir head tail))

;; A LoPosn is one of:
;; - '()
;; - (cons Posn LoPosn)

;; A Food is a Posn

;; A Posn is a (make-posn Integer Integer)

;; A Dir is one of:
;; - "left"
;; - "right"
;; - "up"
;; - "down"
)

These definitions define classes of values used in the Snake program.
The first class of values are Games.  A Game is an instance of a game
structure, which has a Snake and a Food.  A Snake is an instance of a
snake structure with a Dir, representing the direction the snake is
moving, and a Posn for the head segment of the snake, and a list of
Posns for the tail (or body) of the snake.  Just like in Bug, a Food
is an instance of a posn structure with two Integers.  A Dir is an
enumeration of strings representing a direction.

These definition will be used throughtout the signatures in the rest
of program and will drive the design of our functions.

@subsection[#:tag-prefix "assign2"]{Defined Constants}

The defined constants gives names (by convention, we use uppercase for
constant names) to values that will be used repeatedly in the rest of
the code. These do not need to change from Bug.

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
a Snake located in the center of the board, moving up, and a Food
placed at position @math{(0,0)}.  Initially, the snake has no tail
(it's just a head segment), but will grow over time as the snake eats
food.

@#reader scribble/comment-reader
(racketblock
(define G0 ; an initial game state
  (make-game (make-snake "up"
                         (make-posn (quotient WIDTH 2)
                                    (quotient HEIGHT 2))
                         '())
             (make-posn 0 0)))
)

@subsection[#:tag-prefix "assign2"]{Game functions}

This section contains the functions relevant to computing with data
represented as a Game.

There were three functions used in the @racket[main] function and they
are stubbed out here.  These functions will need to be adapted from
Bug to work for the new data in Snek:

@#reader scribble/comment-reader
(racketblock
;; game-handle-key : Game KeyEvent -> Game
;; Handle a key event in this game
(define (game-handle-key g ke)
  ; stub
  g)

;; game-advance : Game -> Game
;; Adance the snake, maybe eating the food
(define (game-advance g)
  ; stub
  g)

;; game-draw : Game -> Scene
;; Render the game as a scene
(define (game-draw g)
  ; stub
  (empty-scene (* WIDTH PX/U)
               (* HEIGHT PX/U)))

;; game-over? : Game -> Boolean
;; Has the snake hit itself or gone off the screen?
(define (game-over? g)
  ; stub
  #false)
)

You will need to design these functions.  Each of them will likely
require breaking things down into smaller problems to solve.  For each
of those subproblems, you will need to design a function.  Those
functions should be added to sections of the program that contain all
the functions relevant to the same kind of data (e.g. Bug functions
should be in a section together, Posn function should be in a section
together, etc.)

@subsection[#:tag-prefix "assign2"]{Snake functions}

This section contains functions relevant to snakes.  A couple of
functions that you @emph{may} find useful are stubbed here.

@#reader scribble/comment-reader
(racketblock
;; snake-change-dir : Snake Dir -> Snake
;; Change snake's direction to given one
(define (snake-change-dir s d)
  ; stub
  s)

;; snake-advance : Snake -> Snake
;; Advance bug in its current direction
(define (snake-advance s)
  ; stub
  s)

;; snake-advance&grow : Snake -> Snake
;; Advance bug in its current direction and grow by one segment
(define (snake-advance&grow s)
  ; stub
  s)

;; snake-self-colliding? : Snake -> Boolean
;; Is the snake colliding with itself?
(define (snake-self-colliding? s)
  ; stub
  #false)

;; snake-wall-colliding? : Snake -> Boolean
;; Has the snake gone past the boundaries of the screen?
(define (snake-wall-colliding? s)
  ; stub
  #false)
)

The movement and growing of a snake is a bit subtle.  Here's how it
should work.  If the snake's head is currently over the food and a
tick happens, the food should be consumed, appear somewhere else, and
the snake should advance and grow.  If the snake's head is not over
food, then it simply advances (but does not grow).

The way to advance a snake is generate a new head based on advancing
the posn of where the current head is (in the direction the snake is
going).  The current head should become the first segment of the tail,
and the last segment of the tail should be removed.  (Try to make
examples on paper to convince yourself that this works.)  You do
@emph{not} have to advance every segment in a snake.  It won't work.
Instead you add something new at the front and drop something old at
the end and @emph{voilà} the snake moves.

Advancing and growing a snake works exactly the same way except you
don't drop the last segment of the tail!  (Again, make examples on
paper to get a feel for how it works.)

This does suggest the need for the following helper function that
works on @bold{non-empty} lists of @tt{Posn}s:

@#reader scribble/comment-reader
(racketblock
;; NeLoPosn -> LoPosn
;; Drop the last element of the given list
(define (drop-last ps)
  ; stub
  ps)
)

Note that to help you with this, consider a data defintion for
non-empty lists of @tt{Posn}s:

@#reader scribble/comment-reader
(racketblock
;; A NeLoPosn is one of:
;; - (cons Posn '())
;; - (cons Posn NeLoPosn)
)

A template for functions that consume @tt{NeLoPosn} is:

@#reader scribble/comment-reader
(racketblock
(define (neloposn-template ps)
  (cond [(empty? (rest ps)) (... (first ps) ...)]
        [(cons? (rest ps))
         (... (first ps)
              (neloposn-template (rest ps))
              ...)]))
)

Use this template to help you write @racket[drop-last].


@subsection[#:tag-prefix "assign2"]{Posn functions}

You will probably want to keep all your Posn functions from Bug.

@section[#:tag-prefix "assign2"]{How to approach}

You will need to design all of the pieces above and put them together
to make Snek.  Use the design recipe to help you.  Templates for list
functions are particularly helpful.  Make examples.  Write tests.


@section[#:tag-prefix "assign2"]{Submit}

Submit your @tt{assign2.rkt} file to assignment 2 on ELMS.  You can
submit early and often.  We will grade the last submission before the
deadline.

