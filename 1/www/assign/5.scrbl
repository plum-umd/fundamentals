#lang scribble/manual
@(provide readings)

@title[#:style 'unnumbered #:tag "assign5"]{Assignment 5: Many, Many, Many}

@bold{Due:} Friday, September 29, 11:59:59 PM EST.

@bold{ATTENTION, ATTENTION, ATTENTION}: The following should be
completed in cooperation with your assigned partner from lab
7. (Partner assignments are listed on
@link["https://piazza.com/class/j474gwnsd3619n?cid=43"]{Piazza}.)  You
may not share code for this assignment with your previous partner.

You must use the design recipe and @secref{style} guidelines to
receive full credit.

@section[#:tag "assign5:prep"]{Preparation}

@(define readings
  @elem{chapters 9 and 10 of
    @link["https://htdp.org/2018-01-06/Book/part_two.html"]{Part II}})

Make sure you have read and studied @readings of HtDP2e.

@section[#:tag "assign5:lists"]{Many Numbers, Many Shapes}

Create a file @tt{list.rkt} for this part of the assignment.  Use
the standard file header at the top of the file.

You may list your solutions in the order given here.

@bold{Exercise 1} Develop a data definition @emph{ManyNumbers} which can store
arbitrarily many numbers. Note, you have to develop your own data definition,
you may only use built-in primitive types and you may not use any existing built-in
data structures, you have to design your own data structures.

@bold{Exercise 2} Develop a data and structure definition for storing pairs of
numbers representing @emph{x} and @emph{y} coordinates.

@bold{Exercise 3} Develop a data definition @emph{ManyPairs} which can store
arbitrarily many pairs of numbers using the data definition from the previous
exercise.

@bold{Exercise 4} Write the templates for all of the three data definitions you
have defined.

@bold{Exercise 5} Design a function, which, given @emph{ManyPairs},
draws all of the points as circles with radius 10 on the same empty scene,
producing the resulting image.

@bold{Exercise 6} Design a function, which, given @emph{ManyPairs} and two
numbers for @emph{x} and @emph{y} coordinates, creates a larger
@emph{ManyPairs} with all of the coordinates plus the new pair of coordinates.

@bold{Exercise 7} Using the functions you developed in exercise 5 and 6, design
a @racket[big-bang] program which places a circle on @emph{every} mouse event
at the location of the mouse event. 


@section[#:tag "assign5:shots"]{Shooting Shots}

Create a file @tt{invader-shots.rkt} for this part of the assignment.
Use the standard file header at the top of the file.

Starting from the simplified Space Invaders game
(@link["invaders.rkt"]{invaders.rkt}), develop a @emph{Shots} data
definition for representing an arbitrary number of @emph{Shot}s.

Design a function @tt{any-shots-hit-invader?} that takes a
@emph{Shots} and an invader and produces true if any shot in the
collection hits the invader.  You may find @tt{shot-hits-invader?}
useful when defining this function.

Design a function @tt{remove-all-over-top} that takes a @emph{Shots}
and removes all of the shots that have gone over the top of the
screen.  You may find @tt{shot-over-top?} useful when defining this
function.

Design a function @tt{shots-tock} that takes a @emph{Shots} and
advances each shot upward and eliminates any shot that has gone over
the top of the screen.  You may find @tt{shot-tock} useful when
defining this function.

Design a function @tt{shots-draw-on} that draws all of the given shots
on the given scene.  You may find @tt{shot-draw-on} useful when
defining this function.

Now redesign the simplified Space Invaders game to allow the player to
shoot as many shots as they'd like.  The representation of a
@emph{Game} can be changed to the following (note there is no longer a
need to represent two distinct states for "aiming" and "firing"):

@#reader scribble/comment-reader (racketblock
;; A Game is a (make-game Invader Base Shots)
(define-struct game (inv base shots))
;; Interp: game state with an arbitrary number of shots fired.
)

Define a @tt{main} function that consumes an integer @tt{i} and
launches Space Invaders game with the invader in the upper right
corner, the base at position @tt{i}, and no shots fired.

Redesign @tt{game-draw}, @tt{game-tock}, @tt{game-key}, and
@tt{game-over?} and any appropriate helper functions so that a shot is
fired any time the player presses "space".  The invader should reset
to the top left any time a shot hits it.  The game is over if the
invader reaches the bottom of the scene.


@section[#:tag "assign5:submission"]{Project submission}

You should submit all files: @tt{list.rkt} and @tt{invader-shots.rkt}.

Submit your files directly to the submit server by uploading them.
Select each of these files individually using the ``Browse'' button.
Once you have selected all the files, press the ``Submit project!''
button. You do not need to put them in a zip file.
