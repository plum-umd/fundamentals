#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner))
@(provide readings)

@title[#:style 'unnumbered #:tag "assign6"]{Assignment 6: The Lonely Pac-Man}

@bold{Due:} Wednesday, October 10, 11:59:59 PM EST.

The following should be completed in cooperation with your NEW
assigned partner. (Partner assignments are listed on
@link["https://piazza.com/class/jlc99zv3cp9pk?cid=49"]{Piazza}.)  You
may not share code for this assignment with anyone but your CURRENT
partner.

You must use the design recipe and @secref{style} guidelines to
receive full credit.

@section[#:tag "assign6:prep"]{Preparation}

@(define readings
  @elem{@emph{all} of
    @link["https://htdp.org/2018-01-06/Book/part_two.html"]{Part II}})


Make sure you have read and studied @readings of HtDP2e.

@section[#:tag "assign6:lists"]{Finger Exercises with Lists}

Create a file @tt{list.rkt} for this part of the assignment.  Add your
information to a standard file header at the top of the file.

Complete exercises 143--148 from
@link["https://htdp.org/2018-01-06/Book/part_two.html"]{HtDP2e Part
2}.


@section[#:tag "assign6:lonely-pac"]{The Lonely Pac-Man}

Create a file @tt{pac0.rkt} for this part of the assignment.  Add your
information to a standard file header at the top of the file.


In this assignment, we will create a first, very simplified version of
the venerable arcade classic
@link["https://en.wikipedia.org/wiki/Pac-Man"]{Pac-Man}.

In this simplified version of the game, there is a single Pac-Man,
which can be rendered as a yellow circle, that navigates up, down,
left, and right, on a rectangular area (the board).  On this area are
some number of pellets (black, smaller circles).  When Pac-Man comes
into contact with a pellet, it is consumed.  When all the pellets are
consumed, the level starts over with new pellets on the screen.  The
game continues until the player the "Esc" key, which ends the game and
displays the final score, which is the total number of pellets
consumed.

There are no ghost, no maze or obstacles, no warps, no power pellets,
fruit, lives, etc., as in the full version of Pac-Man.

The Pac-Man and pellets are arranged on a grid.  The width and height
of the playing area should be easy to change by changing the number of
grid units for the dimensions of the board.  It should be easy to
scale the size of the whole game by changing the size of grid units.

Pac-Man moves at a fixed speed.  Pac-Man, however, does not move a
full grid unit for every unit of time.  This means that although
Pac-Man moves on a grid, it's possible to be "in-between" grid
segments in one of it's dimensions.  For example, suppose Pac-Man is
at grid-coordinate (1,1) and moving right:

@image{img/pac-grid0.png}

At the next moment in time, Pac-Man could be between (1,1) and (2,1):

@image{img/pac-grid1.png}

Let's say the player now press the down arrow key.  Pac-Man continues
moving rightward and does not start moving down until after reaching
(2,1).  So from the state shown above, Pac-Man continues moving right
until reaching (2,1):

@image{img/pac-grid2.png}

Then (assuming the user hasn't pressed additional arrow keys), begins
moving downward:

@image{img/pac-grid3.png}

This means that you will have to keep track of the following
information about Pac-Man: it's current direction and the direction
that will become it's current direction the next time it is
grid-aligned.

It's important that the speed of Pac-Man (in pixels per unit of time)
evenly divides the size of the grid units.  So for example, you might
have Pac-Man move 10 pixels per unit of time with a grid size of 40
pixels.  This means Pac-Man is "grid-aligned" in both dimensions after
travelling in any direction for four ticks.  If, on the other hand,
you don't do this and, e.g., have Pac-Man move 12 pixels per unit of
time, Pac-Man will skip over some grid coordinates and it will
significantly complicate your code and/or cause weird effects in the
game.

Pac-Man cannot go beyond the boundaries of the board.

The game should start with some number of pellets on the board.  It
should be 2 or more, but otherwise is up to you.  When the level ends
(the pellets are consumed), you can start over with the pellets in the
same or different configuration; that's up to you.  But it should be
possible to easily change the number and position of pellets by
editing the program text.




@section[#:tag "assign6:submission"]{Project submission}

You should submit both files: @tt{list.rkt} and
@tt{pac0.rkt}.

Submit your files directly to the submit server by uploading them.
Select each of these files individually using the ``Browse'' button.
Once you have selected all the files, press the ``Submit project!''
button. You do not need to put them in a zip file.
