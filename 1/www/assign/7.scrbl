#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner))
@(provide readings)

@title[#:style 'unnumbered #:tag "assign7"]{Assignment 7: The Lonely, Obstructed PacMan}

@bold{Due:} Monday, October 24, 11:59:59 PM EST.

The following should be completed in cooperation with your latest assigned
partner. (Partner assignments are listed on
@link["https://piazza.com/class/jlc99zv3cp9pk?cid=49"]{Piazza}.)  You may not
share code for this assignment with anyone but your partner.

You must use the design recipe and @secref{style} guidelines to
receive full credit.

@section[#:tag "assign7:prep"]{Preparation}

@(define readings
  @elem{Chapters 14, 15, and 16 of
     @link["https://htdp.org/2018-01-06/Book/part_three.html"]{Part III}})

Make sure you have read and studied @readings of HtDP2e.

@section[#:tag "assign7:lists"]{Finger Exercises with Abstraction}

Edit the file @tt{abs.rkt} for this part of the assignment. Add your
information to the standard file header at the top of the file.

Complete exercises 250--255 from
@link["https://htdp.org/2018-01-06/Book/part_three.html"]{HtDP2e
Part 3}.


@section[#:tag "assign7:pacman-abs"]{Lonely PacMan with Abstractions}

Revise the Lonely PacMan game from assignment 6 to add obstacles.  You
may either start from your assignment 6 solution or a canonical
solution (@link["pac0-soln.rkt"]{@tt{pac0-soln.rkt}}).  Rename your
file to @tt{pac1.rkt}.

Keeping exactly the same functionality as in assignment 6, rewrite
your program in ISL using the list abstraction functions we've seen so
far, such as @tt{filter}, @tt{map}, @tt{foldr}, @tt{andmap}, and
@tt{ormap}, wherever appropriate.

@section[#:tag "assign7:pacman-obs"]{Lonely PacMan with Obstructions}

Continue this section in the same file as the last section.

The Lonely PacMan is not so fun.  Revise your program to add
obstacles.  An obstacle occupies a single grid position and must be
grid-aligned; prevents pacman from occupying that position.
Obstacles are centered on grid positions and have a size of 1 square
grid.  No part of PacMan can occupy any space occupied by an obstacle.

For example, here is a PacMan at (1,1) and obstacle at (2,1), with the
grid drawn for demonstrative purposes:

@image{img/pac-obs1.png}

In this configuration, if PacMan were moving right, he'd be stuck and
wouldn't move until his direction was changed.

Here's an example with obstacles at (2,1), (0,1), (1,0), and (1,2) and
PacMan at (1,1).  This configuration totally traps PacMan:

@image{img/pac-obs2.png}

There can be any number of obstacles in the game and your game should
be written in such a way that it is easy to revise the game to add &
remove obstacles.

With enough obstacles, you can make mazes that look like the real
PacMan game.  You don't need to produce mazes like this, but it should
be easy to set the defined constants in your game to do this:

@image{img/pac-maze1.png}

(If you'd like the data for this maze, you can use
@link["maze-data.rkt"]{@tt{maze-data.rkt}}.)

Note that all other functionality of the game should remain the same.
PacMan can't go over the boundaries of the board.  Pressing "escape"
ends the game and shows the score, etc.

@section[#:tag "assign7:submission"]{Project submission}

You should submit both of the files: @tt{abs.rkt} and
@tt{pac1.rkt}.

Submit your files directly to the submit server by uploading them.
Select each of these files individually using the ``Browse'' button.
Once you have selected all the files, press the ``Submit project!''
button. You do not need to put them in a zip file.
