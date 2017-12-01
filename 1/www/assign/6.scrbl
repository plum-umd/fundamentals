#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner))

@title[#:style 'unnumbered #:tag "assign6"]{Assignment 6: Invasion!}

@bold{Due:} Friday, October 13, 11:59:59 PM EST.

The following should be completed in cooperation with your latest assigned
partner. (Partner assignments are listed on
@link["https://piazza.com/class/j474gwnsd3619n?cid=43"]{Piazza}.)  You may not
share code for this assignment with anyone but your partner.

You must use the design recipe and @secref{style} guidelines to
receive full credit.

We've created an assignment skeleton @link["assign6.zip"]{assign6.zip} for you
to edit, please download the archive and edit the files you find inside. You
@emph{must not} change any file names.

@section[#:tag "assign6:prep"]{Preparation}

Make sure you have read and studied @emph{all} of
@link["http://www.ccs.neu.edu/home/matthias/HtDP2e/part_two.html"]{Part II} of
HtDP2e.

@section[#:tag "assign6:lists"]{Finger Exercises with Lists}

Edit the file @tt{list.rkt} for this part of the assignment. Add your
information to the standard file header at the top of the file.

Complete exercises 143--148 from
@link["http://www.ccs.neu.edu/home/matthias/HtDP2e/part_two.html"]{HtDP2e Part
2}.


@section[#:tag "assign6:shots"]{The Invader Shoots Back}

Copy your code from the @tt{invader-shots.rkt} in assignment 5 into the provided
file @tt{invaders-shoot.rkt} for this part of the assignment. Edit the standard
file header at the top of the file. @bold{Submit only @tt{invaders-shoot.rkt}
for this part of the assignment, not the original @tt{invader-shots.rkt}.}

The fight between your base and the invader is a bit one-sided at this
point. It's far more sporting to let the invader shoot back.

Extend your program such that the invader shoots down toward the base.

@itemlist[
  @item{The invader shoots at a regular interval every N ticks of the
        clock. (Note: if N is small, several shots may be present on the screen
        at once.)}
  @item{Each shot that hits the base costs 1 life.}
  @item{The base starts with 3 lives.}
  @item{The game ends when the base has no lives yet.}
]

Some questions to consider:
@itemlist[
  @item{How does the game state that @racket[big-bang] operates on need
        to change so the invader can fire arbitrarily many shots?}
  @item{Which functions operate on the game state? How do they need to change to
        account for an invader that fires back?}
  @item{@bold{Hint}: Create a TODO list of all functions that need to
        change, then start working your way through that list.}
]


@section[#:tag "assign6:invaders"]{Many Invaders}

Continue this section in the same file as the last section
(@tt{invaders-shoot.rkt}).

Our TAs complain that the single-invader version of the game will be too easy
for them. Modify your game to support a single row of an arbitrary number of
invaders that can all shoot at the base.

@bold{Note:}
@itemlist[

  @item{It should be easy to modify the program to change the number of invaders
        in the row.}

  @item{The whole row should move horizontally along the scene. Once the row
        meets the far left or the far right of the screen, the entire row should move
        down.}

  @item{Knocking off the left-most or right-most invader doesn't change how far
        left and right the row moves.}

  @item{Each invader can shoot at its own regular interval. These intervals should
        vary invader-to-invader to make the game interesting.}

]

Some questions to consider:
@itemlist[
  @item{How does the game state that @racket[big-bang] operates on need
        to change so the invader can fire arbitrarily many shots?}
  @item{Which functions operate on the game state? How do they need to change to
        account for multiple invaders rather than a single row?}
]


@section[#:tag "assign6:submission"]{Project submission}

You should submit both of the provided files: @tt{list.rkt} and
@tt{invaders-shoot.rkt}.

Submit your files directly to the submit server by uploading them.
Select each of these files individually using the ``Browse'' button.
Once you have selected all the files, press the ``Submit project!''
button. You do not need to put them in a zip file.
