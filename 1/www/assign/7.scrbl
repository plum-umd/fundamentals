#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner))
@(provide readings)

@title[#:style 'unnumbered #:tag "assign7"]{Assignment 7: Abstract Invasion!}

@bold{Due:} Monday, October 23, 11:59:59 PM EST. (@bold{extended!})

The following should be completed in cooperation with your latest assigned
partner. (Partner assignments are listed on
@link["https://piazza.com/class/j474gwnsd3619n?cid=43"]{Piazza}.)  You may not
share code for this assignment with anyone but your partner.

You must use the design recipe and @secref{style} guidelines to
receive full credit.

We've created an assignment skeleton @link["assign7.zip"]{assign7.zip}
for you to edit, please download the archive and edit the files you
find inside. You @emph{must not} change any file names -- @bold{we
will only grade the files named in the skeleton}.

@section[#:tag "assign7:prep"]{Preparation}

@(define readings
  @elem{Chapters 14, 15, and 16 of
     @link["http://www.ccs.neu.edu/home/matthias/HtDP2e/part_three.html"]{Part III}})

Make sure you have read and studied @readings of HtDP2e.

@section[#:tag "assign7:lists"]{Finger Exercises with Abstraction}

Edit the file @tt{abs.rkt} for this part of the assignment. Add your
information to the standard file header at the top of the file.

Complete exercises 250--255 from
@link["http://www.ccs.neu.edu/home/matthias/HtDP2e/part_three.html"]{HtDP2e
Part 3}.


@section[#:tag "assign7:shots"]{Abstract invader}

Copy your code from the @tt{invaders-shoot.rkt} in assignment 6 into the provided
file @tt{invaders-abs.rkt} for this part of the assignment. Edit the standard
file header at the top of the file. @bold{Submit only @tt{invaders-abs.rkt}
for this part of the assignment, not the original @tt{invader-shoot.rkt}.}

@bold{Update: you may start from a canonical solution to assignment 6
if you'd prefer:
@link["invaders-shoot-dvanhorn-abourg.rkt"]{invaders-shoot-dvanhorn-abourg.rkt}.}


Keeping exactly the same functionality as in assignment 6, rewrite
your program in ISL using the list abstraction functions we've seen so
far, such as @tt{filter}, @tt{map}, @tt{foldr}, @tt{andmap}, and
@tt{ormap}.


@section[#:tag "assign7:invaders"]{Many More Invaders}

Continue this section in the same file as the last section
(@tt{invaders-abs.rkt}).

Our TAs still complain that the single-row-of-invaders version of the game is too easy
for them. Modify your game to support multiple rows of an arbitrary number of
invaders that can all shoot at the base.

@bold{Note:}
@itemlist[

  @item{It should be easy to modify the program to change the number of rows of invaders.}

  @item{Only the bottom row of invaders can shoot at the base.}

  @item{Once the bottom row has been eliminated, the next row up becomes the bottom (and can start shooting).}
  
  @item{Each invader can shoot at its own regular interval. These intervals should
        vary invader-to-invader to make the game interesting.}

]

Your final version should also use list abstraction functions wherever
appropriate (but you may find it easier to first design the enhanced
game without them).

@section[#:tag "assign7:submission"]{Project submission}

You should submit both of the provided files: @tt{abs.rkt} and
@tt{invaders-abs.rkt}.

Submit your files directly to the submit server by uploading them.
Select each of these files individually using the ``Browse'' button.
Once you have selected all the files, press the ``Submit project!''
button. You do not need to put them in a zip file.
