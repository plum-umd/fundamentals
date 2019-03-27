#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner 2htdp/universe) "helper.rkt")
@(require "../utils.rkt")

@lab-title[14]{State in Worlds}

@section[#:style 'unnumbered #:tag "lab14:intro"]{Intro}

You'll work in this lab with your
@link["https://piazza.com/class/jcspfhewmdn41y?cid=108"]{lab partners}.

The two of you will work as a team to solve problems. At any time, one
of you will be the @bold{Head} and the other will be the
@bold{Hands}. The @bold{Head} does the thinking and the @bold{Hands}
does the typing. @bold{Hands} type only what the @bold{Head} tells
them to, but you're free to discuss any issues that pop up. You should
switch off during the lab to make sure each of you get practice
problem solving, dealing with syntax, and getting finger exercises on
the keyboard.

You can start this lab with @link["Lab15.zip"]{this project skeleton}
or your own completed implementation of @labref{9}.


@section[#:style 'unnumbered #:tag "lab14:recall"]{Recall}

In @labref{9} we made a simple game with a bouncy ball.

We made that game with the
@link["https://course.ccs.neu.edu/cs2510sp17/image-doc.html#%28part._.Images%29"]{@tt{javalib.worldimages}}
and
@link["https://course.ccs.neu.edu/cs2510sp17/image-doc.html#%28part._.Worlds%29"]{@tt{javalib.funworld}}
libraries. The functional worlds provided by @tt{javalib.funworld}
allow us to program much like we did with @racket[big-bang] from the
student languages last semester.

In this lab we'll use @tt{javalib.impworld}, where we change the world
not by creating new worlds from scratch but by @emph{mutating} the
already present world.


@section[#:style 'unnumbered #:tag "lab14:impworld"]{Imperative Worlds}

We've provided you a working copy of the bouncy ball game. Once you've
opened the project skeleton (or made a copy of your own lab 9), import
the @tt{impworld} instead of @tt{funworld}.

@verbatim|{
import javalib.impworld.*;  // previously: import javalib.funworld.*
}|

Your IDE will highlight a number of errors now present due to the
mismatches between the imperative and functional worlds.

The API changes you need to focus on are as follows:

@itemlist[

  @item{The @tt{World} methods @tt{onKeyEvent}, @tt{onTick}, and
        @tt{endOfWorld} must mutate the existing @tt{World} and return
        nothing (@tt{void}), rather than return a new @tt{World}.}

  @item{The @tt{WorldScene} method @tt{placeImageXY} now mutates the
        scene, returning nothing (@tt{void}).}

]

Make sure the order in which operations are performed doesn't
change. Specifically, the bouncy ball should always be drawn on top of
all other features in the game (including the score, enemies).
