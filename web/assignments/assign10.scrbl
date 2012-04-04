#lang scribble/manual
@(require "../utils.rkt"
	  "../unnumbered.rkt"
          (for-label (except-in class/2 empty cons first rest list-ref length e check-expect))
          (for-label (only-in lang/htdp-intermediate-lambda check-expect))
	  (for-label class/universe))

@title[#:tag "assign11"]{4/11: Launching and quitting programs}

Due: 4/11.  Language: @racketmodname[class/2].

Note: even though this is formatted as three problems, you only need to submit
one completed program.


@section{Movable windows}

Again, we'll be revising our multi-world operating system from the
@seclink["assign10"]{previous assignment}.  

@margin-note{@bold{A reminder}: your outer world (that is, the program you write in this
assignment) must work for @emph{any} world nested within.  That means you can't
add features to inner worlds to maintain special information, or anything like
that.}

First, we're going to break the static layout that our programs have been locked in.  This takes two steps:
@itemlist[
@item{Add a title bar to each inner world.}
@item{Allow the window to be dragged by its title bar to anywhere inside the outer window.}
]

Clicking the title bar, like clicking the window, should make that world the
active world.  When two worlds overlap, the active one should be on top.  The
pause button for a world should move around with it.

@section{Launching more worlds}

So far, we've always had exactly two nested worlds.  That's kind of boring.
Instead, revise your system as follows.  It should have two buttons at the
bottom, each of which launches the initial world provided at the start.
Clicking the buttons more times launches more worlds.  Each world should start
as the active world (and thus at the front of the display), and immediately
start receiving tick events.

@section{Quitting worlds}

Now that we have lots of worlds, maybe some of them need to go away.  Add a
quit button to each nested world.  Quitting a world should make it disappear
entirely.




