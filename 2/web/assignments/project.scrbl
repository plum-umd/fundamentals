;; Dumping ground for text from year 2 project.

@section{Controlling nested worlds}

We're now going to enhance our side by side nested worlds from
@seclink["assign07"]{Assignment 6} with more features.   

This is phrased as a list of features, but you only need to turn in one
implementation of the whole system.  

@subsection{Multiple tick rates}

Real @tt{world}s can have different tick rates from each other.  Extend your
side-by-side worlds to support the different tick rates that the nested worlds
may have, as expressed by their @racket[tick-rate] method.  Hint: the
@racket[lcm] function will be useful here.  

@subsection{Pause and quit buttons}

Add a pause button below each of the two nested worlds.  When a world is
paused, it should not receive @emph{any} events---no mouse, keyboard, or tick
events.  Clicking the pause button again should un-pause the world.

Add a quit button for the whole system, and remove the special handling of the
@racket["q"] key.

@subsection{"Active" worlds}

In your original design, every key event went to @emph{both} worlds.  Change
this so that your system has one @emph{active} world at all times.  Only the
active world should receive key events, but both worlds recieve mouse events
and tick events (unless they are paused).

Clicking on a world makes it active.  The world on the left should start as the
active world.  The click that makes a world active should @emph{not} be
provided as a mouse event to the world that is made active.

@subsection{Mouse enter/leave events}

In your original design, the nested worlds never received enter or leave mouse
events.  Revise your system to provide these events whenever the previous mouse
event was over a given world, and the new mouse event is not (or vice versa for
enter events).  

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




