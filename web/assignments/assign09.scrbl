#lang scribble/manual
@(require "../utils.rkt"
	  "../unnumbered.rkt"
          (for-label (except-in class/2 empty cons first rest list-ref length e check-expect))
          (for-label (only-in lang/htdp-intermediate-lambda check-expect))
	  (for-label class/universe))

@title[#:tag "assign10"]{3/30: More Java, and process control}

Due: 3/30.  Language: Java and @racketmodname[class/2].

@section{Finishing Iterators}

Complete exercises 1 through 9 on @tt{Iterator}s and @tt{Collection}s from Lab @secref["lab11"].

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