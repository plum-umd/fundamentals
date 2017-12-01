#lang scribble/manual
@(require "../utils.rkt"
          (for-label class/0 class/universe))

@title[#:tag "assign04"]{1/30: A Universe of Pong}

Due: 1/30.

@section{Networked pong}

Remember your pong game from last week? Remember how crowded it was at
the keyboard when you challenged your roomate to a late-night match?
This week, we’re going to fix that.

The solution to fixing this is to support playing pong between
@emph{different} computers. We’ll use the the @racket[universe]
system.

You should adapt your current pong design to the new setting. You’ll
need to write both a client program and a server program. The server
program should enforce that clients play by the rules—no moving too
fast, or putting the ball in the wrong place, or other such trickery
is allowed.

When you submit the assignment, make sure that you test all of your
code, even the parts that deal with communication. Also, provide an
easy way to run a multi-player version of your game.

Of course, if there were any aspects of your Pong game for last week
that you weren’t happy with, you can take advantage of this week to
improve those as well.

