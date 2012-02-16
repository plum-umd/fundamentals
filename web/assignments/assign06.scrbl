#lang scribble/manual
@(require "../utils.rkt"
	  "../unnumbered.rkt"
          (for-label class/2 class/universe))

@title[#:tag "assign06"]{2/22: Nesting Worlds}

Due: 2/22.

Language: @racketmodname[class/2]

In this problem set, we'll be implementing operating systems---very
simple ones, but operating systems none-the-less.

@section{A boxed @tt{world}}

In this problem, you will implement a wrapper around worlds, which
will display a world with a big border around it.

A @tt{World} implements the following interface:

@codeblock{
;; on-tick : -> World
;; to-draw : -> Image
;; on-key : KeyEvent -> World
;; on-mouse : MouseEvent Number Number -> World
}

Every @tt{World} must produce a 200 by 200 image from the @tt{to-draw}
method.  

Your wrapper around @tt{World}s will be a class that contains a
@tt{World}.  You should then implement a @r[big-bang] program that
displays the wrapped @tt{World} and provides tick, key, and mouse
events to the wrapped @tt{World}. The world should be displayed with a
50 pixel border on all sides.  

Your world should provide tick events to the nested world at the rate
of @racket[(/ 1 28)].  Your world should provide all @tt{KeyEvent}s to
the nested world, @emph{except} the event @racket["q"], which should
end the entire @r[big-bang] program.

Your world should provide mouse events to the nested world, but
@emph{only} when the mouse is located over the 200 by 200 area showing
the nested world.  Your mouse events should be translated to the the
coordinates that the nested world expects.  This means that when the
mouse is over the top left corner of the @emph{nested} world, the
mouse events should have coordinates 0 and 0, not 50 and 50.

@section{A pair of boxed @tt{world}s}

Now, you should extend your implementation to handle two worlds,
side-by-side.  Note that these can be two @emph{different} nested
worlds.  Again, the worlds have a fixed size of 200 by 200.  Mouse
events should be passed to the world the mouse is over, translated so
that the coordinates are appropriate for that world.    Key events
should be passed to both worlds, and both worlds should tick at the
same rate.  

Again, the @racket["q"] key should quit the entire @racket[big-bang]
program.  