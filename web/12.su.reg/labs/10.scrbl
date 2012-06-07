#lang scribble/manual

@(require "../lab.rkt"
          "../unnumbered.rkt"
          "../utils.rkt"
          scribble/eval)

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require lang/htdp-intermediate))
    the-eval))


@(define exercise (exercise-counter))

@title[#:tag "lab10"]{6/7: A Whole New World}

The goal of this lab is to practice designing world programs in an
object-oriented setting.

@lab:section{Fun New Worlds}

Just as we used the World library for designing interactive video
games in Fundamentals I, there is a Java library that is quite similar
for designing interactive games written in Java.

The library, developed by Prof. Proulx, is avalable here:

@tt{@link["http://www.ccs.neu.edu/javalib/FunWorld/index.html"]{http://www.ccs.neu.edu/javalib/FunWorld/}}

Have a look at the JavaDocs for the library here, in particular, look
at the World class:

@tt{@url{http://www.ccs.neu.edu/javalib/FunWorld/funworld-docs/}}

Download the necessary @tt{jar} files for the library and make sure
Eclipse knows where to find them.  Ian and Jason can help you with
this.

@exercise{Design the simplest world program you can; it may simply
tick along and do nothing.}

@exercise{Design a world program that involves a single circular image
dropping from off the top of the screen to off the bottom of the
screen.}

@exercise{Design a world program that involves multiple circular
images dropping from off the top of the screen to off the bottom of
the screen.}

@exercise{Design a world program that involves multiple circular
images dropping from off the top of the screen to off the bottom of
the screen. Whenever the player hits the "c" key, a new circle should
fall from the top.}

@exercise{Revise the above program so that circles bounce on the
bottom edge of the screen and the move up until they're off the top of
the screen.}

