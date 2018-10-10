#lang scribble/manual
@(require scribble/core)

@title[#:style 'unnumbered #:tag "lab10"]{Lab 10: Eggs and Abstractions}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/beginner-abbr.html"]{Beginning Student
Language with LIST ABBREVIATIONS}.

Make sure you follow
@link["https://cs.umd.edu/class/fall2018/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab10:eggs"]{Many Eggs}

@larger{@bold{Ex 1}}: Starting from the code we saw in class today
(@link["snake.1.rkt"]{@tt{snake.1.rkt}}), modify the game so that
there can be an arbitrary number of eggs in play at any given time.


@larger{@bold{Ex 2}}: Modify the game so that instead of a new egg
being generated after eating an egg, a new egg may be generated after
every 15 ticks of time.

@larger{@bold{Ex 3}}: Modify the game so that instead of a new egg
appearing randomly anywhere on the screen, new eggs appear randomly
under some segment of the snake.  You may find the
@racket[pick-random] function defined in the file helpful.

@section[#:style 'unnumbered #:tag "lab10:abstract"]{Abstracting Snake}

@larger{@bold{Ex 4}}: Review all of the code, both given to you and
written by you, and try to develop abstraction functions and
reformulate the existing code to use your new abstraction functions.
