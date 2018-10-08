#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")

@title[#:style 'unnumbered #:tag "lab9"]{Lab 9: No Step on Snek}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/beginner-abbr.html"]{Beginning Student
Language with LIST ABBREVIATIONS}.

Make sure you follow
@link["https://cs.umd.edu/class/fall2018/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab9:list"]{List Abbreviations}

In order to get comfortable with list abbreviations, complete the
following finger exercises.

@larger{@bold{Ex 1}}: Using the @racket[cons] and @racket['()]
notation, create a list with the elements 1, 2, 3, 4.

@larger{@bold{Ex 2}}: Using the @racket[list] notation, create a list
with the elements 1, 2, 3, 4.

@larger{@bold{Ex 3}}: Using the @racket[cons] and @racket['()]
notation, create a list with the elements, which are themselves, 1, 2,
3, 4; @racket["a"], @racket["b"], @racket["c"]; and 6, 7.

@larger{@bold{Ex 4}}: Using the @racket[list] notation, create a list
with the elements, which are themselves lists, 1, 2, 3, 4;
@racket["a"], @racket["b"], @racket["c"]; and 6, 7.

@larger{@bold{Ex 5}}: Using the @racket[cons] and @racket['()]
notation, create a list with the elements @racket[(cons 1 '())],
@racket['()], and 4.


@larger{@bold{Ex 6}}: Using the @racket[list] notation, create a list
with the elements @racket[(cons 1 '())], @racket['()], and 4.


@larger{@bold{Ex 7}}: What are the elements of this list @racket[(list
1 2 3 4)]?

@larger{@bold{Ex 8}}: What are the elements of this list @racket[(cons
1 (cons 2 (cons 3 (cons 4 '()))))]?


@larger{@bold{Ex 9}}: What are the elements of this list @racket[(cons
(cons 1 '()) (cons #false (cons (cons "a" (cons "b" '())) (cons '()
'()))))]?




@section[#:style 'unnumbered #:tag "lab9:snek"]{List Abbreviations}

Swap @bold{Head} and @bold{Hands}!

@larger{@bold{Ex 10}}: Starting from the code
(@link["snake.0.rkt"]{@tt{snake.0.rkt}}) we designed in class,
finish these simple version of the Snake game.

@larger{@bold{Ex 11}}: Refine the Snake game to use an arbitrary
number of eggs that always appear in the Snake's former path.

