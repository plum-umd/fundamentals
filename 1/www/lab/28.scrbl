#lang scribble/manual
@(require scribble/core scribble/examples "helper.rkt"
          (for-label lang/htdp-intermediate-lambda))

@title[#:style 'unnumbered #:tag "lab28"]{Lab 28: Sense and Reference}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/intermediate-lam.html"]{Intermediate
Student Language with Lambda}.

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab28:drills"]{Binding and Reference}

Today's lab focuses on @emph{binding} and @emph{reference} of identifiers. To
@emph{bind} an identifier is to assign it some value. To @emph{reference} an
identifier is to access the value to which it is bound.

In this class, we've often used @racket[define] to @emph{bind} values:

@#reader scribble/comment-reader (racketblock
(define foo 42)           ; @tt{foo} @emph{bound} to @tt{42}
(define (bar x) (+ 2 x))  ; @tt{bar} @emph{bound} to @tt{(λ (x) (+ 2 x))}
)

Functions introduce bindings as well. The function @tt{bar} above accepts a
single argument: @tt{x}. Any time @tt{bar} is applied, the body of the function
executes with @tt{x} bound to some value.

@bold{Ex 1}: What value is the argument @tt{x} bound to in the following three
applications of @tt{bar}? (Leave your answer in a comment.)

@#reader scribble/comment-reader (racketblock
(bar 0)    ; => 2
(bar 40)   ; => 42
(bar foo)  ; => 44
)

@bold{@colorize["red"]{Hint}}: In DrRacket, you can step through the examples.
You'll see the reference to @tt{x} replaced by its value each time @tt{bar} is
applied.


Consider the following example:

@#reader scribble/comment-reader (racketblock
(define x 0)

(define (double x) (+ x x))

(+ x x)     ; => ???
(double 1)  ; => ???
(double 2)  ; => ???
)

@bold{Ex 2}: How many bindings of the identifier @tt{x} appear in the above
code?

@bold{Ex 3}: How many references to the identifier @tt{x} appear?

@bold{Ex 4}: To which binding of @tt{x} do each of the references refer?

@bold{@colorize["red"]{Hint}} If you get stuck, you can always copy the code
into your @emph{definitions window} and hit the [Check Syntax] button. DrRacket
will draw arrows both to and from identifier bindings and
references. (References will be called @emph{occurences} by [Check Syntax].)


@section[#:style 'unnumbered #:tag "lab28:scope"]{Understanding Environments}

At any place in your code, there are identifiers that can be referenced:
identifiers that are @emph{in scope}. We call that set of known identifiers the
@emph{environment}.

Some identifiers are defined by the language itself.

@bold{Ex 5}: Name two identifiers that are provided by the
@link["https://docs.racket-lang.org/htdp-langs/intermediate-lam.html"]{Intermediate
Student Language with Lambda}.

Those identifiers are part of the initial environment. The environment grows as
you bind identifiers with @racket[define]. We also have frequently extended our
environment with libraries, such as the
@link["https://docs.racket-lang.org/teachpack/2htdpimage.html"]{image} and
@link["https://docs.racket-lang.org/teachpack/2htdpuniverse.html"]{universe}.

Of course, function bodies can refer to any known identifiers.

@#reader scribble/comment-reader (racketblock
;; Extend our environment to include
;; @racket[place-image], @racket[circle], @racket[empty-scene], ...
(require 2htdp/image)

(define WIDTH  400)
(define HEIGHT 200)

(define (place-circle x y)
  (place-image (circle 20 "solid" "red") x y (empty-scene WIDTH HEIGHT)))

(place-circle  20  20)  ; => places circle in upper left corner
(place-circle 200 100)  ; => places circle in center
(local [(define WIDTH  0)
        (define HEIGHT 0)]
  (place-circle 200 100)) ; => ???
)

@bold{Ex 6}: What happens in the last application of @tt{place-circle}? More to
the point: do the @racket[local] bindings of @tt{WIDTH} and @tt{HEIGHT} change
the environment of the body of @tt{place-circle}?

@bold{Spoiler Alert}: the answer to @bold{Ex 6} is decidedly no. The references
inside the body of @tt{place-circle} keep their bindings from the environment in
which @tt{place-circle} is defined, not all of the places where the function may
be applied. It's much easier to reason about the single environment from the
definition-site rather than the environments of all possible application-sites.


@section[#:style 'unnumbered #:tag "lab28:close"]{Closing over the Environment}

Functions close over their original environments, so how can we use this to our
advantage? We can encapsulate state! Consider the following data definition for
simple counters:

@#reader scribble/comment-reader (racketblock
;; A Command is one of:
;; - "next"
;; - "reset"
;; Interp: Commands that a Counter can accept.

;; cmd-template : Command -> ???
(define (cmd-template cmd)
  (cond [(string=? "next")  ...]
        [(string=? "reset") ...]))

;; A Counter is a (list Natural [Command -> Counter])
;; Interp: the first of the list is the current 

;; counter-template : Counter -> ???
(define (counter-template c)
  (... (first c) ...
       (second c) ...))

;; make-counter : Natural -> Counter
;; Create a Counter starting at the given number.
(define (make-counter n) ...)

;; next-counter : Counter -> Counter
;; Return the next counter.
(define (next-counter c) ((second c) "next"))

;; reset-counter : Counter -> Counter
;; Reset the given counter to 0.
(define (reset-counter c) ((second c) "reset"))

;; counter-value : Counter -> Natural
;; Return the counter's value.
(define (counter-value c) (first c))

(define c0 (make-counter 0))
(define c1 (next-counter c0))
(define c2 (next-counter c1))
(define c0* (reset-counter c2))

(check-expect (counter-value c0) 0)
(check-expect (counter-value c1) 1)
(check-expect (counter-value c2) 2)
(check-expect (counter-value c0*) 0)
)

@bold{Ex 7}: Define the function @tt{make-counter} such that the
@racket[check-expect]s pass. The first value of the counter will be the current
count; the second value of the counter will be a function that creates a new
counter given some command.

@bold{@colorize["red"]{Hint}}: Follow your templates!

@bold{@colorize["red"]{Hint}}: The function @tt{make-counter} must be recursive.

@bold{Ex 8}: Extend the data definition of command to allow double-counts
(incrementing by 2).

@bold{Ex 9}: Design a data definition and @tt{next} function for a counter-like
value that generates prime numbers.
