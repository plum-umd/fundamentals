#lang scribble/manual
@(require scribble/core (for-label lang/htdp-intermediate) "helper.rkt")

@title[#:style 'unnumbered #:tag "lab17"]{Lab 17: Trees}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/intermediate.html"]{Intermediate Student
Language}.

Make sure you follow
@link["https://cs.umd.edu/class/fall2017/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab17:rant"]{MAD}

@#reader scribble/comment-reader (racketblock
;; A Shuffle is one of:
;; - Duffle
;; - Muzzle
;; - Muff
;;
;; A Duffle is one of:
;; - Number
;; - String
;;
(define-struct muzzle-town ())
(define-struct muzzle-street (oobleck tumble down))
;; A Muzzle is one of:
;; - (make-muzzle-town)
;; - (make-muzzle-street String Duffle Muzzle)
;;
;; A Muff is a [Listof Shuffle].
)

The interpretation is omitted intentionally. Since we don't know anything about
the meaning of this data, what do we know about @emph{Shuffle}s based on the
structure alone?

If your partner hands you a @emph{Shuffle}, you've got one of three things. To
deal with that @emph{Shuffle}, you need to know which of those three things you
have! Any function you write has to know the same:

@#reader scribble/comment-reader (racketblock
;; duffle? : Any -> Boolean
;; Is the given value a Duffle?
(define (duffle? x) (or (number? x) (string? x) (boolean? x)))

;; muzzle? : Any -> Boolean
;; Is the given value a Muzzle?
(define (muzzle? x) (or (muzzle-town? x) (muzzle-street? x)))

;; shuffle-template : Shuffle -> ???
;; The form of every structural operation over Shuffles.
(define (shuffle-template s)
  (cond [(duffle? s) (duffle-template s)]
        [(muzzle? s) (muzzle-template s)]
        [else (muff-template s)]))
)

@emph{Duffle}s are a similar situation. To operate on a @emph{Duffle}, you
need to know which of the three possible types of atomic data it is.

@#reader scribble/comment-reader (racketblock
;; duffle-template : Duffle -> ???
;; The form of every structural operation over Duffles.
(define (duffle-template s)
  (cond [(string? s) (... s ...)]
        [(number? s) (... s ...)]))
)

@bold{Note}: We omit explicit applications of the @tt{string-template} and
@tt{number-template}, since these are atomic data and have trivial templates.

@larger{@bold{Ex 2}}: Write down the template for all operations that consume a
@emph{Muzzle}. Does this remind you of any other template you've seen? Be sure
to include explicit applications of the @tt{shuffle-template} and the
@tt{muzzle-template}.

@larger{@bold{Ex 3}}: Design a function @tt{count-streets} that returns the
number of @emph{Muzzle} streets in a given @emph{Muzzle}.

@racketblock[(define MUZ0 (make-muzzle-town))
             (define MUZ1 (make-muzzle-street "foo" 42 MUZ0))
             (define MUZ2 (make-muzzle-street "bar" "baz" MUZ1))
             (check-expect (count-down-streets MUZ0) 0)
             (check-expect (count-down-streets MUZ1) 1)
             (check-expect (count-down-streets MUZ2) 2)]

@larger{@bold{Ex 4}}: Design a function @tt{muzzle-find} that is given a
@emph{Muzzle} and a string @tt{key}. If the @emph{Muzzle} contains a
@emph{Muzzle} street with @tt{key} in the @tt{oobleck} field, it returns the
@emph{Shuffle} in the @tt{tumble} field. Otherwise, @tt{muzzle-find} returns
#false.

@racketblock[(check-expect (muzzle-find MUZ0 "foo") #false)
             (check-expect (muzzle-find MUZ1 "bar") #false)
             (check-expect (muzzle-find MUZ2 "foo") 42)
             (check-expect (muzzle-find MUZ2 "bar") "baz")]

@larger{@bold{Ex 5}}: Design a function @tt{duffle->string} that turns any
@emph{Duffle} into a string. If the @emph{Duffle} is a string already, wrap it
in quotes: @racket["\"rah\""]. If the @emph{Duffle} is a number, just convert it
to a string.

@racketblock[(check-expect (duffle->string "foo") "\"foo\"")
             (check-expect (duffle->string 42) "42")]


@larger{@bold{Ex 6}}: Design a function @tt{muzzle->strings} that returns a list
of strings representating the given @emph{Muzzle}. Each @emph{Muzzle} street
should be converted into a single string, where the @tt{oobleck} and @tt{tumble}
fields are seperated with a colon @racket[":"].

@racketblock[(check-expect (muzzle->string MUZ0) '())
             (check-expect (muzzle->string MUZ1) '("\"foo\":42"))
             (check-expect (muzzle->string MUZ2) '("\"foo\":42")
                           '("\"foo\":42" "\"bar\":\"baz\""))]

@larger{@bold{Ex 7}}: Using the helper @tt{intersperse}, design a function
@tt{muzzle->string} that returns a string representating the given
@emph{Muzzle}. Each @emph{Muzzle} street should be converted into a single
string, where the @tt{oobleck} and @tt{tumble} fields are seperated with a colon
@racket[":"].

@#reader scribble/comment-reader (racketblock
;; intersperse : [Listof String] String -> String
;; Join the given strings into a single string delimited by delim.
(define (intersperse los delim)
  (cond [(empty? los) "{}"]
        [else (string-append "{" (first los) (prepend-all (rest los) delim))]))

;; prepend-all : [Listof String] String -> String
;; Join the given strings into a single string prepended by delim.
(define (prepend-all los delim)
  (cond [(empty? los) "}"]
        [else (string-append delim 
                             (first los)
                             (prepend-all (rest los) delim))]))
)

@larger{@bold{Ex 8}}: Design a function @tt{shuffle->string} that returns a
string representating the @emph{Shuffle}.
