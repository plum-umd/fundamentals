#lang scribble/manual
@(require scribble/core (for-label lang/htdp-intermediate) "helper.rkt")

@title[#:style 'unnumbered #:tag "lab15"]{Lab 15: Oobleck}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/intermediate.html"]{Intermediate Student
Language}.

Make sure you follow
@link["https://cs.umd.edu/class/fall2018/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab15:magicians"]{Royal Magicians}

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
;; - (make-muzzle-street String Shuffle Muzzle)
;;
;; A Muff is a [Listof Shuffle].
)

The interpretation is omitted intentionally. Since we don't know anything about
the meaning of this data, we must operate on @emph{Shuffle}s based on the
structure alone. To do that, we'll use templates.

If your partner hands you a @emph{Shuffle}, you've got one of three things. To
deal with that @emph{Shuffle}, you need to know which of those three things you
have! Any function you write has to know the same.

We can distinguish @emph{Muff} from other @emph{Shuffle}s with
@racket[list?]. We can implement @tt{duffle?} and @tt{muzzle?} to succinctly
distinguish @emph{Duffle}s from @emph{Muzzle}s.

@bold{Ex 1}: Implement @tt{duffle? : Shuffle -> Boolean}, which returns
@racket[#true] only if the given Shuffle is a Duffle.

@bold{Ex 2}: Implement @tt{muzzle? : Shuffle -> Boolean}, which returns
@racket[#true] only if the given Shuffle is a Muzzle.

@bold{Ex 3}: How much testing is too much testing? Is it necessary to look
through a @emph{Muzzle} to ensure that its @tt{oobleck} is a @emph{String} to
distinguish it from other @emph{Shuffle}s? Does your answer change if we
modified the signature of @tt{muzzle?} to accept @emph{Any} value?

Well-named predicates help keep functions on @emph{Shuffle}s intelligible.


@section[#:style 'unnumbered #:tag "lab15:mantra"]{Search and Destroy}


@#reader scribble/comment-reader (racketblock
;; muff? : Shuffle -> Boolean
;; Is the given Shuffle a Muff?
(define muff? list?)

;; shuffle-template : Shuffle -> ???
;; The form of every structural operation over Shuffles.
(define (shuffle-template s)
  (cond [(duffle? s) (duffle-template s)]
        [(muzzle? s) (muzzle-template s)]
        [(muff? s) (muff-template s)]))
)

In @emph{Shuffle}s, as in all @emph{itemizations}, we search through the
possible cases with @racket[cond] to figure out what to do with the
@emph{Shuffle} at hand. We then refer each case to the proper template for more
specific handling, signaling where helper functions should appear in our code.

@bold{Ex 4}: @emph{Duffle}s are @emph{itemizations} as well. Define
@tt{duffle-template}, the form that all functions that operate on
@emph{Duffle}s.

@#reader scribble/comment-reader (racketblock
;; duffle-template : Duffle -> ???
)

The @tt{muff-template} is the standard @emph{List} template we've written time
and time again. We have one of the two possible cases of the @emph{[Listof
Shuffle]} @emph{itemization}.

@itemlist[

  @item{If we find @racket['()] we'll have to return something. What we return
        is entirely output-specific, so omitted from the template.}

  @item{If we find a @emph{composite} @racket[cons] cell, we destroy it
        by tearing it apart. The smaller pieces are passed off to their
        respective templates for specific handling, signaling where helper
        functions should appear in our code.}

]

@#reader scribble/comment-reader (racketblock
;; muff-template : Muff -> ???
;; The form of all functions that operate on @emph{Muff}s.
(define (muff-template m)
  (cond [(empty? m) ...]
        [(cons? m) (... (shuffle-template (first m))
                        ...
                        (muff-template (rest m)) ...)]))
)

@bold{Ex 5}: Why don't we pass @racket['()] @emph{Muff}s to the
@tt{empty-template}? Put another way, is there anything you can express with an
application @tt{(handle-empty '())} of type @tt{handle-empty : '() -> X} that
you can't express by a plain value type of type @emph{X}?


@bold{Ex 6}: Write down the template for all operations that consume a
@emph{Muzzle}. Be sure to include explicit applications of any necessary
templates.


@section[#:style 'unnumbered #:tag "lab15:assoc"]{Working on @emph{Muzzle}s}

Swap @bold{Head} and @bold{Hands}!

@bold{Ex 7}: Design a function @tt{count-streets} that returns the number of
@emph{Muzzle} streets in a given @emph{Muzzle}.

@colorize["red"]{Hint}: Make sure you check all the places a @emph{Muzzle} can
be found inside a @emph{Muzzle}, it may require a helper function
@tt{shuffle-streets}.

@racketblock[(define MUZ0 (make-muzzle-town))
             (define MUZ1 (make-muzzle-street "foo" 42 MUZ0))
             (define MUZ2 (make-muzzle-street "bar" MUZ1 MUZ1))
             (check-expect (count-streets MUZ0) 0)
             (check-expect (count-streets MUZ1) 1)
             (check-expect (count-streets MUZ2) 3)]

@bold{Ex 8}: Design a function @tt{muzzle-find} that is given a @emph{Muzzle}
and a string @tt{key}. If the @emph{Muzzle} contains a @emph{Muzzle} street with
@tt{key} in the @tt{oobleck} field, it returns the @emph{Duffle} in the
@tt{tumble} field. Otherwise, @tt{muzzle-find} returns #false. (Hint: you may have
to design a new data definition to handle this choice of outputs).

@racketblock[(check-expect (muzzle-find MUZ0 "foo") #false)
             (check-expect (muzzle-find MUZ1 "bar") #false)
             (check-expect (muzzle-find MUZ2 "foo") 42)
             (check-expect (muzzle-find MUZ2 "bar") MUZ1)]


@section[#:style 'unnumbered #:tag "lab15:json"]{Visualizing @emph{Shuffle}s}

Swap @bold{Head} and @bold{Hands}!

To get a better view of @emph{Shuffle}s, let's convert them to
strings. @emph{Duffle}s are easy.

@bold{Ex 9}: Design a function @tt{duffle->string} that turns any @emph{Duffle}
into a string. If the @emph{Duffle} is a string already, wrap it in quotes:
@racket["\"rah\""]. If the @emph{Duffle} is a number, just convert it to a
string.

@racketblock[(check-expect (duffle->string "foo") "\"foo\"")
             (check-expect (duffle->string 42) "42")]

@emph{Muzzle}s, @emph{Muff}s, and @emph{Shuffle}s are mutually recursive, so
we'll need to design these together.

@bold{Ex 10}: Copy the @tt{muzzle-template}, @tt{muff-template}, and
@tt{shuffle-template}s into the bottom of your @emph{definitions window}. Rename
these definitions @tt{muzzle->string}, @tt{muff->string}, and
@tt{shuffle->string}. Write down the proper signature and purpose statements.

Copy in this helper function:

@#reader scribble/comment-reader (racketblock
;; intersperse : [Listof String] String String String -> String
;; Join the given strings into a single string delimited by delim,
;; surrounded by open and close.
(define (intersperse los delim open close)
  (local [;; prepend-all : [Listof String] String -> String
          ;; Join the given strings into a single string prepended by delim
          (define (prepend-all los delim)
            (cond [(empty? los) close]
                  [else (string-append delim
                                       (first los)
                                       (prepend-all (rest los) delim))]))]
    (cond [(empty? los) (string-append open close)]
          [else (string-append open
                               (first los)
                               (prepend-all (rest los) delim))])))

(check-expect (intersperse '() "delimiter!" "first" "last") "firstlast")
(check-expect (intersperse '() "" "" "") "")
(check-expect (intersperse '("1" "2" "3") "" "" "") "123")
(check-expect (intersperse '("1" "2" "3") " " "" "") "1 2 3")
(check-expect (intersperse '("1" "2" "3") "," "[" "]") "[1,2,3]")
)

@bold{Ex 11}: Design the function @tt{muff->string} that returns a string
representating the given @emph{Muff}. The elements should be delimited by
@racket[","] and surrounded by square braces @racket["[...]"].

@colorize["red"]{Hint}: You will not be able to pass these tests until you've
finished exercises 12 and 13.

@bold{Ex 12}: Design a function @tt{muzzle->strings} that returns a list of
strings representating the given @emph{Muzzle}. Each @emph{Muzzle} street should
be converted into a single string, where the @tt{oobleck} and @tt{tumble} fields
are seperated with a colon @racket[":"], and each @tt{oobleck} wrapped in
quotes. (Hint: in order to implement this function, begin by pasting in the
templates for the relevant data definitions; you'll quickly see that you also
need @tt{duffle->string}, @tt{muff->string}, and @tt{shuffle->string}, and
@tt{muzzle->string}.) (Hint 2: you can implement @tt{muzzle->string} using
@tt{muzzle->strings} and @tt{intersperse}).

@racketblock[(check-expect (muzzle->strings MUZ0) '())
             (check-expect (muzzle->strings MUZ1) '("\"foo\":42"))
             (check-expect (muzzle->strings MUZ2)
                           '("\"bar\":{\"foo\":42}" "\"foo\":42"))]


