#lang scribble/manual
@(require scribble/core 
          (for-label lang/htdp-beginner-abbr) 
          "helper.rkt"
          "../utils.rkt")

@title[#:style 'unnumbered #:tag "lab9"]{Lab 9: Oobleck}

@(define ex (make-exerciser "Lab problem"))

@section[#:tag "lab9intro"]{Introduction(s)}

You'll work in labs in pairs.  Find someone to work with for this
first lab and introduce yourself. 

Make sure at least one of you have a laptop to work on for this lab.

The two of you will work as a team to solve problems. At any time, one of you
will be the @bold{Head} and the other will be the @bold{Hands}. The @bold{Head}
does the thinking and the @bold{Hands} does the typing. @bold{Hands} type only
what the @bold{Head} tells them to, but you're free to discuss any issues that
pop up. We'll have you switch off during the lab to make sure each of you get
practice problem solving, dealing with syntax, and getting finger exercises on
the keyboard.

@section[#:tag "lab9:purpose"]{Purpose}

In this lab, you'll practice making templates and designing functions
for complex data.


@section[#:style 'unnumbered #:tag "lab9:magicians"]{Royal Magicians}

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
;; A Muff is one of:
;; - '()
;; - (cons Shuffle Muff)
)

The interpretation is omitted intentionally. Since we don't know anything about
the meaning of this data, we must operate on @emph{Shuffle}s based on the
structure alone. To do that, we'll use templates.

If your partner hands you a @emph{Shuffle}, you've got one of three things. To
deal with that @emph{Shuffle}, you need to know which of those three things you
have! Any function you write has to know the same.

Let's make three predicates for distinguishing each of these three
kinds of things.

@ex[@racket[duffle?]]{

Implement @tt{duffle? : Shuffle -> Boolean}, which returns
@racket[#true] only if the given Shuffle is a Duffle.

}

@ex[@racket[muzzle?]]{

Implement @tt{muzzle? : Shuffle -> Boolean}, which returns
@racket[#true] only if the given Shuffle is a Muzzle.

}

@ex[@racket[muff?]]{

Implement @tt{muff? : Shuffle -> Boolean}, which returns
@racket[#true] only if the given Shuffle is a Muff.

}

Well-named predicates help keep functions on @emph{Shuffle}s intelligible.


@section[#:style 'unnumbered #:tag "lab9:mantra"]{Search and Destroy}


@#reader scribble/comment-reader (racketblock
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

@ex["Duffle template"]{

@emph{Duffle}s are @emph{itemizations} as well. Define
@tt{duffle-template}, the form that all functions that operate on
@emph{Duffle}s.

@#reader scribble/comment-reader (racketblock
;; duffle-template : Duffle -> ???
)

}


The @tt{muff-template} is the template we've written time and time
again. We have one of the two possible cases of the @emph{Muff}
@emph{itemization}.

@itemlist[

  @item{If we find @racket['()] we'll have to return something. What we return
        is entirely output-specific, so we indicate it with @racket[...].}

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


@ex["Muzzle template"]{

Write down the template for all operations that consume a
@emph{Muzzle}. Be sure to include explicit applications of any
necessary templates.

}


@section[#:style 'unnumbered #:tag "lab15:assoc"]{Working on @emph{Muzzle}s}

@;{Swap @bold{Head} and @bold{Hands}!}

@ex["Muzzle streets"]{

Design a function @tt{count-streets} that returns the number of
@emph{Muzzle} streets in a given @emph{Muzzle}.

Hint: Make sure you check all the places a @emph{Muzzle} can be found
inside a @emph{Muzzle}, it may require a helper function
@tt{shuffle-streets}.

@racketblock[(define MUZ0 (make-muzzle-town))
             (define MUZ1 (make-muzzle-street "foo" 42 MUZ0))
             (define MUZ2 (make-muzzle-street "bar" MUZ1 MUZ1))
             (check-expect (count-streets MUZ0) 0)
             (check-expect (count-streets MUZ1) 1)
             (check-expect (count-streets MUZ2) 3)]

}

@ex["Muzzle find"]{
 
Design a function @tt{muzzle-find} that is given a @emph{Muzzle}
and a string @tt{key}. If the @emph{Muzzle} contains a @emph{Muzzle} street with
@tt{key} in the @tt{oobleck} field, it returns the @emph{Shuffle} in the
@tt{tumble} field. Otherwise, @tt{muzzle-find} returns #false. (Hint: you may have
to design a new data definition to handle this choice of outputs).

@racketblock[(check-expect (muzzle-find MUZ0 "foo") #false)
             (check-expect (muzzle-find MUZ1 "bar") #false)
             (check-expect (muzzle-find MUZ2 "foo") 42)
             (check-expect (muzzle-find MUZ2 "bar") MUZ1)]

}

@;{
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


}