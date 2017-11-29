#lang scribble/manual
@(require scribble/core scribble/examples "helper.rkt"
          (for-label
          (only-meta-in 0 typed/racket)
          #;(only-in typed/racket define-type : U Listof require/typed)
          ))


@title[#:style 'unnumbered #:tag "lab26"]{Lab 26: Enforcing Data Definitions}

@(define-syntax-rule (bslblock . body)
  (codeblock #:keep-lang-line? #f "#lang htdp/bsl" "\n" . body))

Implement this lab in
@link["https://docs.racket-lang.org/htdp-langs/intermediate-lam.html"]{typed
racket}.

@section[#:style 'unnumbered #:tag "lab26:tr"]{Getting Typed Racket Running}

In order to create a Typed Racket program, you will need change the
language.  To do this, create a new program, open the Choose Language
menu item in DrRacket.  Choose "The Racket Language".  This will add
@tt{#lang racket} to the top of your file.  Edit the line to be
@tt{#lang typed/racket}.

As mentioned in lecture, Typed Racket, unlike ISL+ and friends, does
not have a testing framework built-in.  So we will be using the
@racketmodname[rackunit] library.

The following will import @racket[check-equal?], roughly analogous to
@racket[check-expect] from ISL+, from @racketmodname[rackunit].  We do this
in the context of a @tt{test} @emph{submodule}, which is the idiomatic way
of adding tests to Racket and Typed Racket programs:

@codeblock|{
  #lang typed/racket
  (module+ test
    (require/typed rackunit
      [check-equal? (Any Any -> Any)]))
}|

You can now write functions and tests like this:

@codeblock|{
  (: sqr : (Number -> Number))
  ;; Square the given number.
  (module+ test
    (check-equal? (sqr 5) 25)
    (check-equal? (sqr -5) 25))
  (define (sqr x) 
    (* x x))
}|

Choose the initial @bold{Head} and @bold{Hands}, and get started!

@section[#:style 'unnumbered #:tag "lab26:repl"]{Typed Interactions}

Once you've switched the language to Typed Racket, try some expressions in the
@emph{interactions window}.

@(define-syntax-rule (trex expr val . body)
   (examples #:label #f (eval:alts expr (eval:result (racketresult val) . body))))

@(define-syntax-rule (trerr expr . body)
   (examples #:label #f (eval:alts expr (eval:results '() "" (string-append . body)))))

@trex[0 0]{- : Integer [more precisely: Zero]}

OK, so we've only tried @racket[0] so far, but there's a lot going on. Typed
Racket informs us of the type of every expression we type into the
@emph{interactions window}. For zero, it informs us that @racket[0] is an
@emph{Integer}, but more precisely has the type @emph{Zero}. It's useful to have
a very specific type for @emph{Zero}, so it can be used easily in other data
definitions.

@bold{Ex 1}: Do all numbers have their own type? Try out some other examples to
find out. You should find at least one number with another very specific
type.

@bold{Ex 2}: Test other atomic values that you've used in the student languages;
write down what types Typed Racket ascribes to them in a comment.

Now let's type some anonymous functions in the @emph{interactions window}.

@trerr[(lambda (x) (if (= 0 x) 1 (sub1 x)))]{Type Checker: type mismatch
  expected: Number
  given: Any in: x}

Hmm, this was rejected because the function @racket[=] has the type @racket[(:
(-> Number Number Number * Boolean))], which means it accepts two or more
@emph{Numbers} as its input and returns a @emph{Boolean}. Because we gave the
argument @tt{x} no type, it could be anything! That would break the contract for
@racket[=].

@bold{Ex 3}: Properly annotate the argument @tt{x} as a @emph{Number} to satisfy
the Type Checker. Look at the
@link["https://docs.racket-lang.org/ts-guide/types.html"]{Typed Racket guide} if
you are unsure of the syntax to use.

@bold{Ex 4}: Write down two examples of functions that you can write in Typed
Racket in comments. Then write down two examples of functions that are rejected
by the Type Checker. Did you find any functions that you'd like to be able to
write that you were unable to write?


@section[#:style 'unnumbered #:tag "lab26:complex"]{More Complex Types}

Whenever we've needed to hold onto more than one piece of data at once, we've
defined structures to do so. When we've needed a type that can describe more
than one kind of data, we used an enumeration data definition. For example, we
can make binary trees with numbers at the leaves:

@#reader scribble/comment-reader (racketblock
(define-struct leaf (val))
(define-struct node (left right))

;; A Tree is one of:
;; - (leaf Number)
;; - (node Tree Tree)
)

We can do the same in Typed Racket, but the data definition is defined as a
@emph{type}.

In Typed Racket, we write a similar implementation, but define a type rather
than write a data definition. Note that the implementation refers to the type,
and the type refers to the names of the structs that make up the
implementation.

@racketblock[
(struct leaf ([val : Number]))
(struct node ([left : Tree] [right : Tree]))
(define-type Tree (U leaf node))
]

The structures look very similar, but their fields have annotated types. The
type is defined as the @emph{union} of the @emph{leaf} and @emph{node} types.

@bold{Ex 5}: Design the function @tt{tree-height} that given a @emph{Tree},
returns the height of that @emph{Tree}.

@bold{Ex 6}: Design the function @tt{tree-product} that given a @emph{Tree},
returns the product of all the leaves of that @emph{Tree}.

@bold{Ex 7}: Design a data type @emph{STree} for binary trees with strings at
the leaves.

@bold{Ex 8}: Design a function @tt{tree->stree} that maps
@racket[number->string] over a @emph{Tree}, returning an @emph{STree} of the
same form.
