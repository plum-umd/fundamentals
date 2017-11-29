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

@(define-syntax-rule (trex . body)
   (examples #:label #f #:eval (make-base-eval #:lang 'typed/racket/base)
             . body))

@trex[0]

OK, so we've only tried @racket[0] so far, but there's a lot going on. Typed
Racket informs us of the type of every expression we type into the
@emph{interactions window}. For zero, it informs us that @racket[0] is an
@emph{Integer}, but more precisely has the type @emph{Zero}. It's useful to have
a very specific type for @emph{Zero}, so it can be used easily in other data
definitions.

@bold{Ex 1}: Do all numbers have their own type? Try out some other examples to
find out. You should find at least one number with another very specific
type.

@bold{Ex 2}: As detailed in the @secref{assign10} specification, the numbers
we've been using in class are very sophisticated compared to most programming
languages. What types do each of the numbers @racket[1], @racket[10],
@racket[100], @racket[1000], ...  @racket[1000000000000] have? Find one number
for each of the types given in the image below:

@centered{@image[#:scale 0.5]{lab26-int-tower.png}}

Look at the
@link["https://docs.racket-lang.org/ts-reference/type-ref.html"]{Type
Reference} for more details about the distinctions between these types, if
you're interested. Most of the time we can use the type @emph{Real} in the same
way as we used the data definition @emph{Number} to date.

@bold{Ex 3}: Test other atomic values that you've used in the student languages;
write down what types Typed Racket ascribes to them in a comment.


@section[#:style 'unnumbered #:tag "lab26:fun"]{Function Types}

Now let's type some anonymous functions in the @emph{interactions window}.

@trex[(lambda (x) x)]

This looks right, given @emph{Any} value that function just returns that same
value. The @racket[->] @emph{type constructor} is how function types are
expressed. As with most expressions we've seen in this class, it is a prefix
operation. The last argument given to @racket[->] is the output and the rest of
the arguments are the inputs.

@bold{Ex 3}: Write down (in a comment) types for functions that:

@itemlist[

  @item{accept a number and return a string,}

  @item{accept two strings and return a number,}

  @item{accept no arguments and return a @emph{Positive-Integer}.}

]

@trex[(eval:error (lambda (x) (if (zero? x) 1 (sub1 x))))]

Hmm, this was rejected. Let's see what the issue is:

@trex[zero?]

OK, the function @racket[zero?] only accepts values of type @emph{Number}. The
Type Checker claims that we gave it a value of type @emph{Any}. Because we gave
the argument @tt{x} no type, it could be anything! Without any ascribed type, an
anonymous function we write assumes its arguments can be of @emph{Any}
type. That would break the contract for @racket[=].

@bold{Ex 4}: Properly annotate the argument @tt{x} with a valid type to satisfy
the Type Checker. Look at the
@link["https://docs.racket-lang.org/ts-guide/types.html"]{Typed Racket guide} if
you are unsure of the proper syntax.

@bold{Ex 5}: Write down two examples of functions that are rejected by the Type
Checker. Did you find any functions that you'd like to be able to write that you
were unable to write?


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
type @emph{Tree} is defined as the @emph{union} of the @emph{leaf} and
@emph{node} types. That union is described by the @emph{type constructor}
@racket[U].

@bold{Ex 6}: Design the function @tt{tree-height} that given a @emph{Tree},
returns the height of that @emph{Tree}.

@bold{Ex 7}: Design the function @tt{tree-product} that given a @emph{Tree},
returns the product of all the leaves of that @emph{Tree}.

@bold{Ex 8}: Design a data type @emph{STree} for binary trees with strings at
the leaves. Design a function @tt{tree->stree} that maps
@racket[number->string] over a @emph{Tree}, returning an @emph{STree} of the
same form.

@bold{Ex 9}: It will be a pain to define a different @emph{*Tree} type for every
kind of value. Design a new parameteric type constructor @emph{Treeof} and new
structure definitions for leaves and values. The leaf structure must be able to
hold a value of any type. Refer to the @racket[struct] and @racket[define-type]
documentation to see how parameterized structures and types are defined.

@bold{Ex 10}: Design a more general @tt{tree-map} using your parameterized
tree from @bold{Ex 9}.

@bold{Ex 11}: Implement a function @tt{ntree->stree} that maps a @tt{[Treeof
@emph{Number}]} to a @tt{[Treeof @emph{String}]} using @tt{tree-map}.
