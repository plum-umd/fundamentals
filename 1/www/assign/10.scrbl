#lang scribble/base
@(require (for-label (only-in typed/racket define-type : U Listof require/typed)
                     (only-in racket define if > * define-struct module+)
                     rackunit
                     (only-in lang/htdp-intermediate-lambda check-expect)))
@(require scribble/manual)

@title[#:style 'unnumbered #:tag "assign10"]{Assignment 10: Typing Markup}

@bold{Due:} Tuesday, December 4, 11:59:59 PM EST.

@(define-syntax-rule (bslblock . body)
  (codeblock #:keep-lang-line? #f "#lang htdp/bsl" "\n" . body))

The following should be completed in cooperation with your latest
assigned partner.
You may not share code for this assignment with anyone but your
partner.

You must use the design recipe and @secref{style} guidelines to
receive full credit.

@section[#:tag "assign10:markup"]{Mark-up in Typed Racket}

This assignment deals with revisiting the @secref{assign8:ml} problem
and reformulating a solution to the problem in a typed programming
language: Typed Racket.

You may start from a canonical solution the problem in ISL+:
@link["ml.rkt"]{ml.rkt}.

You should submit a Typed Racket program saved as @tt{tml.rkt}.

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


The trickiest part of getting start with this assignment is defining
the types you will need for the rest of the program.  Since this
involves a couple features we haven't discussed in class, the type
definitions are given to you:

@codeblock[#:keep-lang-line? #f]|{
#lang typed/racket
(define-type Content [Listof Item])
; Interp: a collection of content in a markup document

(define-type Item
  (U String
     Element))
; Interp: an item in a document, either text or an element

(define-type Element (elem Tag Content))
; Interp: an element is content annotated with a tag
(define-struct (A B) elem ([tag : A] [content : B]) #:transparent)

(define-type Tag String)
; Interp: the name of the tag, e.g. "p"
}|

The first definition is straightforward and uses the built-in
parametric type @racket[Listof].  The second definition, which was an
itemization in ISL+, uses the @racket[U] parametric type, which
constructs an itemization of types.  The third definition defines a
structure type by fixing the the parameters of a parametric structure
type to be specific types.  Note that the use of
@racket[#:transparent] is a bit of Typed Racket magic to get the
testing framework to work properly; don't worry about it, but be sure
to include it.  The last definition sets up an alias by defining one
type as equal to another.

We these definitions in place, it should be pretty easy to translate
your ISL+ code in to Typed Racket.  We suggest you do it peicemeal by
tackling each exercise and getting it to work before moving on.
Trying to do the whole thing at once will make life more difficult
than it needs to be.


@bold{Real Talk About Numbers}: we've mostly glossed
over it in this class, but ISL+ and Typed Racket have a very
sophisticated system of numbers that closely resemble what we think of
as numbers in mathematics.  So for example @racketresult[2/3] is
exactly the rational number 2/3.  It's not an approximation such as
@racketresult[0.666666666] as in many other programming languages.
There's also no such thing as the ``largest'' number, just like in
math and again unlike most other languages.  There are also imaginary
numbers.  So you can compute @racket[(sqr -1)] for example, and it
will produce @racketresult[0+1i], i.e. a complex number with an
imaginary part of @racketresult[1].

So when we write @tt{Number} in a data definition, or @racket[Number]
as a type in Typed Racket, we really mean @emph{all} numbers:
integers, rationals, complex numbers, etc.  This can lead to some
surprises when you try to assign types to your program because you
often take for granted that mathematical operations are defined on all
types of numbers.  But this is not the case.  It turns out asking if
one number is less than another does not make sense when you're
considering complex numbers (well, really, it makes sense but is
ambiguous and there is no one right ``sense'' that it makes).  That
means that if you wrote the following function to select the bigger of
two given numbers, Typed Racket would not sign-off on this type for
the function:

@codeblock[#:keep-lang-line? #f]|{
  #lang typed/racket
  (: bigger : (Number Number -> Number))
  (define (bigger n1 n2)
    (if (> n1 n2) n1 n2))
}|

That's because if you were to pass in complex numbers, which are
numbers, the code would result in a run-time type error because
@racket[>] doesn't work on complex numbers.  So instead, you want to
give a more restricted type to this function:
@codeblock[#:keep-lang-line? #f]|{
  #lang typed/racket
  (: bigger : (Real Real -> Real))
}|

This says you can pass in any kind of number that is not a complex
number and it will work, and Typed Racket is cool with it.  This issue
will pop-up in exercise 7.  @bold{END Real Talk About Numbers.}


