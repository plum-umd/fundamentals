#lang scribble/manual
@(require scribble/core (for-label lang/htdp-intermediate) "helper.rkt")

@title[#:style 'unnumbered #:tag "lab15"]{Lab 15: [Listof Anything]}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/intermediate.html"]{Intermediate Student
Language}.

Make sure you follow
@link["https://cs.umd.edu/class/fall2017/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab15:los"]{A List of Something}

We've seen the @emph{parameterized} data definition for lists in class.

@#reader scribble/comment-reader (racketblock
;; A [Listof X] is one of:
;; - '()
;; - (cons X [Listof X])

;; list-template : [Listof X] -> ???
(define (list-template l)
  (cond [(empty? l) ...]
        [else (... (first l)
                   ...
                   (list-template (rest l))
                   ...)]))
)

We can write @emph{[Listof X]} (where @emph{X} is any other data definition) in
our signatures.

@larger{@bold{Ex 1}}: Write down the signature for a function @tt{foo} that
consumes a list of numbers and returns a list of strings.

@larger{@bold{Ex 2}}: Write down the signature for a function @tt{bar} that
consumes a list of lists of strings and returns a list of images.

@larger{@bold{Ex 3}}: Write down the signature for a function @tt{baz} that
consumes a number and returns a list of lists of lists of strings.

@larger{@bold{Ex 4}}: The general list data definition has a single parameter
@emph{X}. Write down the general data definition for @emph{Posn}s with two
parameters @emph{X} and @emph{Y}.

@larger{@bold{Ex 5}}: Write down the signature for a function @tt{bazoinks} that
consumes a list of posns each containing a string and an image, and returns a posn
containing two lists of numbers.

In the following exercises, be sure to use one of the higher-order functions
@racket[filter], @racket[foldr], or @racket[map] in your implementation. It's
always possible to use more than one, so pick the simplest one for the task at
hand. Also, feel free to use @racket[local] definitions for any simple
helper-functions you implement, especially when they're used in only one
function.

@section[#:style 'unnumbered #:tag "lab15:maps"]{Operating on List Elements}

@larger{@bold{Ex 6}}: Design a function @tt{fake-news} that, given a
@emph{[Listof String]}, returns a new @emph{[Listof String]} where each
@tt{String} in the original is replaced with the string @racket["Fake News"].

@larger{@bold{Ex 7}}: Design a function @tt{emphatic} that, given a
@emph{[Listof String]}, returns a new @emph{[Listof String]} where each
string in the original has had an exclamation point @racket["!"] added to
the end.

@larger{@bold{Ex 8}}: Design a function @tt{auto-grader} that, given a list of
strings returns a list of numbers where each number is a random value between 0
and 100.

@larger{@bold{Ex 9}}: Design a function @tt{counts} that, given a @emph{[Listof
[Listof String]]}, returns a @emph{[Listof Number]} where each number is the
@racket[length] of each list of strings.

@larger{@bold{Ex 10}}: Design a function @tt{run-tests} that, given a list of
numbers and a function @tt{f} with the signature @emph{(Number -> Boolean)},
returns a list of booleans where each boolean is the result of applying @tt{f}
to each number in the list.


@section[#:style 'unnumbered #:tag "lab15:filters"]{Removing Some Elements}

@larger{@bold{Ex 11}}: Design a function @tt{only-passing} that, given a list of
numbers, returns a new list of numbers where any numbers in that list that are
less than 66 are removed.

@larger{@bold{Ex 12}}: Design a function @tt{wider-than-n} that, given a list of
images and a natural number @tt{n}, returns a new list of images that contains
only the images in the original list that have an @racket[image-width] greater
than @tt{n}.

@larger{@bold{Ex 13}}: Design a function @tt{only-in-bounds} that, given a list
of @emph{[Posn Int Int]}s and four integers @tt{xmin}, @tt{ymin}, @tt{xmax} and
@tt{ymax}, that returns a new list of posns that only contains posns from the
original list with @tt{x} values where @tt{xmin <= x <= xmax} and @tt{y} values
where @tt{ymin <= y <= ymax}.

@larger{@bold{Ex 14}}: Design a function @tt{shorter-than-n} that, given a list
of lists of any type and a natural number @tt{n}, returns a new list of lists of
the same type that only contains the lists in the given lists that have a length
less than @tt{n}.


@section[#:style 'unnumbered #:tag "lab15:folds"]{Combining Elements}

@larger{@bold{Ex 15}}: Design a function @tt{all-true} that returns
@racket[#true] only if the elements of the given list of booleans are all
@racket[#true].

@larger{@bold{Ex 16}}: Design a function @tt{any-true} that returns
@racket[#true] only if the any of the elements of the given list of booleans is
@racket[#true].

@larger{@bold{Ex 17}}: What purpose statement would you give the following
function?

@#reader scribble/comment-reader (racketblock
;; wat : [Listof X] -> [Listof X]
;; ???
(define (wat l) (foldr cons '() l))
)

@larger{@bold{Ex 18}}: Design a function @tt{duplicate}, that given a list of
any type, returns a new list of the same type where each of the elements has
been duplicated.

@#reader scribble/comment-reader (racketblock
(check-expect (duplicate '()) '())
(check-expect (duplicate '(1 2 3)) '(1 1 2 2 3 3))
(check-expect (duplicate '((1 2 3) () (4) (5 6))) 
              '((1 2 3) (1 2 3) () () (4) (4) (5 6) (5 6)))
)

@larger{@bold{Ex 19}}: Design a function @tt{prepend-all}, that given a list of
strings and a string @tt{delim}, returns a new list of strings where each of the
elements in the original has had @tt{delim} placed before it.

@#reader scribble/comment-reader (racketblock
(check-expect (prepend-all '() "\n") '())
(check-expect (prepend-all '("foo" "bar") " ") '(" " "foo" " " "bar"))
(check-expect (prepend-all '("a1" "a2" "a3") ",") '("," "a1" "," "a2" "," "a3"))
)
