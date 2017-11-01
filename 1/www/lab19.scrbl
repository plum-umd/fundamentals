#lang scribble/manual
@(require scribble/core (for-label lang/htdp-intermediate) "helper.rkt")

@title[#:style 'unnumbered #:tag "lab19"]{Lab 19: Around & Around We Go}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/intermediate-lam.html"]{Intermediate
Student Language with Lambda}.

Make sure you follow
@link["https://cs.umd.edu/class/fall2017/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab19:sigtofun"]{From Signatures to Functions}

A function signature describes a class of functions. Most signatures describe an
infinite number of functions. The signature @tt{[@emph{Number} ->
@emph{Boolean}]} includes @racket[even?], @racket[(λ (n) (> n 0))], and @racket[(λ
(n) (or (= 0 (modulo 5 n)) (even? n)))].

@bold{Ex 1}: Define three functions @tt{ex-1-{1,2,3}} with the signature
@tt{@emph{String} -> @emph{Number}}.

@colorize["red"]{@bold{Hint}}: Be careful: the wiseacres lab might want to give
@racket[(λ (x) 0)] as a function with the signature @tt{@emph{String} ->
@emph{Number}}. However, the @emph{most general} signature that we can give
@racket[(λ (x) 0)] is @tt{[@emph{X}] . @emph{X} -> @emph{Number}}, since the
input @tt{x} is never used as a @emph{String}. Make sure the inputs to
@tt{ex-1-{1,2,3}} are used as @emph{String}s in your example functions.

@bold{Ex 2}: Define three functions @tt{ex-2-{1,2,3}} with the signature

@tt{@emph{Number} [@emph{Number} -> @emph{Boolean}] -> @emph{String}}.

@bold{Ex 3}: Define two function @tt{ex-3-{1,2}} with the signature

@tt{[@emph{X}] . [@emph{Number} -> @emph{X}] -> @emph{X}}.

@bold{Ex 4}: Define two functions @tt{ex-4-{1,2}} with the signature

@tt{[@emph{X} @emph{Y}] . [Listof @emph{X}] [@emph{X} -> @emph{Number}]
[@emph{Number} -> @emph{Y}] -> [Listof @emph{Y}]}.


@section[#:style 'unnumbered #:tag "lab19:funtosig"]{From Functions to Signatures}

Swap @bold{Head} and @bold{Hands}!

@bold{Ex 5}: Provide the @emph{most general} signatures for each of the
following functions. If you are unsure of your answer, ask one of the pairs near
you. If you are asked about your signature by some other pair, answer only
"that's what we got", "ours is more general", or "ours is less general"; don't
show them your solution.

@#reader scribble/comment-reader (racketblock
;; brillig : 
(define (brillig x)
  (= 1 (modulo x 2)))

;; slithy :
(define (slithy m n)
  (> (length m) n))

;; outgrabe :
(define (outgrabe x y)
  (map x (append y y)))

;; uffish :
(define (uffish x)
  (cond [(number? x) (+ x 10)]
        [(string? x) (string-length x)]))

;; frabjous :
(define (frabjous a b c)
  (a (b c)))

;; callooh :
(define (callooh a b)
  (a 10 (or (b 0) (b 1))))

;; callay :
(define (callay q)
  (frabjous (λ (d) (+ d 42)) q "day"))
)


@section[#:style 'unnumbered #:tag "lab19:absops"]{Abstract Operations
@emph{Abound}}

Swap @bold{Head} and @bold{Hands}!

@bold{Note}: You should use abstract list operations in each of the solutions of
this section.

@bold{Ex 6}: Design a function @tt{ex-6} that removes from a list of numbers any
multiples of 6.

@bold{Ex 7}: Design a function @tt{ex-7} that, given a @tt{[Listof @emph{X}]}, a
@emph{String} @tt{pre} and a function @tt{foo : @emph{X} -> @emph{String}},
returns a list of strings where each @emph{X} value in the given list was
converted to a string and prepended with @tt{pre}.

@bold{Ex 8}: Design a function @tt{ex-8} that, given a list of student names,
returns a list of randomly-generated grades between 0 and 100 for those
students.

@bold{Ex 9}: Design a function @tt{ex-9} that, given a list of grades,
returns a list of randomly-generated grades between 0 and 100 for those
students.


@section[#:style 'unnumbered #:tag "lab19:handrolled"]{Abstracting Operations}

Swap @bold{Head} and @bold{Hands}!

The recursive data definition and template for the natural numbers are as
follows:

@#reader scribble/comment-reader (racketblock
;; A @emph{Natural} is one of:
;; - 0
;; - (add1 @emph{Natural})
;; Interp: The non-negative integers.

;; natural-template : @emph{Natural} -> ???
;; The form of recursive functions over @emph{Natural} numbers.
(define (natural-template n)
  (cond [(= 0 n) ...]
        [else (... n
                   ...
                   (natural-template (sub1 n)))]))
)

@bold{Ex 10}: The following three functions share a similar form over the
@emph{Natural} numbers. Design an abstraction of @tt{hyp{0,1,2}} named
@tt{hypN}, then define @tt{hyp0/2}, @tt{hyp1/2}, and @tt{hyp2/2} in terms of
@tt{hypN}.

@#reader scribble/comment-reader (racketblock
;; hyp1 : @emph{Number} @emph{Number} -> @emph{Number}
;; The first hyperoperation over natural numbers.
(define (hyp1 a n)
  (cond [(= 0 n) a]
        [else (add1 (hyp1 a (sub1 n)))]))

;; hyp2 : @emph{Number} @emph{Number} -> @emph{Number}
;; The second hyperoperation over natural numbers.
(define (hyp2 a n)
  (cond [(= 0 n) a]
        [else (+ a (hyp2 a (sub1 n)))]))

;; hyp3 : @emph{Number} @emph{Number} -> @emph{Number}
;; The third hyperoperation over natural numbers.
(define (hyp3 a n)
  (cond [(= 0 n) a]
        [else (* a (hyp3 a (sub1 n)))]))
)


@bold{Ex 11}: Define the fourth
@link["https://en.wikipedia.org/wiki/Tetration"]{hyperoperation over natural
numbers} @tt{hyp4 : @emph{Number} @emph{Number} -> @emph{Number}} in terms of
@tt{hypN}.

@bold{Ex 12}: Design the function @tt{foldd : [X] . [Natural X -> X] X Natural
-> X}. If you get stuck look at the @tt{natural-template} and consider the
implementation of @tt{foldr} over lists from your notes. Your implementation
should pass the tests below.

@#reader scribble/comment-reader (racketblock
(check-expect (foldd cons '() 3) '(3 2 1))
(check-expect (foldd * 1 5) 120)
(check-expect (foldd (λ (n s) (string-append (number->string n) " " s))
                     "0"
                     10)
              "10 9 8 7 6 5 4 3 2 1 0")
)

@bold{Ex 13}: Define the function @tt{hypN/2} in terms of @tt{foldd}.
