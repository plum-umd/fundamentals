#lang scribble/manual
@(require scribble/eval
          racket/sandbox
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ... check-expect))
          (for-label (except-in class/0 define-struct ... check-expect))
          (for-label class/universe)
          "../utils.rkt")

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (for-syntax racket/base)))
    (the-eval '(require class/0))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    (the-eval '(require "lectures/7/lon.rkt"))
    (the-eval '(require "lectures/7/los.rkt"))
    (the-eval '(require "lectures/7/lox.rkt"))
    the-eval))

@lecture-title[7]{Parametric Interface Definitions and Methods}

@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=48f72fe6-8739-484f-90e0-a8800126c62c"]{Video}.

In the last lecture, we developed an interface and implementation for
lists of numbers:

@class-block{
;; A LoN implements:
;;
;; length : -> Number
;; Compute the length of this list of numbers 
;;   
;; map : [Number -> Number] -> LoN
;; Apply given function to each element of this list of numbers

;; A (new empty-lon%) implements LoN
;; INTERP: Empty list of numbers
(define-class empty-lon%
  ;; Compute the length of this empty list of numbers
  (check-expect (send (new empty-lon%) length) 0)
  (define (length) 
    0)

  ;; map : [Number -> Number] -> LoN
  ;; Apply given function to each element of this empty list of numbers
  (check-expect (send (new empty-lon%) map add1) (new empty-lon%))
  (define (map f) 
    (new empty-lon%)))

;; A (new cons-lon% Number LoN) implements LoN
;; INTERP: Non-empty list of numbers
(define-class cons-lon%
  (fields first rest)

  ;; length : -> Number
  ;; Compute the length of this non-empty list of numbers
  (check-expect (send (new cons-lon% 3 (new cons-lon% 7 (new empty-lon%))) 
                      length)
                2)
  (define (length)
    (add1 (send (send this rest) length)))

 ;; map : [Number -> Number] -> LoN
 ;; Apply given function to each element of this non-empty list of numbers
 (check-expect (send (new cons-lon% 3 (new cons-lon% 7 (new empty-lon%))) 
                     map add1)
               (new cons-lon% 4 (new cons-lon% 8 (new empty-lon%))))
 (define (map f)
   (new cons-lon% 
        (f (send this first)) 
        (send (send this rest) map f))))
}

You could imagine doing a similiar development for lists of strings:

@class-block{
;; A LoS implements:
;;
;; length : -> Number
;; Compute the length of this of strings
;;   
;; map : [String -> String] -> LoS
;; Apply given function to each element of this list of strings

;; A (new empty-los%) implements LoS
;; INTERP: Empty list of strings
(define-class empty-los%
  ;; Compute the length of this empty list of strings
  (check-expect (send (new empty-los%) length) 0)
  (define (length) 0)

  ;; map : [String -> String] -> LoS
  ;; Apply the given function to each element of this empty list of strings
  (check-expect (send (new empty-los%) map string-upcase) (new empty-los%))
  (define (map f) (new empty-los%)))

;; A (new cons-los% String LoS) implements LoS
;; INTERP: Non-empty list of strings
(define-class cons-los%
  (fields first rest)

  ;; length : -> Number
  ;; Compute the length of this non-empty list of strings
  (check-expect (send (new cons-los% "a" (new cons-los% "b" (new empty-los%))) 
                      length)
                2)
  (define (length)
    (add1 (send (send this rest) length)))

  ;; map : [String -> String] -> LoS
  ;; Apply given function to each element of this non-empty list of strings
  (check-expect (send (new cons-los% "a" (new cons-los% "b" (new empty-los%))) 
                      map string-upcase)
                (new cons-los% "A" (new cons-los% "B" (new empty-los%))))
  (define (map f)
    (new cons-los% 
         (f (send this first)) 
         (send (send this rest) map f))))
}


Of course the obvious thing to observe is that these pairs of programs
are very very similar.

In fact, the @emph{code} is identical, it's only the signatures that
differ.  We can see evidence of this by experimenting with the code in
ways that break the signatures.  Notice that it's possible to
correctly compute with lists of strings even when they're represented
using the classes for lists of numbers.

@examples[#:eval the-eval
(send (new cons-lon% "a" (new cons-lon% "b" (new empty-lon%))) length)
(send (new cons-lon% "a" (new cons-lon% "b" (new empty-lon%))) 
      map string-upcase)
]

This is strong evidence to suggest that @emph{abstraction} is needed
to avoid the duplication.  Since the differences between these
programs is not at the level of @emph{values}, but @emph{data
definitions}, we should do abstraction at this level.  Let's consider
first the interface definitions:

@class-block{
;; A LoN implements:
;;
;; length : -> Number
;; Compute the length of this list of numbers 
;;   
;; map : [Number -> Number] -> LoN
;; Apply given function to each element of this list of numbers

;; A LoS implements:
;;
;; length : -> Number
;; Compute the length of this of strings
;;   
;; map : [String -> String] -> LoS
;; Apply given function to each element of this list of strings
}

By applying the abstraction process, we arrive at the following
@emph{parameterized} interface definition as a first cut:

@class-block{
;; A [Listof X] implements:
;;
;; length : -> Number
;; Compute the length of this list of numbers 
;;   
;; map : [X -> X] -> [Listof X]
;; Apply given function to each element of this list of numbers
}

We could then revise the data definitions and signatures of the
classes implementing this interface to arrive a single, re-usable
program:

@class-block{
;; A (new empty%) implements [Listof X]
;; INTERP: Empty list of Xs
(define-class empty%
  ;; Compute the length of this empty list of Xs
  (check-expect (send (new empty%) length) 0)
  (define (length) 
    0)

  ;; map : [X -> X] -> [Listof X]
  ;; Apply given function to each element of this empty list of Xs
  (check-expect (send (new empty%) map add1) (new empty%))
  (define (map f) 
    (new empty%)))

;; A (new cons% X [Listof X]) implements [Listof X]
;; INTERP: Non-empty list of Xs
(define-class cons%
  (fields first rest)

  ;; length : -> Number
  ;; Compute the length of this non-empty list of Xs
  (check-expect (send (new cons% 3 (new cons% 7 (new empty%))) 
                      length)
                2)
  (define (length)
    (add1 (send (send this rest) length)))

  ;; map : [X -> X] -> [Listof X]
  ;; Apply given function to each element of this non-empty list of Xs
  (check-expect (send (new cons% 3 (new cons% 7 (new empty%))) 
                      map add1)
                (new cons% 4 (new cons% 8 (new empty%))))
  (define (map f)
    (new cons%
         (f (send this first)) 
         (send (send this rest) map f))))
}

We can now reconstruct our original programs by applying the
parameteric definitions: @tt{[Listof Number]} and @tt{[Listof
String]}.  We also make new data definitions by applying @tt{Listof}
to other things.  For example, here's a computation over a @tt{[Listof
Boolean]}.

@examples[#:eval the-eval
(send (new cons% #true (new cons% #false (new empty%))) map not)
]

This is a big step forward, but there's an opportunity to do even
better.  Consider the following.

@examples[#:eval the-eval
(send (new cons% "a" (new cons% "aa" (new empty%))) map string-length)]

This program works fine and makes perfect sense.  It computes a length
of numbers from a list of strings.  However, it has broken the
signature of the @tt{map} method since @racket[string-length] does not
have the signature @tt{String -> String}, which is what's obtained when
plugging in @tt{String} for @tt{X}.

This is more evidence that further abstraction is possible.  In
particular we can loosen the constraints in the signature for
@tt{map}:

@class-block{
;; A [Listof X] implements
;;
;; map : [X -> Y] -> [Listof Y]
;; Apply given function to each element of this list of Xs
;;
;; ...
}

Notice that this method signature makes use of two parameters: @tt{X}
and @tt{Y}.  The @tt{X} parameter is "bound" at the level, @tt{[Listof
X]}.  The @tt{Y} is implicitly a parameter of the method's signature.

So in an object-oriented setting, these parameters can appear at the
interface and class level, but also at the method level.

We can do another exercise to write things we've seen before.  Let's
see what @tt{foldr} looks like:

@class-block{
;; A [Listof X] implements
;;
;; ...
;;
;; foldr : [X Y -> Y] Y -> Y
;; Fold over the elements with the given combining function and base
}

We can make some examples.

@examples[#:eval the-eval

(send (new empty%) foldr + 0)
(send (new cons% 5 (new cons% 3 (new empty%))) foldr + 0)
(send (new empty%) foldr string-append "")
(send (new cons% "5" (new cons% "3" (new empty%))) foldr string-append "")
]

Let's instantiate the template for @tt{foldr} for @racket[cons%].

@filebox[
 (racket cons%)
 @class-block{
;; foldr : [X Y -> Y] Y -> Y
;; Fold over this non-empty list of elements with combining function and base
(define (foldr f b)
  (send this first) ...
  (send (send this rest) foldr f b))
}]

Thinking through the examples and templates, we get:

@filebox[
 (racket empty%)
 @class-block{
;; foldr : [X Y -> Y] Y -> Y
;; Fold over this empty list of elements
(check-expect (send (new empty%) foldr + 0) 0)
(define (foldr f b) b)
}]

@filebox[
 (racket cons\%)
 @class-block{
;; foldr : [X Y -> Y] Y -> Y
;; Fold over this empty list of elements
(check-expect (send (new cons% 5 (new cons% 3 (new empty%))) foldr + 0)
              8)
(define (foldr f b)
  (f (send this first)
     (send (send this rest) foldr f b)))
}]


There's an interesting remaining question: how do we write methods
that work on specific kinds of lists?  For example, if we wanted to
write a @tt{sum} method that summed up the elements in a list of
numbers, how would we do it?  We can't put it into the @tt{[Listof X]}
interface since it wouldn't work if @tt{X} stood for string.
