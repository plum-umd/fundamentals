#lang scribble/manual
@(require scribble/eval
          racket/sandbox
          (for-label lang/htdp-intermediate-lambda)
          #;(for-label class0))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require lang/htdp-intermediate-lambda))
   ;(the-eval '(require class0))
    (call-in-sandbox-context 
     the-eval 
     (lambda () ((dynamic-require 'htdp/bsl/runtime 'configure)
                 (dynamic-require 'htdp/isl/lang/reader 'options))))
    the-eval))

@title{Lectures}

In this section, you'll find notes and code from 
each lecture.

@include-section["lectures/lec01.scrbl"]

Rocket + Satellite.

[High-level: here's why objects are a good idea.  Motivated by world
programs.

Data + functionality, using atomic data (numbers) and compound data
(pair of numbers).  Methods for animation.]

Ch. 1, 2, 10

@section[#:tag "lec02"]{1/13: Designing classes}

[Here we build OO-design from the gound up.]

[Enumerations, unions (recursive), containment, ...]

[The Union lives in your head.  Move on to interface version quickly.]

[Writing data definitions.  How to write methods for all of these
things.]

Ch. 3, 4, 5, 6, 11, 12.

@section[#:tag "lec03"]{1/17 - MLK}

There is no lecture on 1/17 since it is Martin Luther King, Jr. Day.

@section[#:tag "lec04"]{1/20: Dispatch and Interfaces}

@section[#:tag "lec05"]{1/24: Designing applicative classes}

[Bigger system design.  In lecture: Snake Game.]

@section[#:tag "lec06"]{1/27: Universes}
@section[#:tag "lec07"]{1/31: }
@section[#:tag "lec08"]{ 2/3: }
@section[#:tag "lec09"]{ 2/7: Abstracting with classes}                        
@section[#:tag "lec10"]{2/10: Designing methods with accumulators}
@section[#:tag "lec11"]{2/14: Designing generative recursive methods}
@section[#:tag "lec12"]{2/17: Intermezzo: Java}
@section[#:tag "lec13"]{2/21 - Presidents}

There is no lecture on 2/21 since it is Presidents Day.

@section[#:tag "lec14"]{2/24: }
@section[#:tag "lec15"]{2/28 - Spring break}

There is no lecture on 2/28 since it is during Spring break.

@section[#:tag "lec16"]{3/3 - Spring break}

There is no lecture on 3/3 since it is during Spring break.


@section[#:tag "lec17"]{ 3/7: Circular data; Methods for circular data}
@section[#:tag "lec18"]{3/10: Stateful objects (1)}
@section[#:tag "lec19"]{3/14: Stateful objects (2)}
@section[#:tag "lec20"]{3/17: Abstracting with mixins}
@section[#:tag "lec21"]{3/21: Functional objects}

@racketblock[
  @code:comment{Estimate the derivative of the given function.}
  @code:comment{[Number -> Number] -> [Number -> Number]}]
@defs+int[#:eval the-eval
 ((check-expect ((deriv sqr) 5) 10)
  (check-within ((deriv sin) 5) (cos 5) ε)
  (check-within ((deriv cos) 5) (- (sin 5)) ε)
  
  (define ε #e0.000001)
  (define (deriv f)
    (λ (x)
      (/ (- (f (+ x ε))
            (f (- x ε)))
         (* 2 ε)))))
 
 ((deriv sin) 5)]

@section[#:tag "lec22"]{3/24: Traversals}
@section[#:tag "lec23"]{3/28: Visitors}
@section[#:tag "lec24"]{3/31: Object equality: principles}
@section[#:tag "lec25"]{ 4/4: Object equality: practice}
@section[#:tag "lec26"]{4/7: Intermezzo: Ruby}
@section[#:tag "lec27"]{4/11}
@section[#:tag "lec28"]{4/14}
@section[#:tag "lec29"]{4/18 - Patriots}

There is no lecture on 4/18 since it is Patriots Day.
