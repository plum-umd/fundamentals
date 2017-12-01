#lang scribble/manual
@(require "utils.rkt"
	  "unnumbered.rkt"
	  scribble/eval
          racket/sandbox
          (for-label lang/htdp-intermediate-lambda)
          #;(for-label class/0))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require lang/htdp-intermediate-lambda))
   ;(the-eval '(require class/0))
    (call-in-sandbox-context 
     the-eval 
     (lambda () ((dynamic-require 'htdp/bsl/runtime 'configure)
                 (dynamic-require 'htdp/isl/lang/reader 'options))))
    the-eval))

@title*[]{Lectures}

In this section, you'll find brief notes from each lecture.  For more detailed
notes, please see the book.

@;@local-table-of-contents[#:style 'immediate-only]

@section[#:tag "lec01"]{1/7: Objects}

@itemlist[
 @item{Announcements
   @itemlist[
     @item{Course staff introductions.}
     @item{Basic class mechanics.}
     @item{On the experimental nature of this course.}
     @item{The @seclink["assign01"]{first assignment}
           is due @bold{this} Wednesday.}
     @item{The @seclink["lab01"]{first lab} is @bold{tonight}.}
     @item{Partners will be assigned in lab.}     
     @item{Questions?}]}
 @item{Basics of objects
   @itemlist[
     @item{New paradigm.  Open your mind and embrace it (or you will be miserable).}
     @item{Rocket, designed in functional style.}
     @item{Rocket, designed in object-oriented style.}
     @item{Landing and take off.}
     @item{Moon.}]}]

If you don't have a CCIS account, you need to get one TODAY.  

@section[#:tag "lec02"]{1/10: Data definitions and functionality with classes}

@section[#:tag "lec03"]{1/14: Interfaces}

@section[#:tag "lec04"]{1/17: Zombies}

@section[#:tag "lec05"]{1/21: No Class}
MLK Day

@section[#:tag "lec06"]{1/24: Delegation, Zombies II}

@section[#:tag "lec07"]{1/28: Universe and accumulator invariants}

@section[#:tag "lec08"]{1/31: Inheritance}

@section[#:tag "lec09"]{2/4: Properties and random testing}

@section[#:tag "lec10"]{2/7: Function objects and parameterized data
definitions}

@section[#:tag "lec11"]{2/11: Overrriding and default worlds and
universes}

@section[#:tag "lec12"]{2/14: Black-box testing, invariants, amortized
analysis, and function constructors}

@section[#:tag "lec13"]{2/18: Holiday (Presidents Day)}
There is no lecture on 2/18 since it is Presidents Day.

@section[#:tag "lec14"]{2/21: Constructors and visitors}

@section[#:tag "lec15"]{2/25: Java I: Syntax and Semantics}

@section[#:tag "lec16"]{2/28: Java II: Types + Project Intro}

@section[#:tag "lec17"]{3/4: Holiday (Spring Break)}
There is no lecture on 3/3 since it is during Spring break.

@section[#:tag "lec18"]{3/7: Holiday (Spring Break)}
There is no lecture on 3/6 since it is during Spring break.

@section[#:tag "lec19"]{3/11: Mutation I}

@section[#:tag "lec20"]{3/14: Mutation II}

@section[#:tag "lec21"]{3/18: Overriding}

@section[#:tag "lec22"]{3/21: Equality I}

@section[#:tag "lec23"]{3/25: Equality II}

@section[#:tag "lec24"]{3/28: Implementing OO}

@section[#:tag "lec25"]{4/1: Mixins and Prototypes}

@section[#:tag "lec26"]{4/4: Slack}

@section[#:tag "lec27"]{4/8: Slack}

@section[#:tag "lec28"]{4/11: Slack}

@section[#:tag "lec29"]{4/15: Holiday (Patriot's Day)}
There is no lecture on 4/15 since it is Patriot's Day.