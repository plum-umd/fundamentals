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

@title*[#:style 'toc]{Lectures}

In this section, you'll find brief notes from each lecture.

@local-table-of-contents[#:style 'immediate-only]

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
     @item{Partners have already been assigned.  See the @seclink["Blog"]{blog}.

     Make sure to check your CCIS username is correct in the pair
     assignments on the blog.  Make sure your partner exists.}     
     @item{Questions?}]}
 @item{Basics of objects
   @itemlist[
     @item{New paradigm.  Open your mind and embrace it (or you will be miserable).}
     @item{Rocket, designed in functional style.}
     @item{Rocket, designed in object-oriented style.}
     @item{Landing and take off.}
     @item{Moon.}]}]

If you don't have a CCIS account, you need to get one TODAY.  Make
sure we have your CCIS username on the @seclink{Blog}.

@section[#:tag "lec02"]{1/10: Data definitions and functionality with classes}

@section[#:tag "lec03"]{1/16: Holiday (MLK Day)}
There is no lecture on 1/16 since it is Martin Luther King, Jr. Day.

@section[#:tag "lec04"]{1/19: Interfaces and Space Invader design}

[Sam will be in California.]

@section[#:tag "lec05"]{1/23: Zombies}

@section[#:tag "lec06"]{1/26: Delegation, Zombies II}

@section[#:tag "lec07"]{1/30: Universe and accumulator invariants}

@section[#:tag "lec08"]{2/2: Inheritance}

@section[#:tag "lec09"]{2/6: Properties and random testing}

@section[#:tag "lec10"]{2/9: Function objects and parameterized data
definitions}

@section[#:tag "lec11"]{2/13: Overrriding and default worlds and
universes}

@section[#:tag "lec12"]{2/16: Black-box testing, invariants, amortized
analysis, and function constructors}

@section[#:tag "lec13"]{2/20: Holiday (Presidents Day)}
There is no lecture on 2/20 since it is Presidents Day.

@section[#:tag "lec14"]{2/23: Constructors and visitors}

@section[#:tag "lec15"]{2/27: Java I: Syntax and Semantics}

@section[#:tag "lec16"]{3/1: Java II: Types}

@section[#:tag "lec17"]{3/5: Holiday (Spring Break)}
There is no lecture on 3/5 since it is during Spring break.

@section[#:tag "lec18"]{3/8: Holiday (Spring Break)}
There is no lecture on 3/8 since it is during Spring break.

@section[#:tag "lec19"]{3/12: Project Intro}

@section[#:tag "lec20"]{3/15: Operating Systems}

@section[#:tag "lec21"]{3/19: Overriding}

@section[#:tag "lec22"]{3/22: Functions}

@section[#:tag "lec23"]{3/26: Equality}

@section[#:tag "lec24"]{3/29: Data Structures and Algorithmics}

@section[#:tag "lec25"]{4/2: Java III: Libraries}

@section[#:tag "lec26"]{4/5: Implementing OO}

@section[#:tag "lec27"]{4/9: Mixins and Prototypes}

@section[#:tag "lec28"]{4/12: Slack}

@section[#:tag "lec29"]{4/16: Holiday (Patriot's Day)}
There is no lecture on 3/8 since it is Patriot's Day.