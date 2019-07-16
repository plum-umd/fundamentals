#lang scribble/manual
@(require scribble/core 
          scribble/examples
          racket/sandbox
          (for-label lang/htdp-beginner) 
          (for-label (except-in 2htdp/image image?) 2htdp/universe)
          ;"helper.rkt" 
	  "../utils.rkt"
          "../defns.rkt")

@(define-syntax-rule (result e) 
   @examples[#:eval the-eval #:result-only e])


@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (only-in lang/htdp-intermediate check-expect)))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (only-in lang/htdp-beginner identity string-whitespace?)))
    (the-eval '(require (prefix-in r: racket)))
the-eval))


@examples[#:eval the-eval #:hidden
;; place code here
]

@title[#:style 'unnumbered #:tag "lab6"]{Lab 6: A John Woo Joint}

@(define ex (make-exerciser "Lab problem"))


@section[#:tag "lab6intro"]{Introduction(s)}

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

@section[#:tag "lab6:purpose"]{Purpose}

In this lab, you'll practice 
@itemlist[
@item{refining programs designed by others and}
@item{using lists to represent arbitrarily large collections of data.}]

@section{A Simplified Space Invader}

In this lab, you will be adapting a program designed to implement (the
simplified beginnings of) a
@link["https://en.wikipedia.org/wiki/Space_Invaders"]{Space Invader}
style game.

In this simplified version of the game, a Duke student who's behind on
their HtDP reading has implemented the base and shooting functionality
of the game.  Unfortunately, only one shot at a time can be fired.
@image[#:style float-right]{img/space-invader.png}
Your task is to understand the Duke student's program (luckily they
did follow the design recipe, so their code should be fairly clear)
and adapt the program so that the player can shoot an unbounded number
of shots by repeatedly pressing the space key.


Here are the critical data definitions and the main function of their
program:

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main

;; main : Number -> Game
;; Launches a game of Space Invaders (simplified)
;; Example: (main 0)
(define (main n)
  (big-bang (make-si (+ n (* 1/2 WIDTH)) #false)
    [to-draw si-draw]
    [on-tick si-advance]
    [on-key si-handle-key]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; A SI (Space Invader game) is a (make-si Base MaybeShot)
;; Interpretation: x is the x-coordinate of the base, s is the current shot 
(define-struct si (x s))

;; A Base is an Integer

;; A MaybeShot is one of:
;; - #false
;; - Shot
;; Interpretation: shot or #false to indicate no shot

;; A Shot is a (make-posn Integer Integer)
;; Intepretation: coordinates of a shot position
}|

The complete code is available here: @link["invader.rkt"]{@tt{invader.rkt}}.
Download and open in DrRacket (copy and pasting will not work).
Read and understand the code.  Give it a try with @racket[(main 0)].

@section{From @tt{MaybeShot} to @tt{Shots}}

As you'll notice in the design above, a Space Invader game state
includes a @tt{MaybeShot} which represents either 0 or 1 shot having
been fired.  Better would be to represent an arbitrarily large
collection of shots.

@ex["Shots"]{

Develop a data definition called @tt{Shots} for representing an
arbitrarily large collection of shots.

Revise the data defintions of @tt{SI} to use @tt{Shots}.

Revise @racket[main] so that the initial state of the game uses @tt{Shots}.
}


@ex["Shots functions"]{

Design the following functions for operating on shots:

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; Shots -> Shots
;; Advance all shots in the list upward.
(define (shots-advance ss) ss)

;; Shots -> Shots
;; Remove any shot in the list that has gone off-screen.
(define (shots-remove-offscreen ss) ss)

;; Shots -> Shots
;; Advance all shots and remove any that are off-screen
(define (shots-advance/remove ss) ss)

;; Shots Image -> Image
;; Draw each of the shots on given image
(define (shots-draw-on ss scn) scn)

;; Shots Integer -> Shots
;; Add a new shot to given list at position x
(define (shots-new ss x) ss)
}|

}

@ex["Revise SI"]{

Revise @racket[si-draw], @racket[si-advance], and
@racket[si-handle-key] to use the functions you designed above.

}


