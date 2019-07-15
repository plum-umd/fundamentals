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
(define (small left right)
  (add-outline (add-cursor (text left 25 "black")
                           (text right 25 "black"))))

(define (add-outline img)
  (overlay img (empty-scene (image-width img) (image-height img))))

(define (show left right)
  (add-cursor (text left 50 "black")
              (text right 50 "black")))

(define (add-cursor img1 img2)
  (beside img1
          (rectangle 2
                     (max (image-height img1) (image-height img2))                     
                     "solid"
                     "red")
          img2))

]



@title[#:style 'unnumbered #:tag "lab5"]{Lab 4: Stop and Smell the Sheep}

@(define ex (make-exerciser "Lab problem"))


@section[#:tag "lab5intro"]{Introduction(s)}

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

@section[#:tag "lab5:purpose"]{Purpose}

In this lab, you'll practice 
@itemlist[
@item{communicating your own program designs to others,}
@item{understanding programs designed by others,}
@item{adapting existing programs, and}
@item{using itemizations involving structures.}]

@section{Explain Chip to your Partner}

In this lab, you will be adapting one of your two solutions to
@secref{ex3} to add some functionality.  The first step, however, is
to understand both solutions.

Open both you and your partners solution to the Chip exercise and take
turns explaining how you designed the program.  Focus on giving
rationales for the decisions you made.  Go through each problem and
discuss any parts you felt were particular tricky or you struggled
with.

When you're done, select one of the solutions.  It doesn't matter
which since you should both understand both programs.

@section{Possibility of a Pause}

Revise your solution to the Chip problem to add the ability to pause
the game.  When paused, Chip should stop moving and a text message
indicating the game is paused should be overlayed on top of the scene.
When unpaused, the game should resume exactly how it was before: Chip
is moving is the same direction, with the same speed, and from the
same location as when the game was paused.

There are many ways to represent this information, but for this lab,
use the following data definition (in addition to ones given to you in
the exercise) to guide your design:

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; A PPC (possibly paused Chip) is one of:
;; - (make-paused Chip)
;; - Chip
(define-struct paused (chip))

;; Interpretation: (make-paused c) represents the pausing of a Chip
;; game in state c.  Otherwise the game is unpaused.
}|

The world state should now be represented as a @tt{PPC} instead of a
@tt{Chip} and your program should be revised accordingly.  For
example, your @racket[main] function should become something like this:

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; main : Integer -> PPC
;; Launch Chip at given position, moving right slowly
(define (main x)
  (big-bang (make-chip x #true 1)
    [on-tick ppc-move]
    [to-draw ppc-draw]
    [on-key ppc-key]))
}|

where the signatures of @racket[ppc-move], @racket[ppc-draw], and
@racket[ppc-key] should now be, respectively:
@itemlist[
@item{@tt{PPC -> PPC},}
@item{@tt{PPC -> Image}, and}
@item{@tt{PPC KeyEvent -> PPC}.}]

The @racket[ppc-key] function should toggle the paused/unpaused state
of the game whenever the user presses the space key.

@ex[@elem{Template for @tt{PPC}s}]{

Write down a template for @tt{PPC} consuming functions.

}

@ex[@elem{Revising for @tt{PPC}s}]{

Revise your program to work with @tt{PPC}s.  (Hint: you may want to
add new @tt{PPC} functions rather than modifying your @tt{Chip}
functions to work on @tt{PPC}s.)

}


