#lang scribble/manual
@(require scribble/core 
          scribble/examples
          racket/sandbox
          (for-label lang/htdp-beginner) 
          (for-label (except-in 2htdp/image image?))
          ;"helper.rkt" 
	  "../utils.rkt"
          "../defns.rkt")


@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (only-in lang/htdp-intermediate check-expect)))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (only-in lang/htdp-beginner identity string-whitespace?)))
    (the-eval '(require (prefix-in r: racket)))
the-eval))





@title[#:style 'unnumbered #:tag "lab3"]{Lab 3: Genius Bar}

@(define ex (make-exerciser "Lab problem"))


@section[#:tag "lab3intro"]{Introduction(s)}

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

@section[#:tag "lab3:purpose"]{Purpose}

In this lab, you'll practice writing functions and building simple
world programs using the @bold{design recipe} for systematic
computational problem solving.

@section{A simple interactive program}

@secref{lab2} introduced you to the world of @racket[big-bang]
programs.  However, fundamentally, the program you wrote was just a
fancier version of @racket[animate] programs you had already seen: the
program did not respond to any interactions from the user.

In this lab, you will write a very simple interactive program that
responds to user actions.

You will make a program that shows a progress bar that graphically
displays the percentage of some task being complete (e.g. downloading
a file, installing some software, starting DrRacket).

The first version of the program should assume progress is made at a
uniform rate of %1 per tick of time.  You will need to use the
@racketmodname[2htdp/image] and @racketmodname[2htdp/universe]
libraries to complete this lab. 

@racketblock[
(require 2htdp/image)
(require 2htdp/universe)]


The main function for this program will look like this:

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; main : Integer -> Integer
;; Launch progress bar at given percentage level (between 0 and 100)
(define (main x)
  (big-bang x
    [on-tick progress-forward]
    [to-draw progress-draw]))
}|

Your job is to design @racket[progress-forward] and
@racket[progress-draw].

We will add functionality to handle user input shortly.  For the time
being, the program should display a rectangular scene with a red
rectangle aligned on the left side.  The width of the rectangle should
correspond to the percentage of progress that has been made.  When 0%
progress has been made, the rectangle should be 0 px wide.  When 100%
progress has been made, the rectangle should be as wide as the scene.

@ex["Problem analysis and data definition"]{

Consider the description above.  Express how you wish to represent information as data.

}


@ex["Signatures, purpose statements"]{

Consider the use of @racket[progress-forward] and
@racket[progress-draw].  Write down a signature, a statement of
purpose, and a function header for both functions.  The purpose
statement should answer the question: @emph{what does the function
compute?}

}

@ex["Examples"]{

Illustrate the use of @racket[progress-forward] and
@racket[progress-draw] with examples and their expected outcome.

}

@ex["Inventory"]{

Take an inventory of what each function has to compute with.

}

@ex["Code"]{

Guided by your example, signature, and purpose statement, complete the
definitions of @racket[progress-forward] and
@racket[progress-draw].

}

@ex["Test"]{

Using @racket[check-expect], turn your earlier examples in to tests
and confirm your functions behave as expected.  Revise your program if
things do not work as intended.

}


Now consider adding the ability for the user to pause and unpause the
progress by pressing any key.

To handle key events, we need to add another clause to our
@racket[big-bang] and will need to design another function.


@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; main : Integer -> Integer
;; Launch progress bar at given percentage level (between 0 and 100)
;; Allow user to pause and unpause with press of a key
(define (main x)
  (big-bang x
    [on-tick progress-forward]
    [to-draw progress-draw]
    [on-key progress-pause]))
}|


Revisit problems 1-6 with this revised description of the program.

Finally, consider yet another design revision where only pressing the
space-key pauses and unpauses progress.  Revise as necessary.

