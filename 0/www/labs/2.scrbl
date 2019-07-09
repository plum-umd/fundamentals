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
    (the-eval '(require (only-in lang/htdp-beginner identity)))
    (the-eval '(require (prefix-in r: racket)))
the-eval))




@title[#:style 'unnumbered #:tag "lab2"]{Lab 2: A World of Fun}

@(define ex (make-exercise))


@section[#:tag "lab2intro"]{Introduction(s)}

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

@section{Purpose}

In this lab, you'll practice writing functions and building simple
world programs.  Along the way, you'll be exposed to some new
operations for computing with strings and generating random numbers.

@section{Deteriorating Texts}

Your task is to design a program that displays a message that
gradually deteriorates by losing letters until it has totally
vanished.  After all the letters have vanished, it should reappear and
deteriorate again.  @image[#:scale 1/2 #:style
float-right]{img/blanking.gif} An example is shown to the right, in
which the phrase "veritas numquam perit" ("truth never perishes")
deteriorates.  (Note: the example is a looping GIF, so the phrase
deteriorates in the same way every time; your program should have the
phrase deteriorate randomly each time.)


You will need the @racketmodname[2htdp/image] and
@racketmodname[2htdp/universe] libraries to complete this lab.


To help get you started, here is a function that renders a string as
an image using a fixed-width font.  It uses the
@racketmodname[2htdp/image] function @racket[text/font] for selecting
a font face.  The details here are not important (although you can
read the documentation if you're interested).  What is important is
that if given two strings of the same length, @racket[text-tt] will 
produce images of the same width.

@examples[
  #:eval the-eval
(define (text-tt s)
  (text/font s 60 "black" "Courier" "default" "normal" "bold" #false))

(text-tt "Hello!")
]

Here you can see that strings of the same length produce images of the
same width:

@examples[
  #:eval the-eval
(text-tt "George")
(text-tt "Scooby")
]

To see why this is helpful, consider these examples.

@examples[
  #:eval the-eval
(text-tt "Scooby")
(text-tt "S ooby")
(text-tt "S o by")
]


We can now set-up the use of @racket[big-bang] to illustrate the basic
idea of how this program will work.  First we define a constant for
the message we want to display when undeteriorated:

@codeblock[#:keep-lang-line? #false]|{
#lang racket
(define MSG "Truth never perishes")
}|

The main function for this program will consume a string representing
the message to be displayed.  It uses @racket[text-tt] to display the
message.  Here is a first attempt at the main function:

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; Show s as it deteriorates until gone and reappears as MSG
(define (main s)
  (big-bang s
    [to-draw text-tt]
    [on-tick identity]))
}|

This use of @racket[big-bang] represents the state of the world as a
string: the message in its current state of deterioration.  Initially
it is just the complete message you want to display.

You can try this out by running:
@codeblock[#:keep-lang-line? #false]|{
#lang racket
(main MSG)
}|

Notice that this works, except that nothing ever changes.  That's due
to @racket[main]'s use of the @racket[identity] function.  The
@racket[identity] function has a simple purpose: it simply gives back
as a result any value it's given as an argument.

@examples[
  #:eval the-eval
(identity 7)
(identity "Wilma")
(identity (text-tt "Yo"))
]

While @racket[identity] may seem like a useless function, it serves a handy role here
as a placeholder for an @racket[on-tick] event handler function.

To complete this labb, you will to replace @racket[identity] with a
function of your own design that causes the current string to
deteriorate one letter and restart once all letters are gone.

To accomplish this, we suggest writing @bold{two} functions.  One
takes a string and replaces some randomly selected position of the
string with a space.  The second takes a string, determines if it
consists only of whitespace (i.e. all spaces) and if so, produces
@racket[MSG], otherwise it uses the first function to deteriorate.

Some functions worth knowing about to complete this lab:
@racket[random], @racket[string-whitespace?], @racket[substring],
@racket[string-append], @racket[string-length].  Read the
documentation for these functions, try out some examples, then try to
solve the rest of this lab.

