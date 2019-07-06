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
    (the-eval '(require (prefix-in r: racket)))
the-eval))

@(define (colorize c . content)
  (elem #:style (style #f (list (color-property c)))
        content))

@title[#:style 'unnumbered #:tag "ex1"]{Exercise 1}


@bold{Due}: Monday, July 8, 11:59:59 PM EST.

This exercise will be released July 8.

@;{

@section{Start}

Open DrRacket.  Make sure your language is set to Beginning Student.
At the top of the file, write the following, except use your actual name in
place of @italic{... your name here...}:

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; Exercise 1
;; Name: ...your name here...
}|

Save the file on your computer with the name @tt{ex1.rkt}.



@section{Boring Arithmetic}

This will be the only assignment completed individually.  Subsequent
assignments will be completed with partners.

@itemize[
@item{Open up DrRacket, and choose the "Beginning Student
Language".}

@item{Start using the @racketmodname[2htdp/image] library, as
described in the
@link["https://htdp.org/2018-01-06/Book/part_prologue.html"]{Prologue}
of the book. Read all the way to where images are discussed!}

@item{Using this library, create a program that when you hit the "Run"
button, it produces a 200x200 pixel image that is your own personal
design for an "Emoji" (a small digital image or icon used to express
an idea, emotion, etc., in electronic communication).

You may use any of the @racketmodname[2htdp/image] operations to
construct this image, but you should not include any copy/pasted
images.  You may try to recreate an existing Emoji or design a
completely new one.

You will be graded solely on the basis of producing a 200x200 image.
Your design may be as simle or as complicated as you like.  Use this
assign to explore the image library and experiment with
programmaticaly constructing images.}

@item{Save and submit the file on ELMS. You
may submit as many times as you'd like (and we recommend submitting
early and often).  The latest submission before the deadline will be
graded.  @bold{Do not email submissions to course staff.}}]

}