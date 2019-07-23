#lang scribble/manual
@(require scribble/core 
          scribble/examples
          racket/sandbox
          (for-label lang/htdp-beginner) 
          (for-label (except-in 2htdp/image image?))
          ;"helper.rkt" 
	  "../utils.rkt"
          "../defns.rkt")

@title[#:style 'unnumbered #:tag "ex7"]{Exercise 7}

@bold{Due}: Monday, July 22, 11:59:59 PM EST. 

@(define ex (make-exerciser "Problem"))

Implement these exercises with the
@link["https://docs.racket-lang.org/htdp-langs/intermediate.html"]{Intermediate
Student Language}.

@section[#:tag "ex7:submit"]{Directions for submitting}

Please read and follow these intructions carefully.  You should submit
a @bold{single} file named @tt{ex4.rkt} on @link[elms-url]{ELMS}.  You
may submit many times, but each submission should be one file called
@tt{ex7.rkt}.  You may lose points for not following these
instructions.

Make sure the top of the file contains the following, with your name filled in:
@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; Exercise 7
;; Name: ...your name here...
}|

@section[#:tag "ex7:overview"]{Oveview}

The goal of this exercise is to practice using the abstraction
functions for lists.

@section[#:tag "ex7:problems"]{Using abstraction functions}

@ex["Redo with abstraction functions"]{

Revisit your solution to @secref{ex5}.  Revise your solution to use
the list abstraction functions @racket[andmap], @racket[ormap],
@racket[map], @racket[filter], and @racket[foldr] wherever
appropriate.

}
