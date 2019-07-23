#lang scribble/manual
@(require scribble/core 
          scribble/examples
          racket/sandbox
          (for-label lang/htdp-beginner) 
          (for-label (except-in 2htdp/image image?))
          ;"helper.rkt" 
	  "../utils.rkt"
          "../defns.rkt")

@title[#:style 'unnumbered #:tag "ex8"]{Exercise 8}

@bold{Due}: Tuesday, July 23, 11:59:59 PM EST.

@section[#:tag "ex8:overview"]{Revising Assignment 1}

Revise your solution to @secref{assign1} to fix any issue and improve your
code.  Submitting a revision earns you full credit.  Your revisions
will be graded and substituted for your assignment 1 grade (if it is
higher than your original grade).

You must do the following to get credit (not doing these things means
you will not only not get credit for this exercise, but your
assignment 1 score will remain unchanged):

@itemlist[

@item{Name your file @tt{assign1-revised.rkt}; submit this and only this file.}

@item{Make sure the top of your file contains the following:
@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; Assignment 1, revised
;; Name: ...your name here...
}|
}

@item{Below your name, add a section of comments that has a bulleted
list of changes.  For example:
@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; CHANGES:
;;
;; * Added signatures for all functions.
;; * Added tests for image producing functions.
;; * Properly indented code.
;; * Fixed food and bug collision detection.
}|
Graders will only check for the changes you list here, so if you do work that you want
credit for, you need to describe it concisely in this section.
}

]

@bold{If you have no changes to make, you must still follow the above
directions and write ``no changes'' in the CHANGES section in order to
get credit for the exercise; your assignment 1 score will stay as it is.}



Things that are worth doing to improve your score on assignment 1:

@itemlist[

@item{Follow all the directions of the assignment.}

@item{Make sure the program does not crash while playing.}

@item{Make sure the program behaves correctly when playing.}

@item{Follow the design recipe.}

@item{For every function, have a correct signature.}

@item{For every function, have a concise, descriptive purpose
statement.}

@item{For every function, the design should either follow the
structure of the data or consist of a composition of other functions.}

@item{For every function except main, have a good set of tests that
serve as examples of using the function.}

@item{For every function except main, have complete test coverage: no
halloween colors left after running the tests.}

@item{Adhere to the @emph{``one task, one function''} mantra.  This
means using helper functions when appropriate.  It should result in
small, readable function definitions that clearly carry out the
function's purpose.}

@item{Use defined constants so that changing the basic parameters of
the game is easy.  Changing constant definitions should not cause
tests to fail or the game to crash or misbehave when playing.}

@item{Make sure code is properly indented.  Select-all + tab should
not move anything is a good baseline.}

]


