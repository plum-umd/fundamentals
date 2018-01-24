#lang scribble/manual
@(require racket/sandbox
          scribble/example)


@title[#:style 'unnumbered #:tag "style"]{The Style}

In addition to following the design recipe, all code must adhere to
the following basic style guidelines:

@itemlist[#:style 'ordered

@item{@bold{Start every file with a standard header}:

@#reader scribble/comment-reader
(racketblock
;; Authors: <directoryID1>, <directoryID2>
;; Purpose: <concise, high-level purpose statement for program>
)

These should be the @bold{first two lines} of the file.
}

@item{@bold{Organize your program top-down,} regardless of how you
actually work through your wish list. The phrase "top down" means that
project files consist of a general purpose statement, a data
definition and a constant definition section, a main function,
followed by sections for handler functions, and wrapped up by general
utility functions.

The @emph{main} function is the one that uses @tt{big-bang},
@tt{read-file}, @tt{write-file}, and so on. A good purpose statememt
for the main function explains how to use it. For example,

@#reader scribble/comment-reader
(racketblock
; PortNumber -> ClientState
; (client p) connects to port p on
; the SERVER machine, deals with incoming messages, displays
; ...
; Try with (client 10002)
(define (client port-no)
  (big-bang CLIENTSTATE0
    [register SERVER]
    [port     port-no]
    ...
    [to-draw render-client-state]))
)

Note the specific sample function call.

Separate distinct sections of your program with dashed lines that are
exactly 80 columns wide.}

@item{@bold{Arrange your functions according to the design recipe.}
Place short and informative test cases between the signature and the
function definition. Place any additional long or complicated test
cases below the function or at the bottom of your program in a
separate test section.}

@item{Use (function, constant, parameter) @bold{names that make sense}
with respect to the problem.}

@item{@bold{Design concise functions.} No function should span more
than five to eight lines. If it does, reconsider your interpretation
of the "one task, one function" guideline.

If a function consumes a complex argument and must perform several
different tasks, design several functions that all consume the
argument, produce a value in the same data collection, and hand it
over to the next function:

@verbatim{
  ;; ------------------------------------------------- GOOD
  ;; HHState -> HHState

  (define (hungry-henry-action-per-clock-tick hh-world-state)
    (bump-tick-value
      (eat-all-cup-cakes-in-reach
         (move-hungry-henry-closer-to-wp hh-world-state))))

  ;; HHState -> HHState
  (define (bump-tick-value hh-world-state) ...)

  ;; HHState -> HHState
  (define (eat-all-cup-cakes-in-reach hh-world-state) ...)

  ;; HHState -> HHState
  (define (move-hungry-henry-closer-to-wp hh-world-state) ...)
}

Piling the code from these three functions into the first one would
yield a confusing mess.}

@item{@bold{Keep lines narrow.} No line should span more than 80
characters. See bottom right of DrRacket or use its @tt{edit -> find
longest line} menu.

Break lines at those points suggested by HtDP.}

@item{@bold{Use proper indentation.} Use the indentation style of
DrRacket in your program. To indent a selected portion of your file,
press tab.  Ctrl+A is useful to select all of the file.}

@item{Programs use the parentheses style displayed in HtDP and this
web page:

@verbatim{
;; ------------------------ GOOD
(define (f l)
  (cond [(empty? l) 0]
        [else (f (rest l))]))

;; ------------------------ BAD
(define (f l)
  (cond [(empty? 1) 0]
        [else (f (rest l))]
  )
)
}

The dangling parentheses in the second code excerpt are considered
@bold{extremely bad style}. You will lose @bold{all} style points for
using it even once.}

]

Not observing these very basic guidelines leads to unreadable code and
to loss of points.