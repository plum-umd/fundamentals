#lang scribble/manual
@(require scribble/core)
@(require "../defns.rkt")
@(define (colorize c . content)
  (elem #:style (style #f (list (color-property c)))
        content))

@title[#:style 'unnumbered #:tag "assign1"]{Assignment 1: Defining Classes of Data}


@bold{Due}: Tuesday, January 30, 11:59:59 PM EST.

This assignment is to be completed individually.  You may not work
with a partner.

@section[#:style 'unnumbered #:tag "assign1:piazza"]{Sign up for Piazza}

Our primary means of communication and answering questions will be the
course @link[piazza-url]{Piazza} message board. Sign up.


@item{Save and submit the file using the
@link["https://submit.cs.umd.edu/"]{submit server} for @tt{ps1}.  Only
one partner needs to submit on behalf of the pair.  You may submit as
many times as you'd like (and we recommend submitting early and
often).  The latest submission before the deadline will be graded.
@bold{Do not email submissions to course staff.}}]
