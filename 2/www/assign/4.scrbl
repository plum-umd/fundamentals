#lang scribble/manual
@(provide readings)

@title[#:style 'unnumbered #:tag "assign4"]{Assignment 4: Style and design}

@bold{Due:} Friday, September 22, 11:59:59 PM EST.

The following should be completed in cooperation with your assigned
partner from lab 1. (Partner assignments are listed on Piazza.)

This is the second assignment where you must use the design recipe and
@secref{style} guidelines to receive full credit.  We will grade these
aspect much more severely from now on.

@section[#:tag "assign4:prep"]{Preparation}

@(define readings
  @elem{@emph{all} of
@link["http://www.ccs.neu.edu/home/matthias/HtDP2e/part_one.html"]{Part
I}})

Make sure you have read and studied @emph{all} of
@link["http://www.ccs.neu.edu/home/matthias/HtDP2e/part_one.html"]{Part
I} of HtDP2e.

@section[#:tag "assign4:editor"]{Graphical text editor}

Create a file @tt{editor.rkt} for this part of the assignment.  Use
the standard file header at the top of the file.

Complete exercises 83--87 from
@link["http://www.ccs.neu.edu/home/matthias/HtDP2e/"]{HtDP2e}.

@section[#:tag "assign4:chip"]{Chip the Cheap Sheep Cashes Out}

Create a file @tt{chip.rkt} for this part of the assignment.  Use the
standard file header at the top of the file.  Complete the design of
the Chip program @secref{September_15__2017}.


Create a file @tt{chips.rkt} and use the standard file header.  Design
a variant of the Chip program that has two sheep that start off on
opposite sides of the screen and run toward eachother.  When their
heads butt, they should ``bounce'' off running the other direction.
The left sheep (notice that the sheep can never pass each other)
should be controlled with the @tt{"a"} and @tt{"d"} keys for changing
direction toward the left and right, respectively.  The right sheep is
controlled with the left and right arrow keys as before.

Here's a little animation of a sample running of this program:

@image{img/chips.gif}

(You do not need to add the ``POW!'' functionality, but may if you
have the time.)

It should go without saying that this program should be designed using
the design recipe and style guidelines.  In particular make sure you
use defined constants so that parameters of the program can be changed
without breaking anything.

@section[#:tag "assign4:calendar"]{Revising the calendar program}

Create a file @tt{calendar-revised.rkt} for this part of the
assignment.  Use the standard file header at the top of the file.

Revise your calendar program from @secref{assign3} to address all
comments from the grader and make any improvements you think
appropriate.

Make sure your program follows the design recipe for all functions.

Make sure your program follows the style guidelines.

At a mininum, your program:
@itemize[

@item{Should have no lines longer than 80 columns.}

@item{Should have no text move if you do Ctl+A (select all) and hit
tab (this is using auto-indent to check that you're following the
usual indentation conventions).}

@item{Should follow the standard file header outlined in the style
guidelines.}

@item{Should use the names of functions given to you in the problem
description.  @bold{You will receive no points for any function that
has a different name than what is specified.}}
]

Additionally, make explicit template definitions for the following
data definitions: @tt{Month}, @tt{MonthFormat}, @tt{DateOrder}, and
@tt{DaysInYear}.

There are a list of common problems in the class notes for
@secref{September_18__2017}.

@section[#:tag "assign4:submission"]{Project submission}

You should submit all files: @tt{editor.rkt}, @tt{chip.rkt},
@tt{chips.rkt}, and @tt{calendar-revised.rkt}.

Submit your files directly to the submit server by uploading them.
Select each of these files individually using the ``Browse'' button.
Once you have selected all the files, press the ``Submit project!''
button. You do not need to put them in a zip file.
