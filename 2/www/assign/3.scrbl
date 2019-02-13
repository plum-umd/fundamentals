#lang scribble/manual
@(require scribble/core)
@(require "../utils.rkt")
@(define (colorize c . content)
  (elem #:style (style #f (list (color-property c)))
        content))

@assn-title[3]{To Java with Class}

This is assignment is to be completed and submitted individually. You
may not work with anyone else.

@bold{Due}: Tuesday, February 19, 11:59:59 PM EST.

@section[#:style 'unnumbered #:tag "assign3:java"]{Install Java and IntelliJ}

You will need Java and the IntelliJ development environment to
complete this assignment.  If you do not have both installed, follow
the instructions included in @labref{4}.

@section[#:style 'unnumbered #:tag "assign3:download"]{Download the assignment project}

For this assignment, download the following zip file:
@link["Assign3.zip"]{Assign3.zip}.  Save the file somewhere on your
computer and unzip it.  This will create a directory called
@tt{Assign3} with an IntelliJ project inside.  It contains all the
libraries you will need and some code to get you started.

Open IntelliJ and select "Open".  Navigate to the @tt{Assign3}
directory and select it.

This should open up the project and place you inside the
@tt{Assign3.java} file.  The first thing you should do is edit the
authors line (which should be selected when you open the project) to
be your directory ID.

The project has a "Test" configuration in the top right corner.  Press
the "Run" (green triangle) button to run the test suite.  You should
see 4 tests pass, confirming everything is set up properly.

@section{Data Definitions and Methods in Java}

Revisit your solution to @assnref{1}.  You may also use the following
reference solution: @tt{@link["assign1-soln.rkt"]{assign1-soln.rkt}}.

Your task is to redesign a solution to assignment 1 in Java.  A
solution for the first problem is already included in the code given
to you.  For the remaining problems 2-6, translate the @tt{class} code
into Java, making sure to include data definitions, tests, and purpose
statements.

@section[#:style 'unnumbered #:tag "assign3:submit"]{Submission}

Use @tt{submit.cs.umd.edu} to submit your solution to the problems.
You should create a zip file called @tt{Assign3.zip} that contains the
IntelliJ project containing your solutions.
