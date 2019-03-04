#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")
@(require "../utils.rkt")

@lab-title[9]{Fold!}

@section[#:style 'unnumbered #:tag "lab9:intro"]{Intro}

Work in ad-hoc pairs.  The two of you will work as a team to solve
problems. At any time, one of you will be the @bold{Head} and the
other will be the @bold{Hands}. The @bold{Head} does the thinking and
the @bold{Hands} does the typing. @bold{Hands} type only what the
@bold{Head} tells them to, but you're free to discuss any issues that
pop up. We'll have you switch off during the lab to make sure each of
you get practice problem solving, dealing with syntax, and getting
finger exercises on the keyboard.


@section[#:tag "lab9:init"]{Lab Skeleton}

You must start this lab with @link["Lab9.zip"]{this project
skeleton}. Unzip the file into your IdeaProjects directory and open
it with IntelliJ to get started.

It contains the code we wrote in class today.

@section[#:tag "lab9:fold"]{Design Foldr}

You've now seen how to implement @tt{map}.  Go for the gold and add
@tt{foldr} to your list interface.

You will need to define a new interface for representing binary
functions, i.e. functions that consume two inputs.

@section[#:tag "lab9:usefold"]{Use foldr for everything}

TBD.


