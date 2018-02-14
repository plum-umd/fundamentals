#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")
@(require "../utils.rkt")

@lab-title[5]{Distances, Speeds, and Times in Java}

@section[#:style 'unnumbered #:tag "lab5:intro"]{Intro}

You'll work in this lab with your
@link["https://piazza.com/class/jcspfhewmdn41y?cid=27"]{assigned
partner}. Help each other get up and running with IntelliJ and Java
(@labref{4}). The two of you will work as a team to solve problems. At
any time, one of you will be the @bold{Head} and the other will be the
@bold{Hands}. The @bold{Head} does the thinking and the @bold{Hands}
does the typing. @bold{Hands} type only what the @bold{Head} tells
them to, but you're free to discuss any issues that pop up. We'll have
you switch off during the lab to make sure each of you get practice
problem solving, dealing with syntax, and getting finger exercises on
the keyboard.

@section[#:tag "lab5:lab1"]{Lab 1 Redux}

Our goal for today's lab is to re-implement the first lab (@labref{1})
in Java. If you've already completed lab 1 you're in luck--the
translation from @tt{class/0} should be straightforward.

You should translate tests to Java comments; we'll introduce a
@tt{check-expect} style testing mechanism next week.
