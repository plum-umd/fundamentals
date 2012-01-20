#lang scribble/manual
@(require #;"utils.rkt"
          #;"unnumbered.rkt")

@title{Fundamentals II
       @linebreak[]
       Introduction to Class-based Program Design}

@larger{@bold{Spring, 2012}}

The course studies the class-based program design and the design
of abstractions that support the design of reusable software and
libraries. It covers the principles of object oriented program 
design, the basic rules of program evaluation, and examines the 
relationship between algorithms and data structures, as well as 
basic techniques for analyzing algorithm complexity.

The course is suitable for both CS majors and non-majors.  It 
assumes that student has been introduced to the basic principles 
of program design and computation. 

@bold{Prerequisites}

@centered{@emph{``Think first, experiment later.''}}

The course assumes proficiency with the systematic design of 
programs and some mathematical maturity. It demands curiosity 
and self-driven exploration and requires a serious commitment 
to practical hands-on programming. 

@bold{Course objectives}

The goal is to help you understand the principles of class-based
program design using an object-oriented programming language(s), not
just Java. Java is used so we can learn how the principles are used in
practical applications, and gives us an opportunity to discuss the
strengths and weaknesses of languages and paradigms.

@include-section{general.scrbl}
@include-section{texts.scrbl}
@include-section{syllabus.scrbl}

@section[#:style 'unnumbered]{Labs}

@itemlist[
 @item{@link["Lab1.html"]{Lab 1}}
 @item{@link["Lab2.html"]{Lab 2}}]

@section[#:style 'unnumbered]{Assignments}

@itemlist[
 @item{@link["Assignment1.html"]{Assignment 1}}
 @item{@link["Assignment2.html"]{Assignment 2}}]


@include-section{subversion.scrbl}
@include-section{style.scrbl}
@include-section{../pair-programming.scrbl}
@include-section{blog.scrbl}
