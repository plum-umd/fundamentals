#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")
@(require "../utils.rkt")

@lab-title[7]{99 Red Falling Balls}

@section[#:style 'unnumbered #:tag "lab7:intro"]{Intro}

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

@section[#:tag "lab7:init"]{Lab Skeleton}

You must start this lab with @link["Lab7.zip"]{this project
skeleton}. Unzip the file into your IdeaProjects directory and open
it with IntelliJ to get started.

We'll be using the JavaLib image and world library again in this lab;
it's all set up in the project skeleton. See
@link["https://course.ccs.neu.edu/cs2510sp17/image-doc.html"]{the
documentation} for details about available classes/methods.

@section[#:tag "lab7:problem"]{Many Falling Ball}

Our goal in this lab is to create a simple world with an arbitrary
number of falling balls. Each ball should be initialized with a random
X and Y velocity and should fall at a rate of 9.8 pixels per
second. The lab skeleton gives you an outline of the tasks you need to
complete to get this program up and running, so go get started!
