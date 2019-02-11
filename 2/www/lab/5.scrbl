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


@section[#:tag "lab6:world"]{Drawing Images with JavaLib}

In DrRacket, we used the library @tt{htdp/image} to draw images. In
Java, we'll use JavaLib's images. We used the library
@tt{htdp/universe} to draw animate our worlds. In Java, we'll use
JavaLib's Functional Worlds.

The documentation for the images and functional world library can be
found
@link["https://course.ccs.neu.edu/cs2510sp17/image-doc.html"]{here}.

These libraries are included in the @link["Lab6.zip"]{project skeleton}.


@section[#:tag "lab6:problem"]{A Falling Ball}

Our goal in this lab is to create a simple world with a falling
ball. The ball should be initialized with a random X and Y velocity
and should fall at a rate of 9.8 pixels per second. The project
skeleton gives you an outline of the tasks you need to complete to get
this program up and running.

