#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")
@(require "../utils.rkt")

@lab-title[8]{Popping Balloons}

@section[#:style 'unnumbered #:tag "lab8:intro"]{Intro}

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

@section[#:tag "lab8:init"]{Lab Skeleton}

You must start this lab with @link["Lab8.zip"]{this project
skeleton}. Unzip the file into your IdeaProjects directory and open
it with IntelliJ to get started.

We'll be using the JavaLib image and world library again in this lab;
it's all set up in the project skeleton. See
@link["https://course.ccs.neu.edu/cs2510sp17/image-doc.html"]{the
documentation} for details about available classes/methods.

@section[#:tag "lab8:problem"]{Popping Balloons}

Our goal in this lab is to modify the program we wrote in the last lab.

This time,

@itemlist[

  @item{instead of a blank screen, 99 red balloons will start on the
        screen,}

  @item{instead of the balls flying off the screen, you must modify
        the world so that they bounce off the walls,}

  @item{instead of mouse clicks adding balls, mouse clicks should
        remove all the balls that fall within the click.}
  
]

The lab skeleton gives you an outline of the tasks you need to
complete to get this program up and running, so go get started!
