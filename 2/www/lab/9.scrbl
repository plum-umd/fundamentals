#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")
@(require "../utils.rkt")

@lab-title[9]{Bouncy Ball™}

@section[#:style 'unnumbered #:tag "lab9:intro"]{Intro}

You'll work in this lab with your
@link["https://piazza.com/class/jcspfhewmdn41y?cid=27"]{assigned
partner}. Help each other get up and running with IntelliJ and Java
(@labref{4}).

The two of you will work as a team to solve problems. At any time, one
of you will be the @bold{Head} and the other will be the
@bold{Hands}. The @bold{Head} does the thinking and the @bold{Hands}
does the typing. @bold{Hands} type only what the @bold{Head} tells
them to, but you're free to discuss any issues that pop up. You should
switch off during the lab to make sure each of you get practice
problem solving, dealing with syntax, and getting finger exercises on
the keyboard.


@section[#:tag "lab9:init"]{Lab Skeleton}

You must start this lab with @link["Lab9.zip"]{this project
skeleton}. Unzip the file into your IdeaProjects directory and open
it with IntelliJ to get started.

We'll be using the JavaLib image and world library again in this lab;
it's all set up in the project skeleton. See
@link["https://course.ccs.neu.edu/cs2510sp17/image-doc.html"]{the
documentation} for details about available classes/methods.


@section[#:tag "lab9:problem"]{Bouncy Ball™}

The TAs have taken your code from the past few labs and made a cheap
clone of the @link["https://flappybird.io/"]{Bouncy Bird} game that
we'll call Bouncy Ball™. We want you to finish the game for us and
clean up the code a bit.

Don't worry if you haven't finished the past few labs, you should
start with the provided @link["Lab9.zip"]{baseline program}.

You can play the game right now, but it's pretty boring. Spacebar
bounces the ball up a bit; keep the green bouncy ball above the floor
to continue playing. But there seem to be bugs in the new
@emph{ListOfBall} code we added.


@section[#:tag "lab9:ex1+2"]{ListOfBall Extensions}

We needed to extend your @emph{ListOfBall} interface to fit our
needs. And by "we" we mean you.

@bold{Exercise 1}: Complete the implementations of @tt{append} inside
the @tt{EmptyLoB} and @tt{ConsLoB} classes.

@bold{Exercise 2}: Complete the implementations of @tt{exists} inside
the @tt{EmptyLoB} and @tt{ConsLoB} classes.


@section[#:tag "lab9:ex3+4"]{Repetative Redundancies}

The TAs did a poor job combining your implementations to create the
methods @tt{bounce}, @tt{jump}, and @tt{tick} methods inside the
@tt{Ball} class. Each of those methods create a new ball with a
modified vertical velocity and update the new ball's position.

@bold{Exercise 3}: Implement the method @tt{modifyVY} as an
abstraction of the @tt{bounce}, @tt{jump}, and @tt{tick} methods. It
consumes a unary function (@tt{Function<Integer, Integer>}) that
modifies the y-axis velocity of the ball. It should return a new ball
with its positions adjusted by the current x-axis velocity and the
@emph{new} y-axis velocity.

@bold{Exercise 4}: Simplify the implementations of the @tt{bounce},
@tt{jump}, and @tt{tick} methods using your new @tt{modifyVY}.


@section[#:tag "lab9:go-crazy"]{Go Crazy!}

If you're done, make any changes you want to the game. Make the
obstacle balls change color every tick to stupify the player. Make the
bouncy ball render as a bird. Do something else interesting and show
it off to your TA!
