#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")
@(require "../utils.rkt")

@lab-title[3]{Classy Snakes}

@section[#:style 'unnumbered #:tag "lab3:intro"]{Introduction}

You'll work in this lab with your
@link["https://piazza.com/class/jcspfhewmdn41y?cid=27"]{newly assigned
partner}.

The two of you will work as a team to solve problems. At any time, one of you
will be the @bold{Head} and the other will be the @bold{Hands}. The @bold{Head}
does the thinking and the @bold{Hands} does the typing. @bold{Hands} type only
what the @bold{Head} tells them to, but you're free to discuss any issues that
pop up. We'll have you switch off during the lab to make sure each of you get
practice problem solving, dealing with syntax, and getting finger exercises on
the keyboard.

You both should install DrRacket, but only one instance should be use during the
lab. At the end you'll submit the lab as a pair via the
@link["https://submit.cs.umd.edu"]{UMD CS Submit Server} so we can keep an eye
on lab attendance.

@section[#:tag "lab3:snake"]{The Snake Game}

Reimplement the @link["snake.rkt"]{Snake Game} in the @tt{class/0}
language.

Please note: you'll need to implement a class-based @tt{[Listof X]}
rather than using the built-in lists, as in the previous two labs. Put
some thought into which data definitions should remain the same and
which should be reorganized, especially the unions.
