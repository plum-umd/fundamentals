#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")
@(require "../utils.rkt")

@lab-title[3.5]{Classy Snakes (cont.)}

@section[#:style 'unnumbered #:tag "lab3-cont:intro"]{Introduction}

You’ll work in this lab in ad-hoc pairs. Find a partner and get started.

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

@section[#:tag "lab3-cont:snake"]{Continue with The Snake Game}

Finish @labref{3} by reimplementing the @link["snake.rkt"]{Snake Game}
in the @tt{class/0} language.
