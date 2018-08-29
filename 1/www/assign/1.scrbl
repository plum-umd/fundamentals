#lang scribble/manual
@(require scribble/core)
@(define (colorize c . content)
  (elem #:style (style #f (list (color-property c)))
        content))

@title[#:style 'unnumbered #:tag "assign1"]{Assignment 1: Draw an image in DrRacket}


@bold{Due}: Wednesday, September 5, 11:59:59 PM EST.

@(define @Piazza @link["http://piazza.com/umd/fall2018/cmsc131a"]{Piazza})

@section[#:style 'unnumbered #:tag "assign1:piazza"]{Sign up for Piazza}

Our primary means of communication and answering questions will be the
course @Piazza message board. Sign up.

@section[#:style 'unnumbered #:tag "assign1:drracket"]{Download and install DrRacket}

DrRacket is the program you’ll use to design and run your
programs. You can download it
@link["http://racket-lang.org"]{here}. It's available for any platform
you might want. If you have trouble installing it, post on @|Piazza|.

@section[#:style 'unnumbered #:tag "assign1:read"]{Read the website}

Read all the pages on this website and familiarize yourself with the
course policies.

@section{Create a program that produces an image}

The following should be completed in cooperation with your assigned
partner from lab 1.  (Partner assignments are listed on @|Piazza|.)

@itemize[
@item{Open up DrRacket, and choose the "Beginning Student
Language".}

@item{Start using the @racketmodname[2htdp/image] library, as
described in the
@link["https://htdp.org/2018-01-06/Book/part_prologue.html"]{Prologue}
of the book. Read all the way to where images are discussed!}

@item{Using this library, create a program that when you hit the "Run"
button, it produces a picture.  The picture should represent two of
your interests (one for each partner) and include your prefered first
name.  The composition of the image is otherwise unconstrained and you
may be as creative or unimaginative as you'd like, but we may showcase
the more interesting images in class.  You may use any of the
@racketmodname[2htdp/image] operations to construct this image and you
may use any image you'd like from the internet or any other source
(keep it clean!).  You should use the head/hands model of pair
programming you learned in lab and remember to occasionally switch
modes while you work.}

@item{Save and submit the file using the
@link["https://submit.cs.umd.edu/"]{submit server} for @tt{ps1}.  Only
one partner needs to submit on behalf of the pair.  You may submit as
many times as you'd like (and we recommend submitting early and
often).  The latest submission before the deadline will be graded.
@bold{Do not email submissions to course staff.}}]
