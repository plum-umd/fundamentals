#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")

@title[#:style 'unnumbered #:tag "lab12"]{Lab 12: Applying the Design Process (1/2)}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/beginner.html"]{Beginning Student
Language}. Require the HtDP2e
@link["https://docs.racket-lang.org/teachpack/2htdpimage.html"]{image} and
@link["https://docs.racket-lang.org/teachpack/2htdpuniverse.html"]{universe}
libraries at the top of your definitions: @racketblock[(require 2htdp/image)
(require 2htdp/universe)]

Make sure you follow
@link["https://cs.umd.edu/class/fall2017/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.

@section[#:style 'unnumbered #:tag "lab12:assign4"]{Designing with Style}

In @secref{assign4}, you were asked to methodically apply the design
recipe to develop a graphical text editor.  In this lab, the TAs will
lead an interactive session developing a solution to this assignment,
highlighting the steps of the design process and arguing for their
utility.
