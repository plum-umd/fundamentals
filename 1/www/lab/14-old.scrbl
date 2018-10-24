#lang scribble/manual
@(require scribble/core (for-label lang/htdp-intermediate) "helper.rkt")

@title[#:style 'unnumbered #:tag "lab14"]{Lab 14: Cleaning Up the Client}

@section[#:style 'unnumbered #:tag "lab14:survey"]{In the First 5 Minutes}

Please complete @link["https://goo.gl/forms/XYHlP632pKd5DZ4D2"]{this quick
survey}. None of the instructional staff for the course will see any of the
individual responses until after final grades have been submitted.

@section[#:style 'unnumbered #:tag "lab14:back"]{Back to the Lab}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/intermediate.html"]{ISL}. Require
the HtDP2e
@link["https://docs.racket-lang.org/teachpack/2htdpimage.html"]{image} and
@link["https://docs.racket-lang.org/teachpack/2htdpuniverse.html"]{universe}
libraries at the top of your definitions: @racketblock[(require 2htdp/image)
(require 2htdp/universe)]

Make sure you follow
@link["https://cs.umd.edu/class/fall2017/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.

Open your current @tt{ChatRoom} implementation from
@link["https://cs.umd.edu/class/fall2017/cmsc131A/Labs.html"]{labs} 6-8,
10-13. Make sure you've completed these labs before you continue with this lab
and save/submit your definitions. We will be extending this program in future
labs.

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab14:ho"]{Looking Back at Lists}

In class you've been learning about higher-order operations on lists. You've
seen examples of @racket[filter]s (removing even numbers from lists of numbers,
removing empty strings from lists of words), @racket[map]s (adding 1 to each
number in a list, drawing each @emph{ChatClient} in the @emph{ChatRoom}), and
@racket[foldr]s (getting the sum or product of numbers in a list, turning a list
of characters into a list of words).

@larger{@bold{Ex 1}}: Looking back over the exercises you've completed in labs
(since @secref{lab9}), list 3 functions that you've designed that can be
classified as @racket[filter]s, 3 as @racket[map]s, and 3 as
@racket[foldr]s. Write your answers in a comment, then discuss with another pair
in your lab to see if your answers agree.

@larger{@bold{Ex 2}}: Rewrite any functions you've found in your @emph{ChatRoom}
implementation that can be expressed as @racket[filter]s or @racket[map]s as
a @racket[filter] or a @racket[map].

@larger{@bold{Ex 3}}: Rewrite any functions you've found in your @emph{ChatRoom}
implementation that cannot be expressed as @racket[filter]s or @racket[map]s,
but can be expressed as a @racket[foldr] as a @racket[foldr]. What is the key
difference between these operations and those that can be expressed as
@racket[filter]s or @racket[map]s?
