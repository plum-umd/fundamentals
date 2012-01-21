#lang scribble/manual
@(require "unnumbered.rkt")

@(require (for-label (except-in class/0 check-expect)))
@(require (for-label (only-in lang/htdp-intermediate-lambda check-expect)))
@(require (for-label class/universe))


@title*{Blog}

@section*{Class diagram for Invader}
@tt{Sat Jan 21 13:52:03 EST 2012}

Here is the simple Space Invader @link["Invader01.pdf"]{class diagram}
presented in class on Thursday.  Your design doesn't have to follow
this one, but we just wanted to share the design we used.


@section*{Problem with .plt file}
@tt{Fri Jan 20 16:10:42 EST 2012}

When posting the latest assignment, somehow an old .plt file got
copied to the web site which didn't have the @racketmodname[class/0]
modifications discussed below.

That problem has been fixed, so please try to re-install the latest
.plt file.  Sorry for the mixup.

@section*{Revised @racketmodname[class/0]}
@tt{Fri Jan 13 14:13:15 EST 2012}

We've decided to make some different design choices for
@racketmodname[class/0].  These changes are backwards compatible, so any
programs you've already written should still work in the new
@racketmodname[class/0].

We have added the ability to write @racket[check-expect]s within a
class definition so that tests can be closer to code.  It's important
to keep in mind that these tests really exists @emph{outside} of the
class and any of its instances, so you cannot refer to @racket[this]
or access any fields.  Think of it purely as a convenience for writing
tests near method definitions; the are lifted out to the top-level of
a program by our implementation.

We made the mistake of introducing the notion of "visibility" too
early by including @racket[define/public] and @racket[define/private]
in @racketmodname[class/0].  Visibility, which is a mechanism for enforcing
invariants of objects, is something that we don't need to worry about
until later in the semester.  We have therefore made it possible to
define methods using @racket[define], which has the same meaning as
@racket[define/public], and @racket[define/public] and
@racket[define/private], while still available, should be considered
deprecated.

Thus the following new kind of program can be written: 

@#reader scribble/comment-reader
(racketmod
class/0
;; A Posn is a (new posn% Number Number)
(define-class posn%
  (fields x y)
  ;; dist : Posn -> Number
  ;; Euclidean distance from this posn to that posn.
  (check-expect (send (new posn% 3 4) dist (new posn% 0 0)) 5)
  (define (dist that)
    (sqrt (+ (sqr (- (send that x) (field x)))
	     (sqr (- (send that y) (field y)))))))
)

which is equivalent to this one:

@#reader scribble/comment-reader
(racketmod
class/0
;; A Posn is a (new posn% Number Number)
(define-class posn%
  (fields x y)
  ;; dist : Posn -> Number
  ;; Euclidean distance from this posn to that posn.
  (define/public (dist that)
    (sqrt (+ (sqr (- (send that x) (field x)))
	     (sqr (- (send that y) (field y)))))))

(check-expect (send (new posn% 3 4) dist (new posn% 0 0)) 5)
)

You are still free to use @racket[define/public] although we'd prefer
you use @racket[define] moving forward.

The documentation for @racketmodname[class/0] has been updated to reflect
these changes.

To access the new features, you should install the new @tt{.plt} file
as described on the @secref{class} page.

@section*{Assignment 2 posted}
@tt{Thu Jan 12 12:10:33 EST 2012}

@seclink["assign02"]{Assignment 2} has been posted.

@section*{Partnerships updated}
@tt{Wed Jan 11 11:10:16 EST 2012}

The initial partnerships, listed below, have been updated for one parternship
change and for everyone's updated username.

@section*{Programming is like Cooking}
@tt{Wed Jan 11 11:05:34 EST 2012}

Here's a recent NYTimes article
(@link["http://www.ccs.neu.edu/home/dvanhorn/tmp/programming-is-like-cooking.pdf"]{There's
the Wrong Way and Jacques Pépin's Way}) about the chef Jacques Pépin,
author of @emph{La Technique}.  There are many fruitful analogies
between programming and cooking; one of which is that Pépin's
"technique" is very much like our design recipe.  The goal of
Fundamentals I and II is to instill the essential technique of great
programmers, and to borrow Pépin's words, "Once you learn the
technique, then can be a creative programmer; a great programmer is
first a tehnician."  And there's only one way to become a master of
technique: "you have to repeat, repeat, repeat, repeat until it
becomes part of yourself."

@section*{Book PDF link fixed}
@tt{Wed Jan 11 01:03:16 EST 2012}

The link to the PDF for @seclink["book"]{@emph{Designing Programs with
Class}} has been fixed.  Remember: the book is updated on a regular
basis so be sure you're reading the latest version.

@section*{Assignment 1 posted}
@tt{Mon Jan  9 15:40:38 EST 2012} 

@seclink["assign01"]{Assignment 1} has been posted.


@section*[#:tag "partners1"]{Initial partner assignments}

@tt{Sun Jan  8 19:39:16 EST 2012} 

If your username is not listed, please contact the instructors right away.

@verbatim{
pair 001: tlunter, dcalacci, iredpath
pair 002: gwong, kevrcav
pair 003: ajhorst, butlerch
pair 004: stoye, emichel
pair 005: ckohler, gchan93
pair 006: ronbrz, cclark
pair 007: jgoode, cmoss
pair 008: aloud, mlhamlin
pair 009: lestille, rmacnz
pair 010: wjj, kingm
pair 011: chris11, psanshi
pair 012: ksoldau, erenn16
pair 013: 11bowerj, bsoohoo
pair 014: colemanm, rramsey
pair 015: ajacks, taldrich
pair 016: asdjkl, gloden
pair 018: jkantor, pletchst
pair 019: mullinsk, nhahn
pair 020: mechanik, manning
}

There is no longer a @tt{pair017}.

@section*{Welcome to CS2510H}

@tt{Sun Jan  8 19:39:16 EST 2012} 

We hope you'll have fun.
