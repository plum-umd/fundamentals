#lang scribble/manual
@(require "unnumbered.rkt")
@(require "utils.rkt" racket/runtime-path)

@(require (for-label (except-in class/0 check-expect)))
@(require (for-label (only-in lang/htdp-intermediate-lambda check-expect)))
@(require (for-label class/universe))

@title*{Blog}

@section*{Tester documentation}
@tt{Mon Mar 26 19:16:05 EDT 2012}

If you'd like to read more about how the tester works and how to use its
advanced features, take a look at the documentation here:
@url["http://www.ccs.neu.edu/javalib/Tester/"]

@section*{Assignment 9}
@tt{Fri Mar 23 11:30:59 EDT 2012}

The @seclink["assign10"]{new homework assignment} is now up.  It is due one
week from today.

@section*{New partner assignments}
@tt{Thu Mar 22 12:00:30 EDT 2012}

Here are the new partner assignments that are in effect now (for
assignment 8).

@verbatim{
pair040: pletchst, erenn16
pair041: aloud, iredpath
pair042: colemanm, taldrich
pair043: mlhamlin, gloden
pair044: tlunter, dcalacci
pair045: cmoss, lestille
pair046: kevrcav, gchan93
pair047: rmacnz, wjj
pair048: cclark, ckohler
pair049: bsoohoo, rramsey
pair050: stoye, gwong
pair051: asdjkl, ronbrz
pair052: ajhorst, jgoode
pair053: ksoldau, mullinsk
pair054: jkantor, nhahn
pair055: ajacks, manning
pair056: 11bowerj, mechanik
pair057: kingm, jgoode
}

We have asked Systems to add these pairs to the Subversion repository,
but it may be a little while (less than a day) before you can commit.

@section*{Exam 2}
@tt{Thu Mar 22 10:30:48 EDT 2012}

The second exam will be on Tuesday, in 130 Hurtig Hall.

Last year's second exam is available @link["old-exam2.pdf"]{here}.

@section*{5 Under 25; Tonight}
@tt{Thu Mar 22 08:31:56 EDT 2012}

Here is an email from Fundies II alum Mike Amirault about tonight's 5
Under 25 event.  Hope you can make it.

@;image[#:scale .1 #:suffixes '(".png")]{CCISFlyer.jpg}

Hello Prof. Van Horn,

My name is Mike Amirault and I am a member of the Student Alumni
Association (SAA) and also am in the class of 2013 here in CCIS. I was
in your Fundies 2 class way back in the spring of 2009 and also have
tutored Fundies 1 for you the following year.  I wanted to take the
opportunity to inform you of an upcoming networking series that the
Student Alumni Association and the Office of Alumni Relations hosts
that will be beneficial for your students.

The program is called 5 Under 25, and it is a panel series that gives
current students a glimpse of where their education and experiences at
Northeastern can take them. The 5 panelists are all recent CCIS
graduates who are under 25 years old. The program gives students the
opportunity to ask questions and hear about co-op experiences, class
schedules, student involvement, job searches and their emerging
careers.

This month, the Student Alumni Association will be hosting 5 Under 25
for the College of Computer & Information Science on Thursday, March
22nd at 6:30pm in the Alumni Center (716 Columbus Ave, 6th floor,
across from Squashbusters). There will be free pizza, drinks and
dessert served.

The panel will include :

Brian Correia – Technology Support Specialist

Dan Gonyea – Software Development Engineer

Lauren Siegert – Product Software Engineer

Phil Kelly - Systems Administrator

Jeff Cumplido - TBA

I would greatly appreciate it if you could let your CCIS students know
of this event so they may have the opportunity to hear alumni success
stories and make key contacts. If you have any questions about the
program, please contact me at amirault.m at husky.neu.edu.

Best,

Mike

What: 5 Under 25, College of Computer & Information Sciences
Where: Alumni Center, 6th floor, 716 Columbus Ave (across from Squash Busters)
When: Thursday, March 22nd at 6:30pm
Who: All students interested in networking with successful young alumni and learning more information
about the CCIS classes, student groups, co-ops, careers and more.

@section*{Java code from class today}
@tt{Thu Mar 15 17:03:22 EDT 2012}

Here is the @link["Book.java"]{Java code} we wrote in class
today.  


@section*{Deadline extended for current homework}
@tt{Mon Mar 12 11:07:42 EDT 2012}

The deadline has been extended to Friday for the current homework.

If you need some guidance with Java and Eclipse, please see the notes
from @link["http://www.ccs.neu.edu/course/cs2510/Lab2.html"]{lab 2} of
the regular course.


@section*{Assignment 8 is up}
@tt{Thu Mar  1 14:31:51 EST 2012}

@seclink["assign09"]{Assignment 8} is now up.

@section*{Corrected Exam Date}
@tt{Thu Mar  1 10:35:30 EST 2012}

The second exam date has been corrected, it will be on @bold{Tuesday,
March 27}. 

@section*{Java code from class today}
@tt{Mon Feb 27 17:20:11 EST 2012}

Here is the @link["First.java"]{first} Java program we wrote in class
today.  Here is the @link["Trees.java"]{rest}.


@section*{Assignment 7 is up}
@tt{Thu Feb 23 20:19:26 EST 2012}


@seclink["assign08"]{Assignment 7} is now up, along with a revised
class system, which you will need.


@section*{Assignment 6 is up}
@tt{Thu Feb 16 20:05:03 EST 2012}

@seclink["assign07"]{Assignment 6} is now up.


@section*{Exam 1 solution}
@tt{Wed Feb 15 00:39:40 EST 2012}

Here is a @link["soln1.pdf"]{solution} to the first exam.


@section*{New class system released}
@tt{Mon Feb 13 19:46:02 EST 2012}

There is a new @tt{.plt} file for the class, which includes the new
language level @racketmodname[class/2] that we will now be using.

@section*{No Homework this week}
@tt{Fri Feb 10 10:34:28 EST 2012}

There is no homework for this coming week.  We encourage you to work
through the problems from the last exam.

@section*{Marathon code}
@tt{Thu Feb  9 17:45:44 EST 2012}

@link["marathon.rkt"]{Here} is a slightly cleaned up version of the
Boston marathon code we wrote in class today.  You should try
implementing the @tt{Gender} interface yourself, but if you want,
@link["gender.rkt"]{here's} the one we used.


@section*{Exam 1 review session}
@tt{Thu Feb  9 17:44:39 EST 2012}

The exam 1 review session will be in @bold{108 WVH} on Sunday 2/12
from 6-8pm.


@section*{Exam 1}
@tt{Wed Feb  8 07:55:50 EST 2012}

Exam 1 will be held @bold{Monday, 2/13 from 6-9pm in
@link["http://www.northeastern.edu/campusmap/map/qad5.html"]{103
Churchill Hall (CH)}}.  Yes, it conflicts with lab; don't go to lab,
go to 103 Churchill Hall and take the exam.

The exam is open book, open notes; closed computational devices except
for the one in between your ears.

There has only been one instance of this course before, so we only
have one exam to give you for preparation: @link["exam1.pdf"]{Exam 1,
Spring 2011}.  (You may recognize problem 1!)

There are a couple minor differences with the language used on the
exam compared with the language we have been using: 

@itemize[
@item{It uses @racket[define/public] instead of @racket[define].  They
mean the same thing.}

@item{It mentions @racket[define-interface], a feature we abandoned;
don't worry about it.}

@item{It leaves off @racket[new] when constructing objects; just
imagine it didn't.}
]

There will be a @bold{review session} led by Spencer and Ryan who were
nice enough to volunteer to run the review.  The review session will
be @bold{Sunday, 2/12 from 6-8pm}.  We are working on reserving a room
now and will post the location as soon as we have one.


@section*{Homework Due Date}
@tt{Tue Feb  7 16:32:03 EST 2012}

There was a mistake in the date listed for Assignment 5; it is due on
Wednesday at midnight, as usual.

@section*{Bootstrap} 
@tt{Mon Feb  6 12:38:18 EST 2012}

If you're interested in volunteering for
@link["http://bootstrapworld.org"]{Bootstrap}, which is a program for
teaching middle-schoolers programming using video games in DrRacket,
then you should email @link["vicki@bootstrapworld.org"]{Vicki} to volunteer.  

@section*{New partner assignments}

Here are the new partner assignments that are in effect now (for
assignment 5).

@verbatim{
pair021 ronbrz, wjj
pair022 nhahn, pletchst
pair023 jgoode, ajacks
pair024 rmacnz, mlhamlin
pair025 colemanm, cclark
pair026 mullinsk, lestille
pair027 cmoss, ksoldau
pair028 erenn16, mechanik
pair029 taldritch, ckohler
pair030 gloden, aloud
pair031 rramsey, psanshi
pair032 dcalacci, jkantor
pair033 11bowerj, gchan93
pair034 asdjkl, kingm
pair035 iredpath, bsoohoo
pair036 ajhorst, chris11
pair037 manning, emichel
pair038 tlunter, gwong
pair039 stoye, kevrcav
}

We have asked Systems to add these pairs to the Subversion repository,
but it may be a little while (less than a day) before you can commit.

@section*{Assignment 5 is out}
@tt{Fri Feb  3 00:45:34 EST 2012}

@seclink["assign05"]{Assignment 5} is now available.

@section*{New .plt file and @racketmodname[class/1]}
@tt{Thu Feb  2 12:28:20 EST 2012}

There is a new @tt{.plt} file for the class, which includes the new
language level @racketmodname[class/1] that we will now be using.

@section*{Zombie code}
@tt{Thu Feb  2 12:13:16 EST 2012}

There is now a solution to the @seclink["Zombie_"]{Zombie exercise} in
the book.

We will post the simple universe server and client later.

@section*{Style guide additions}
@tt{Sat Jan 28 14:19:49 EST 2012}

We have added a couple more style guides to @secref{style} having to
do with where test cases should be placed and how to write programs
that need to embed images.  You should follow these additional
guidelines starting with assignment 4.


@section*{Partner requests due by 2/1 at midnight}
@tt{Sat Jan 28 11:37:40 EST 2012}

Partner requests are now being accepted for assignment 5.  In order to
request a partner you must send an email to Prof. Van Horn listing the
partners, their CCIS usernames, and the subject ``Partner Request'';
for example:

@indented[@verbatim|{
To: dvanhorn@ccs....
Subject: Partner Request

George Dickel, gdickel
John Jameson, jjameson
}|]

If we don't receive your request before midnight on 2/1, we will
assign you randomly to a partner.  You may request to continue working
with your current partner.

@section*{Interface chapter written}
@tt{Mon Jan 23 19:10:10 EST 2012}

An @secref{Interfaces} chapter has been added to the book.

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
