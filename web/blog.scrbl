#lang scribble/manual
@(require "unnumbered.rkt")
@(require "utils.rkt"
	  racket/runtime-path
	  scribble/eval
          racket/sandbox)

@(require (for-label (except-in class/0 check-expect)))
@(require (for-label (only-in lang/htdp-intermediate-lambda check-expect)))
@(require (for-label class/universe))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (only-in lang/htdp-intermediate-lambda local sqr / + sqrt make-posn posn-x posn-y posn?)))
    (the-eval '(require 2htdp/image))
   ;(the-eval '(require lang/htdp-intermediate-lambda))
    (the-eval '(require class/2))
    #;(call-in-sandbox-context
     the-eval
     (lambda () ((dynamic-require 'htdp/bsl/runtime 'configure)
                 (dynamic-require 'htdp/isl/lang/reader 'options))))
    the-eval))

@title*{Blog}

@section*{Exam 2: 4/1 at 6PM in WVH 108}
@tt{Tue Feb 19 14:37:44 EST 2013}

The second exam will be on Monday, 4/1 at 6PM in WVH 108.  This means that
there will be no lab on 4/1.

If you have a conflict with the portion of this time from 7:40-9:00, please let
the instructors know as soon as possible.


@section*{Review session}
@tt{Wed Feb 13 10:10:35 EST 2013}

The review session will be from 6-9 PM on Wednesday (i.e., today) in WVH 366.



@section*{Past exams} @tt{Thu Feb 7 11:56:19 EST 2013}

Here are the past exams from CS2510H:
@itemlist[
@item{@link["exam1-s11.pdf"]{Spring 2011}}
@item{@link["exam1-s12.pdf"]{Spring 2012}}]

@section*{Assignment 6 out}
@tt{Wed Feb  6 19:31:15 EST 2013}

The next assignment is @seclink["assign06"]{out}.

@section*{First exam: In class, Thursday 2/14}
@tt{Wed Feb  6 17:47:40 EST 2013}

The first exam will be held on Thursday 2/14 @emph{in class}.
Previously we had planned for a three hour out-of-class exam, but due
to the short notice, we've decided to hold a 1.5 hour exam.  We will
host a review session and post past exams to this blog.

@section*{Some answers on quick lists}
@tt{Wed Feb  6 17:42:10 EST 2013}

Q: I'm having trouble understanding what is meant by representing a
list as a ``forest of trees of elements'' -- and my confusion only
gets worse as I read more of the paragraph describing ``Van Horn's
idea''.  Would you mind clarifying the data structure we're being
asked to build?

A: A ``forest'' is just a collection of trees.  So when the assignment
says that ``A quick list is a forest of increasingly large full binary
trees'' that means that your quick list data structure should contain
a bunch of binary trees, each of which is full (we talked about full
binary trees in class on Monday), and which are increasingly large as
you go through the forest.

Q: And going through/deeper into the forest would be the equivalent of
traveling further down the elements of a list, correct?

A: Yes, later elements in the list are further into the forest.

@section*{Assignment 3 questions and updates}

@tt{Mon Jan 21 12:38:31 EST 2013}

@seclink["assign03"]{Assignment 3} incorrectly asked two questions about super
classes, which we haven't discussed in class. These have been removed from the
assignment.

If you want, you may use @racketmodname[class/1] to solve the assignment, but
@emph{only} the features of @racketmodname[class/1] we have discussed in class.

Also, there have been questions about the @racket[half-working] method.  In a
case where aging won't produce a population that is exactly half as large (as
when there are an odd number of adults), your @racket[half-working] method
should stop when there are @emph{no more than half} as many adults as
originally.

There was also a typo in the @racket[Possession] problem, which has been fixed,
and the problem on mobiles has been clarified.


@section*{Assignment 3 out}

@tt{Thu Jan 17 11:30:30 EST 2013} 

@seclink["assign03"]{Assignment 3} is posted.


@section*{Fixing image errors in DrRacket}

@tt{Mon Jan 14 12:21:14 EST 2013}

Sometimes, when you have a file in DrRacket that uses images,
sometimes you'll get the following error

@verbatim{
write: cannot marshal value that is embedded in compiled code
  value: (object:image-snip% ...)
}

To fix this, you should change your DrRacket settings as follows.
First, open the @emph{Choose Language} item from the @emph{Language}
menu.  Then click on @emph{Show Details} Then @bold{uncheck} the
@emph{Populate "compiled" directories} item.

In general, tools such as Git will work better with files that embed
images when you follow the guidelines outlined on the @secref["style"]
page.

@section*{Assignment 2 out, Git notes updated}

@tt{Thu Jan 10 11:29:45 EST 2013}

@seclink["assign02"]{Assignment 2} is posted.  Be forewarned, it is
quite involved so we suggest you start early.

We've also updated the notes on @secref{Git} and flushed out
descriptions of some of the most important Git concepts.

@section*{Name and User name survey}

@tt{Mon Jan  7 16:18:18 EST 2013}

Register your CCIS and GitHub user names with @link["https://docs.google.com/spreadsheet/viewform?formkey=dG1TZk9BYzN0UU54NVhXVzhPR3J1YXc6MQ"]{this survey}.

@section*{Initial Partnerships}

@tt{Mon Jan  7 16:16:06 EST 2013}

Below are the initial partnership assignments.  If you can't find your partner,
or are not on this list, please see a member of the course staff.

@verbatim{
01. Nir Shtuhl               Michael R. Rinaldi
02. Trevyn J. Langsford      Tevin M. Otieno
03. Cody M. Wetherby         Dylan P. Collins
04. Matthew T. Cotton        Maxwell T. Skala
05. Eli L. Abidor            Timothy J. Wright
06. Nicholas L. Jones        Tiffany L. Chao
07. Christopher M. Freeley   Calvin J. Pomerantz
08. Zachary Youngren         Craig J. Ellis
09. Lukas Berger             Julia T. Ebert
10. Samantha T. Sanborn      Lochlainn O. Macdonald
11. Ariel R. Winton          Kevin G. O'Leary
12. Stefan T. Cepko          Sree Vishant Prabhakaran
13. Kyle H. Alpert           James C. Larisch
14. Yang Yang                Christian J. DiMare
15. Kaila M. Corrington      Victor M. Monterroso
16. Joseph S. Maxwell        Victor Lei
17. Zoe R. Winkworth         Priscilla A. Baquerizo
18. Kyle P. Meyer
19. Trithep Devakul          Matthew C. Singer
20. Nicholas D. Acquadro     William Caruso
}

@section*{Welcome to CS2510H}

@tt{Fri Jan  4 16:06:59 EST 2013} 

We hope you'll have fun.
