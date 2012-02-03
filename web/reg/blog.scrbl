#lang scribble/manual
@(require "../unnumbered.rkt" "../utils.rkt")

@(require (for-label class/0))
@(require (for-label class/universe))

@title*{Blog}

@section*{Past exams}
@tt{Thu Feb  2 20:28:08 EST 2012}

Here are the past exams:
@itemlist[
@item{@link["exam1-sol-sp10.pdf"]{Exam 1, Spring 2010 (Version 1)}}
@item{@link["exam1v2-sol-sp10.pdf"]{Exam 1, Spring 2010 (Version 2)}}
@item{@link["exam1-sol-su10.pdf"]{Exam 1, Summer 2010}}
@item{@link["exam1-sol-fl10.pdf"]{Exam 1, Fall 2010}}
]

@section*{Room for Exam Review: 201MU}
@tt{Thu Feb  2 11:57:24 EST 2012}

The exam review will be in
@link["http://www.northeastern.edu/campusmap/map/qad5.html"]{201 Mugar
Life Sciences Building (MU)} from 6pm-8pm on 2/7.  Be sure to come
prepared with questions for Nikko and Scott.

@section*{Question on homehork}
@tt{Thu Feb  2 08:07:50 EST 2012}

Here is a question we got on the current homework:

@indented{
I am currently working on completing the @tt{directionTo} method in
the second problem of the homework, but was not sure what the method
should return if the two points are the same.  Should I assume that
they're always different points, or should it return @racket["Same Position"], 
or maybe even throw an exception?  I figured I would ask
since the problem didn't really explain that particular scenario and I
just wanted to make sure that I implemented the method properly.}

Good question.

When the customer does not specify something it is Ok for the
programmer to design a reasonable solution and document the choices
he/she has made.

I think returning the String @racket["Same Position"] sounds
reasonable, especially considering that as of this homework we know
nothing about exceptions, and that we really do not want the program
to crash, just because for a short while during our trip we take a
break for lunch and stay in the same place.


@section*{Bookstore code}
@tt{Tue Jan 31 17:21:51 EST 2012}

Here is the worked out @link["BookstoreAbstract.java"]{bookstore code}
from class.

@section*{Shark code from last lecture}
@tt{Mon Jan 23 08:00:46 EST 2012}

Here's the @link["Shark.java"]{shark code} we've been developing in
class.

@section*{Small revisions to Assignment 2, SVN guide, Code style}
@tt{Fri Jan 20 15:41:20 EST 2012}

We've made some small adjustments to
@link["Assignment2.html"]{assignment 2}, so be sure to read the latest
version.  (You should reload the page in your browser if you've
visited it recently.)

Jonathan Schuster has put together a nice
@seclink["Subversion"]{guide} on using Subversion that includes
instructions for Windows, Macs, and Linux machines.  It also tells you
how to organize your code and how to set up Eclipse to work with the
repository.

We've also added a note on @seclink["Code_style"]{proper code style}
that you should use for all the code you write in this class.

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

@section*{3:25 Lab canceled}
@tt{Tue Jan 10 21:50:25 EST 2012}

The 3:25pm-5:05pm lab is no longer going to be offered this semester.

@section*{Welcome to CS2510}
@tt{Sun Jan  8 19:39:16 EST 2012} 

We hope you'll have fun.
