#lang scribble/manual

@(require "../lab.rkt"
          "../unnumbered.rkt"
          "../utils.rkt"
          scribble/eval)

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require lang/htdp-intermediate))
    the-eval))


@(define exercise (exercise-counter))

@title[#:tag "lab01"]{5/8: A Little Eclipse, A Little Java}

The goal of this lab is to introduce some aspects of our work
environment: the Eclipse IDE and the basics of running a program in
Java-like languages. In the second part of the lab, (the one that
really teaches you something) will focus on data definitions and
examples in a simple subset of Java language.


@lab:section{CCIS Login}


@exercise{
  Log in to a lab computer using your CCIS credentials.
}

If you requested a CCIS username and password yesterday, it
@emph{should} be active today.  Please send email to @tt{dvanhorn} if
you're still having issues -- we need to resolve all of these ASAP
since your homework requires a working CCIS account.

@exercise{
  Verify your CCIS username in the following list:

@verbatim|{
 acalo          deema           jcaron          kosof           sripley7
 ajacks         dsimond         jinwow          merylas         yusuf.m
 ascherj        goodwin         joejeno         michng
 bjliuy         gudosha         jwall           nnamdi93
 bmccabe4       guom            kevin08         psanshi
 cungtran       jbrooks2        kmiao           schwarta
}|


}

These are the usernames submitted in assignment 1.  Make sure your
username appears @bold{exactly} as it does on the CCIS system.  If
there's any corrections that need to be made, please inform the TA.

@lab:section{Partnerships}

@exercise{
  Select a homework partner and register your selection with the TA.
}

You should select a partner that you feel comfortable working with for
at least the first few weeks of this course.  (You will have the
opportunity to request a new partner in a couple weeks.) You and your
partner will be responsible for developing solutions to assignments.
Both of you must be able to explain, defend, and update your designs
to a jury of your peers.  Be sure to read about the history and
motivation for @seclink["pair"]{Pair programming} on the course web
page.

@exercise{
  Exchange contact information with your partner and discuss
  arrangements for working together.
}

You should complete the rest of this lab (and future labs) working as
a pair.  Your lab partner doesn't necessarily have to be your homework
partner, but it's probably a good idea for the first couple of labs so
you can get comfortable working as a pair.




@lab:section{Eclipse IDE}

In this course, we will use the
@link["http://www.eclipse.org/"]{Eclipse} development environment to
write Java programs.  Eclipse is freely available and we recommend you
install it on your personal computer.  It is also available on all
CCIS lab machines.

If you feel more comfortable, you may choose to use another IDE
(e.g. NetBeans) or work directly from the command line, but you and
your partner must both be comfortable with the chosen programming
environment, and the staff may not be able to assist you with issues
encountered in other environments.

Eclipse includes an editor and allows you to organize your work into
many files that together make up a project. It has an "incremental"
compiler that so you can edit and run your programs while getting
relatively fast error feedback. Your Eclipse workspace can contain
many projects, so you should be able to keep all your work in one
workspace, with one project for each assignment or lab.

@exercise{
   Start Eclipse and create a workspace for the work in this class.
   Create a new project for today's lab.
}

Be sure to save all of your work to a location that you can retrieve
it from later.  Labs will often build upon each other and you may be
asked to submit parts or all of a lab as part of an assignment.  In
the future, you may use @tt{svn} to save and keep track of the changes
to your work.  We will look more closely at @tt{svn} in the next lab.


@lab:section{A Few Data Definitions}

Now that you have Eclipse up and running and a project started for
today's lab, you can try your hand at developing a few data
definitions in the Java notation.

@exercise{
Develop a class-based representation of
complex numbers.
}

Complex numbers are are used in several fields, including:
engineering, electromagnetism, quantum physics, applied mathematics,
and chaos theory.  A @emph{complex number} is a number consisting of a
real and imaginary part. It can be written in the mathematical
notation @emph{a+bi}, where @emph{a} and @emph{b} are real numbers,
and @emph{i} is the standard imaginary unit with the property
@emph{i@superscript{2} = −1}.

@exercise{
Develop a class-based representation of shapes, including points,
rectangles, and circles.  A shape should include a pinhole, which
represents the center point of the shape.}

Once you have defined class definitions for complex numbers and
shapes, it would be nice to create some examples.  To do, define an
@tt{Examples} class that you can use to hold some example data.
Here's a sketch that creates some examples of fictional objects:

@verbatim|{
class Examples {
  Examples(){}

  Blah b1 = new Blah(3, 4, 5);
  Blah b2 = new Blah(8, 9, 10);

  Foo f1 = new Baz(b1, 87);
  Foo f2 = new Bix();
}
}|

@exercise{
  Create an @tt{Examples} class and write several examples of complex numbers
  and shapes.
}

Be sure to take note of Eclipse's reaction when you make mistakes as
you type.  Try making a few intentional mistakes (like passing a
complex number constructor a string) to get a feel for what goes
wrong.

Now let's work on some recursive union data definitions.

@exercise{
  Develop a class-based representation of lists of integers.  Add
  examples of several lists of integers to your @tt{Examples} class.
}

@exercise{
  Develop a class-based representation of lists of complex numbers.  Add
  examples of several lists of complex numbers to your @tt{Examples} class.
}

It's tempting to want to abstract the above two data definitions.
Identify what's different between the two.  We will discuss how to
abstract such definitions in future lectures.

@lab:section{A Few Methods}

You've now defined some new classes of data and made of few instances
thereof.  But what good is information without computation?  Let's
develop a few methods, starting with some for complex numbers.

Complex numbers are so useful, it turns out they are included in the
set of numeric values that Racket supports, but unfortunately Java
lags behind and doesn't support them.  The Racket notation for writing
down complex numbers is @racket[5+3i], where this number has a real
part of @racketresult[5] and an imaginery part of @racketresult[3];
@racket[4-2i] has a real part of @racketresult[4] and imaginary part
of @racketresult[-2].  (Notice that complex numbers @emph{generalize}
the real numbers since any real number can be expressed as a complex
number with an imaginery part of @racketresult[0].)  Arithmetic
operations on complex numbers work as they should, so for example, you
can add, subtract, multiply, and divide complex numbers.  (One thing
you can't do is @emph{order} the complex numbers, so @racket[<] and
friends work only on real numbers.)

So if you'd like to experiment with a complex number calculator, just
fire up DrRacket and use the interactions panel.

Here are a few examples written in ISL:
@#reader scribble/comment-reader
(interaction
  #:eval the-eval
  ;; Verify the imaginary unit property.
  (sqr (sqrt -1))
  (sqr 0+1i)
  ;; Arithmetic on complex numbers.
  (+ 2+3i 4+5i)
  (- 2+3i 4+5i)
  (* 2+3i 4+5i)
  (/ 2+3i 4+5i)
  ;; Complex numbers can't be ordered.
  (< 1+2i 2+3i)
  ;; Real numbers are complex numbers with an imaginary part of 0,
  ;; so you can perform arithmetic with them as well.
  (+ 2+3i 2)
  (- 2+3i 2)
  (* 2+3i 2)
  (/ 2+3i 2)
  (magnitude 3+4i)
)

@exercise{
Develop class-based analogs of the above functions on complex numbers.
}

We haven't discussed testing Java programs, but we will in the next
lecture.  For the time being, just write examples and the expect
result in the comments of your code.

@exercise{
(Challenge) Develop a @tt{sum} method for lists of complex numbers.
}

Note that this requires writing a recursive method (since we wrote a
recursive data definition!), which we did not cover in lecture.  Try
to figure out how to solve the problem by analogy with how you would
solve the problem in ISL.

@lab:section{Wrap up}

You've now developed a few data definitions and methods in Java.
Congratulations.  Be sure to practice more on your own so that you can
fluently solve these kinds of problems.  Come prepared with questions
to the next lecture.

If you would like to have your lab solutions (anonymously) critiqued
by the class, send your code to @tt{dvanhorn}.  (This is completely
optional, but highly encourage; critique is how you become a better
designer.)

