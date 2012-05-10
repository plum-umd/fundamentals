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

@title[#:tag "lab02"]{5/8: Subversion, Testing, and Recursive Methods}

The goal of this lab is to introduce more aspects of our work
environment: the Subversion revision control system and the Tester
library. In the second part of the lab, you will practice writing some
recursive data definitions and their methods.

@lab:section{Administrivia}

@exercise{
  Check your partner assignment on the @secref{Blog} and alert the
  TA if there are any mistakes.
}


@lab:section{Tester setup}


@exercise{
  Set up the tester library in Eclipse.
}

@itemlist[

@item{Download the tester @link["http://www.ccs.neu.edu/javalib/Downloads/v1.4.1/tester1_4_1.jar"]{jar file}.}
@item{If you haven't already, create a Java project in Eclipse.}
@item{Define a class named @tt{Examples}.}
@item{Give this class a method

@verbatim|{
    Boolean testSomeStuff(Tester t) {
      return t.checkExpect(true, true)   // Clearly passes
          && t.checkExpect(true, false); // Clearly fails
    }
}|
}

@item{Right click on the project folder in the package manager: Build
   Path -> Add External Archives -> Navigate to and select the tester
   jar.}

@item{Make a run configuration for ``Java Application'' and give it a
   name.  For "Main class:" input tester.Main In the arguments tab, in
   "Program arguments:" input @tt{Examples}.
}]

Running this will run all your tests and give a result digest.

@exercise{Revisit your code from @seclink["lab01"]{Lab 1} and
  reformulate all of your examples as tests.  Run them and correct any
  mistakes this uncovers.}

For the remainder of the course, when writing tests, you should use
the Tester library.


@lab:section{Getting started with Subversion}

Subversion is a revision control system.  It helps you keep track of
the changes to a document over the course of its lifetime.  It can be
used for any kind of document: papers, notes, spreadsheets, web pages,
etc., but it is often used for the source code of programs.  We're
going to be using Subversion (svn) for the remainder of the course in
order to track changes to code written for labs and homeworks and we
will use it as the mechanism for turing in and receiving feedback on
assignments.

@exercise{Read the course notes on @secref{Subversion}.}

Subversion is available on all the lab machines, but you may also want
to install a Subversion client (there are several to choose from) on
your personal computer.

The course repository, @tt{cs2510spring2012}, has been created with a
directory for each pair and a directory for each student.  You will
only have access to access to your user directory and your pair
directory.  No one but the course staff has access to your user
directory and no one but your partner and the course staff has access
to your pair directory.

@exercise{Check out a copy of your user directory in the
@tt{cs2510spring2012} repository.  Verify that your solution to
assignment 1 is in it.}

You should use this directory to store lab material, notes, or
anything else you'd like that shouldn't be shared with others (beside
the staff).  We will use this directory to communicate (non-pair)
grades back to you.

Remember that no changes are made to the repository until you
successfully commit.  Until then, any changes you make are only stored
on the computer you're working on.  Therefore you should commit early
and often (like voting).

@exercise{Check out a copy of your pair directory in the
@tt{cs2510spring2012} repository.}

You should use this directory to store assignments.  We will use this
directory to communicate pair grades back to you and your partner.

@exercise{Within your user directory, create two directories called
@tt{lab1} and @tt{lab2}.  Add your Java source code to for each lab to
these directories.  Check these directories in to the repository and
commit.}


@lab:section{The Tree of Life}

For the remainder of the lab, you will gain experience designing
recursive classes.  Be sure to test all of your designs using the
Tester library and commit your work to your user directory in the svn
repository.

Life can be viewed as a sequence of situations in which you must make
a decision about what to do.  Let's simplify things and say life is
really series of situations in which you can either do or not do
something.  For example, ``do you go to college?'', ``do you brush
your teeth?'', ``do you step out on to Hungtington without looking
both ways?''.  You choice influences the remaining questions you will
face; if go to college, for example, you will probably face choices
such as ``should I go to lecture to today?'', whereas this situation
doesn't present itself if you chose not to go to college; instead
you'd face a different set of decisions to make.

Eventually, no matter what series of decisions you make, you'll die.
At which point, you're done making decisions.

@exercise{Design a data definition for representing different life scenarios.}

Your data definition must be adequate to represent scenarios such as
the following:

@itemlist[
@item{You die!  (Bummer)}

@item{You're on the Green tracks and a train is coming.  Do you jump
into the street?
@itemlist[
@item{If yes: You die!  (Hit by a car)}
@item{If no: You die!  (Hit by a train)}]}

@item{You're texting while driving.  Do you stop?

@itemlist[
@item{If yes: You arrive at your friend's house.  Do you go in?
@itemlist[
@item{If yes: You're friend has a Pizza and offers you some.  Do you eat some?
@itemlist[
@item{If yes: You die! (Poison)}
@item{If no: You die! (Friend shoots you for being rude)}]}
@item{If no: You die!  (Piano dropped on your head)}]}
@item{If no: You die! (Car crash)}
]
}
]

@exercise{Make 4 examples of life scenarios (different from the above).}

@exercise{Design a method that determines the least number of
decisions you can make before dying.}

@exercise{Design a method that determines the maximum number of
decisions you can make before dying.}

@exercise{Design a method that determines the number of
decisions you can make if you always answer ``yes''.}

@exercise{Design a method that determines the number of
decisions you can make if you always answer ``no''.}

@exercise{(Tricky) Design a method that determines the number of decisions you
can make if you always alternate between answering ``yes'' and ``no''
(starting with ``yes'').}

@exercise{Design a data definition for lists of questions.}

@exercise{Design a method that produces the list of questions you face
if you always answer ``yes''.}

@exercise{(Tricky) Design a method that produces the list of questions
you face if you always alternate between ``yes'' and ``no''.}

Be sure to check the course web page later this afternoon; the second
assignment will be posted.

