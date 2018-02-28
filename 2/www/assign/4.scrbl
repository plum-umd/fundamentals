#lang scribble/manual
@(require scribble/core)
@(require "../utils.rkt")
@(define (colorize c . content)
  (elem #:style (style #f (list (color-property c)))
        content))

@assn-title[4]{A Game in Java}

This is assignment is to be completed and submitted with your
@link["https://piazza.com/class/jcspfhewmdn41y?cid=27"]{partner}.  You
may not work with anyone other than your assigned partner.

@bold{Due}: Tuesday, March 13, 11:59:59 PM EST.

@(define @Piazza @link["http://piazza.com/umd/spring2018/cmsc132a"]{Piazza})

@section[#:style 'unnumbered #:tag "assign4:download"]{Download the assignment project}

For this assignment, download the following zip file:
@link["Assign4.zip"]{Assign4.zip}.  Save the file somewhere on your
computer and unzip it.  This will create a directory called
@tt{Assign4} with an IntelliJ project inside.  It contains all the
libraries you will need and some code to get you started.

Open IntelliJ and select "Open".  Navigate to the @tt{Assign4}
directory and select it.

This should open up the project and place you inside the
@tt{Assign4.java} file.  The first thing you should do is edit the
authors line (which should be selected when you open the project) to
be your and your partner's directory ID.

The project has a "Test" configuration in the top right corner.  Press
the "Run" (green triangle) button to run the test suite.  You should
see 4 tests pass, confirming everything is set up properly.

@section{Java Invaders}

For this assignment, you must re-write a significant @racket[big-bang]
game from last semester to use the Java language. 

The game you will re-design is the simplified Space Invaders game as
written by Austin and David.  The code is
@link["http://www.cs.umd.edu/class/fall2017/cmsc131A/invader-shots-dvanhorn-abourg.rkt"]{here}.
The code should be all you need, but there is a
@link["http://www.cs.umd.edu/class/fall2017/cmsc131A/Notes.html#%28part._.Pair_programming_.Space_.Invaders_with_shots%29"]{video}
of the code being written should you find it useful.

You will need to use the @tt{javalib.funworld} and
@tt{javalib.worldimages} libraries, which you have seen in lab and are
included in the IntelliJ project.  The project has some code to get
you started, which when run will create a new screen and place an
image on it.  For documentation about these libraries, see this
@link["https://course.ccs.neu.edu/cs2510h/image-doc.html"]{page}.

The project comes with two configurations: @tt{BigBang} will run the
@tt{main} method of the @tt{Assign4} class, and thus launch the game;
the @tt{Test} configuration will run the tester library on the
@tt{Tests} class, and thus run the test suite for the program.

@section[#:style 'unnumbered #:tag "assign4:submit"]{Submission}

Use @tt{submit.cs.umd.edu} to submit your solution to the problems.
You should create a zip file called @tt{Assign4.zip} that contains the
IntelliJ project containing your solutions.
