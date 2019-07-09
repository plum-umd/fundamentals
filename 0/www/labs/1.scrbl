#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt" "../utils.rkt")

@title[#:style 'unnumbered #:tag "lab1"]{Lab 1: Getting Started}

@(define ex (make-exerciser "Lab problem"))


@section{Introduction(s)}

You'll work in labs in pairs.  Find someone to work with for this
first lab and introduce yourself. 

Make sure at least one of you have a laptop to work on for this first lab.

The two of you will work as a team to solve problems. At any time, one of you
will be the @bold{Head} and the other will be the @bold{Hands}. The @bold{Head}
does the thinking and the @bold{Hands} does the typing. @bold{Hands} type only
what the @bold{Head} tells them to, but you're free to discuss any issues that
pop up. We'll have you switch off during the lab to make sure each of you get
practice problem solving, dealing with syntax, and getting finger exercises on
the keyboard.

You both should install DrRacket, but only one instance should be used
during the lab.

@section{Install DrRacket}

Download and install DrRacket, part of the Racket platform, available
for download from
@link["https://download.racket-lang.org/"]{https://download.racket-lang.org/}.

@panopto-vid["https://umd.hosted.panopto.com/Panopto/Pages/Embed.aspx?id=fca012d7-29a3-4b30-af98-a9ad01537e17"]

@section{Meet DrRacket}

You should already have DrRacket installed on your computer, but now
is the chance to get help (or catch up) if you don't yet.  DrRacket is
the program you'll use to design and run your programs.
@link["https://download.racket-lang.org"]{Download}, install, and run
DrRacket to get started.

Help your partner install it on their machine if you're done first. Then, pick
the first @bold{Head} and @bold{Hands} and get continue on only one machine.

Explore DrRacket's interface. First, find out how to set the current
@link["https://docs.racket-lang.org/drracket/choose-language.html"]{Language} to
the @link["https://docs.racket-lang.org/htdp-langs/beginner.html"]{Beginning
Student Language} (BSL). We'll tell you which language to use at the beginning
of each lab, exercise, or assignment.

(Note: if you change the language, you'll have to hit the <Run> button for it to
take effect.)

Next, look through the
@link["https://docs.racket-lang.org/htdp-langs/beginner.html"]{documentation for
the BSL}. (Hint: also check the Help menu.) You should get comfortable searching
and reading the documentation for anything you need to know about the language
and its libraries.

Locate the @italic{definitions window} and the @italic{interactions window}.

The @italic{interactions window} lets you quickly make simple calculations. You
can type in some expression and hit <Enter> to run it. Test out a few
expressions, try to do some arithmetic with big numbers and fractions (or
anything else you want to test out).

The @italic{definitions window} is where you'll define and develop programs. You
execute the code in the @italic{definitions window} by hitting the <Run> button.

There's a whole bunch more to explore, but that's all you need for now. Feel
free to try out the Stepper or other features as you program.


@section{Finger exercises}

Now that you've had a chance to install and look around in DrRacket,
here are a few exercises to help you get a first glance at programming
in BSL.

Use the interactions window to try some simple calculations.  For each
of these BSL expressions, make a prediction about what you think will
happen when its evaluated.  Discuss it with your partner.  Then type
it in the interactions window and press enter.  If something different
than expected occurs, discuss possible explainations with your
partner.  Try some variations of these expressions if it's helpful.

@ex["A fancy calculator"]{

In the interactions window, type each of the following and predict
what happens when evaluated:

@itemlist[
  @item{@racket[25]}
  @item{@racket[#true]}
  @item{@racket["Fred"]}
  @item{@racket[(+ 4 8)]}
  @item{@racket[(* 4 8)]}
  @item{@racket[(+ (* 2 2) 12)]}
  @item{@racket[(+ (sqr 2) 12)]}
  @item{@racket[(string-append "Dear" "Fred")]}
  @item{@racket[(string-append "Dear" (string-append " " "Fred"))]}
  @item{@racket[(+ 3 4 6)]}
  @item{@racket[(string-append "Dear" " " "Fred")]}
  @item{@racket[(/ 9 (- 10 (+ 9 1)))]}
  @item{@racket[(string-length "Dear Fred")]}
  @item{@racket[(string-length (string-append "Dear" " " "Fred"))]}
  @item{@racket[(string-length 352)]}
  @item{@racket[(string-length "352")]}
  @item{@racket[(widget 42)]}
  @item{@racket[(8 + 7)]}
  @item{@racket[(+ (8) 7)]}
  @item{@racket[(/ (+ 8 4) 4)]}
  @item{@racket[(/ (+ 8 4) (* 12 3))]}
  @item{@racket[(not #false)]}
  @item{@racket[(and (not #false) (or #false #true))]}
]

}

The interactions window gives you an ephemeral interaction with
computation.  Once you press Run, all your work is wiped out and you
get a fresh interaction prompt. 

To write programs that persist, type expressions in to the Definitions
window.

@ex["A fancy calculator that persists"]{

Copy and paste all of the examples above into the Definitions window.
Press Run.  What happens?

}

You'll notice that BSL prints out a read message in the interactions
window saying there is a problem with this program.

Technically speaking, it's not a program, it's just junk.

To check whether some text is a grammatically well-formed program,
press the Check Syntax button.  This will highlight the offending
peice of text and give you a message (the same one we saw when
running).

Read the message and try to understand what it's telling you.  Once
you understand, comment out that expression by putting a semicolon ";"
in front of it.  This tells BSL to treat the rest of the line after
the ";" as prose intended only for the human reading and writing this
code.

Try Check Syntax again.  Rinse and repeat until Check Syntax succeeds.
When it does, you have a well-formed BSL program.  Hit Run.  What
happens?

@ex["A fancy calculator that persists (w/o errors)"]{

Comment out the expressions that are ill-formed.  Press Run.  What
happens?
 
}

Now you should see some values printed in the Interactions window,
followed by another red message.  Some of the expressions in the
Definitions window will be colored in Halloween colors (orange and
black).  What do you think that means?

You've now seen @bold{two} kinds of errors.  Syntax errors are about
grammar.  They include things like putting parentheses in the wrong
place or putting operators somewhere other than the front of an
expression or using names that aren't defined.  The other kind of
error here is a @bold{run-time error}.  It happens when you run a
well-formed program that does something non-sensical, like divide by
zero or uses a primitive operation on a kind of data that it doesn't
understand.

@ex["A fancy calculator that persists (w/o errors)"]{

Comment out the expressions that produce run-time errors. Press Run.
What happens?
 
}

Now you've got a fancy calculator that does extended "arithmetic"; it
works not just for numbers, but for strings, booleans, and other kinds
of values as we'll see.

Press the Save button to save your Definitions window as a file on
your computer.  If you want to revist this program, you can just open
it in DrRacket.

@bold{Switch Head and Hands.}  Whoever was typing should now be
guiding the thinking and conversation.  Whoever was guiding the
thinking and conversation should now be typing.

In grade school, you were often asked to do the kinds of calculations
we have just asked DrRacket to do.  We are using a different notation
(BSL) and some of our operations are different (e.g. you probably
didn't see @racket[string-append] in 3rd grade).  But essentially it's
the same process.

Another thing you might've been asked to do was to go beyond just
producing the "right answer," but to justify your calculation by
showing your work at each step.

You can ask DrRacket to show you exactly that information with the
Step button.  This will open up a new window, The Stepper, which let's
you explore step-by-step each part of the computation.

@ex["Show your work"]{ 

Press the Step button.  Step through the computation.  Try to build a
mental model of what's happening.  After a few steps, start making
predictions about what will happen the next time you press Step.  Work
your way through all of the steps.

}

Now that you can write expressions, let's move on to functions.

@ex["From Arithmetic to Algebra"]{ 

@itemlist[

@item{Define a function @racket[double-plus1] that take a number and
produces twice that number, plus one.  So @racket[(double-plus1 5)]
should produce 11.}

@item{Define a function @racket[greeting] that takes two arguments: a
first and last name and produces a greeting for use in the opening of
a letter or email.  For example, 
@racket[(greeting "David" "Van Horn")] 
should produce @racket["Dear David Van Horn"].}

]

}

You've now got the basics covered.  If you've made it this far and
have time to spare, engage in some thoughtful experimentation.


