#lang scribble/manual
@(require scribble/core 
          scribble/examples
          racket/sandbox
          (for-label lang/htdp-beginner) 
          (for-label (except-in 2htdp/image image?))
          ;"helper.rkt" 
	  "../utils.rkt"
          "../defns.rkt")

@title[#:style 'unnumbered #:tag "ex1"]{Exercise 1}


@bold{Due}: Monday, July 8, 11:59:59 PM EST.

@section[#:tag "ex-install"]{Install DrRacket}

If you haven't already, download and install DrRacket, part of the
Racket platform, available for download from
@link["https://download.racket-lang.org/"]{https://download.racket-lang.org/}.

The installation process is pretty easy and should work just like
installing any other software on your computer.  Here's a short video
demonstrating the process.

@panopto-vid["https://umd.hosted.panopto.com/Panopto/Pages/Embed.aspx?id=fca012d7-29a3-4b30-af98-a9ad01537e17"]


@section{Start}

Open DrRacket.  Make sure your language is set to Beginning Student.
At the top of the file, write the following, except use your actual name in
place of @italic{... your name here...}:

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; Exercise 1
;; Name: ...your name here...
}|

Save the file on your computer with the name @tt{ex1.rkt}.

Log in to @link[elms-url]{ELMS} and upload your file as a solution to
Exercise 1.

Continue to do the rest of this exercise.  Save and upload
@tt{ex1.rkt} as you make progress.  Getting in the habit of submitting
early and often will pay off in saving you hours or weeks of lost work
in the future.

@section{Boring Arithmetic}

One of the first things you will have to do when learning a
programming language for the first time is get comfortable translating
from concepts you're comfortable with into the notation (or syntax) of
the langauge you're learning.

Translate the following arithmetic expressions into BSL notation.  For
each translation, write an expression in @tt{ex1.rkt}.  Based on the
arithmetic, calculate what the expression @emph{should} produce when
you run the program.  Run the program and confirm (or refute) your
prediction.  

@margin-note{This arithmetic stuff is boring.  We get it.  More
interesting stuff is coming, we promise, but first you have to master
the syntax of BSL.  It's like learning the grammar of a foreign
language.  It takes time and practice, but eventually it will become
second nature.}

If your BSL expressions don't produce the right answer, it could be
that you've translated incorrectly or you calculated the arithmetic
wrong.  Reflect on which has occurred and revise your program if
needed until your predictions and observed outcomes match up.

Note that you may need to recall the precedence of arithmetic
operations in order to make sense of some of these.

@exercise["Translate expressions to BSL"]{

@itemlist[
  @item{@math{25 · 3}}
  @item{@math{25 + 3}}
  @item{@math{25 / 3}}
  @item{@math{25 · 2 + 4}}
  @item{@math{15 - 4 · 2}}
  @item{@math{2 / 3}}
  @item{@math{6 / 3 · 2}}
  @item{@math{2 · 6 / 3}}
]

}

These examples involve using more operations.  Skim the
@link["https://docs.racket-lang.org/htdp-langs/beginner.html?q=bsl#%28part._htdp-beginner._.Numbers__.Integers__.Rationals__.Reals__.Complex__.Exacts__.Inexacts%29"]{documentation}
on numeric operations to see what BSL operations correspond to these
arithmetic operations.

@exercise["Translate expressions to BSL with more operations"]{

@itemlist[
  @item{@math{√25}}
  @item{@math{√-1}}
  @item{@math{5^2}}
  @item{@math{2^5}}
  @item{@math{|4|}}
  @item{@math{|-4|}}
]

}

Leveling-up from arithmetic to algebra, consider the following
function definitions and translate each into equivalent BSL
definitions.

@exercise["Translate functions to BSL"]{

@itemlist[
  @item{@math{f(x) = 2 · x}}
  @item{@math{g(x) = 2^x+x^3+4}}
  @item{@math{h(x) = 7 / x}}
  @item{@math{q(v) = 1 / 2 · v^2}}
  @item{@math{r(s) = f(s) + 4 · g(3 · s)}}
  @item{@math{m(x) = x + h(2)}}
  @item{@math{z(a,b,c) = 2^a+b^2+c}}
]

}

Now write expressions for each of these @emph{uses} of the functions
you defined.  Make hypotheses about what @emph{should} happen and
compare them to what @emph{does} happen.

@exercise["Using functions in BSL"]{

@itemlist[
  @item{@math{f(3)}}
  @item{@math{f(5)}}
  @item{@math{g(4)}}
  @item{@math{h(-7)}}
  @item{@math{q(8)}}
  @item{@math{r(3)}}
  @item{@math{r(4)}}
  @item{@math{z(5,9,1)}}
]

}

You should now have a sense of how to translate a language you know
(math) into a language you're learning (BSL).  From this, you can
build a fluency in understanding BSL in particular, and computation in
general.

@section{From Syntax to Semantics}

Being able to translate from math notation into BSL notation is all
about understanding the rules for how expressions are formed. In other
words, it's all about @emph{grammar} (a.k.a. @emph{syntax}).
Understanding what expressions @emph{mean} requires an understanding
of BSL @emph{semantics}.

You already have a sense of what BSL programs should mean because you
know it should match the meaning of the original mathematical
expressions.  But how do you determine the meaning of a mathematical
expression like @math{2 · 6 / 3}?  You calculate.  You start by
simplifying @math{2 · 6}, which you know from the rules of
multiplication is @math{12}.  So the meaning of @math{2 · 6 / 3} is
the same as @math{12 / 3}, which by the rules of division, is the same
as @math{4}.

The same process is how BSL compute and there's a tool to help you
explore this process called "the stepper."

Using the program you wrote above, use the @bold{Step} button (rather
than @bold{Run}) to explore the peice by peice calculation that
happens when you run a BSL program.

Step through several examples and try to build a mental model of how
computation in BSL works.  (Formulate hypotheses about what will
happen next, observe, and revise if needed.)

(This part of the exercise is all about building understanding, but
doesn't require you to write or submit anything new.)


@section[#:tag "ex1:submit"]{Submit}

Save your work as @tt{ex1.rkt} and submit the file on
@link[elms-url]{ELMS}. The latest submission before the deadline will
be graded, so submit the entirety of your work every time.  @bold{Do
not email submissions to course staff.}


