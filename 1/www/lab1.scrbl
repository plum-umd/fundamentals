#lang scribble/manual
@(require scribble/core)
@(define (colorize c . content)
  (elem #:style (style #f (list (color-property c)))
        content))

@title[#:style 'unnumbered #:tag "lab1"]{Lab 1: Getting Started}

@section[#:style 'unnumbered #:tag "lab1:intro"]{Introduction(s)}

You'll work in labs and on problem sets in pairs, and we've randomly assigned
your partner for this first lab (and this week's problem set). Find your partner
and introduce yourself. If your partner is @bold{not} present, let one of your
TAs know.

The two of you will work as a team to solve problems. At any time, one of you
will be the @bold{Head} and the other will be the @bold{Hands}. The @bold{Head}
does the thinking and the @bold{Hands} does the typing. @bold{Hands} type only
what the @bold{Head} tells them to, but you're free to discuss any issues that
pop up. We'll have you switch off during the lab to make sure each of you get
practice problem solving, dealing with syntax, and getting finger exercises on
the keyboard.

You both should install DrRacket, but only one instance should be use during the
lab. At the end you'll submit the lab as a pair via the
@link["https://submit.cs.umd.edu"]{UMD CS Submit Server} so we can keep an eye
on lab attendance.


@section[#:tag "lab1:ide"]{Meet DrRacket}

DrRacket is the program you'll use to design and run your programs.
@link["https://download.racket-lang.org"]{Download}, install, and run DrRacket
to get started.

Help your partner install it on their machine if you're done first. Then, pick
the first @bold{Head} and @bold{Hands} and get continue on only one machine.

Explore DrRacket's interface. First, find out how to set the current
@link["https://docs.racket-lang.org/drracket/choose-language.html"]{Language} to
the Beginning Student teaching language (BSL). We'll tell you which language to
use at the beginning of each lab and problem set.

(Note: if you change the language, you'll have to hit the <Run> button for it to
take effect.)

Next, find the documentation for the BSL. (Hint: check the Help menu.) You
should get comfortable searching and reading the documentation for anything you
need to know about the language and its libraries.

Locate the @italic{definitions window} and the @italic{interactions window}

The @italic{interactions window} lets you quickly make simple calculations. You
can type in some expression and hit <Enter> to run it. Test out a few
expressions, try to do some arithmetic with big numbers and fractions (or
anything else you want to test out).

The @italic{definitions window} is where you'll define and develop programs. You
execute the code in the @italic{definitions window} by hitting the <Run> button.

There's a whole bunch more to explore, but that's all you need for now. Feel
free to try out the Stepper or other features as you program.

If something you type results in @colorize["red"]{red text} being printed in the
@italic{interactions window}, the Dr is telling you something unexpected
happened. Errors happen a lot, and are a @colorize["blue"]{GoodThing™}! The Dr
is giving you the very best help that it can. Read the message! Figure out what
went wrong. Try to fix the problem.


@section[#:tag "lab1:ex"]{Finger exercises}

@larger{@bold{Ex 1}}: Define a function @tt{how-long-at-60-mph} that, when given
a number that represents distance in miles, will return the time (in hours) it
takes to travel that distance when going 60 MPH. Write your function in the
@italic{definitions window}, click Run, then use the @italic{interactions
window} to test your function.

@larger{@bold{Ex 2}}: Define a function @tt{how-far-at-70-mph} that, when given
a number representing time in minutes, will return the distance traveled when
going 70 MPH, rounding up to whole miles.

@bold{@colorize["red"]{Hint}}: If you don't know how to perform some particular
calculation, search the docs! If you can't find it in the docs, ask a TA (and
show them what you searched for in the docs).

@larger{@bold{Ex 3}}: Swap @bold{Head} and @bold{Hands}!

When you program, you encounter three kinds of errors:
@itemlist[
  @item{syntax errors, meaning what you wrote is not a BSL expression;}
  @item{run-time errors, that is, you wrote a BSL expression but when you
        interact with it, DrRacket signals an error (because a function is
        applied to too many or too few arguments, the wrong kinds of
        arguments, and so on); and}
  @item{logical errors, which do not manifest themselves as some red text in
        the interactions area. Instead, you apply a function and it gives you
        the wrong value back as a result.}
]

Define three variants of the function from @bold{Ex 2}:
@tt{how-far-at-70-mph/{syntax,run-time,logical}}, each of which demonstrates a
different kind of error. Comment out the functions once you're done.

@larger{@bold{Ex 4}}: Add the following expression at the top of your definitions window:
@racketblock[(require 2htdp/image)]

The @tt{require} form lets you use external library definitions in your
code. Hit <Run> after adding the @tt{require} to load the image library.

Find a picture of your favorite animal on the internet (talking to you,
@bold{Head}). Copy and paste it into DrRacket's @italic{definitions window} and
give it a name (like Chip!).

Draw a frame with a green border around your picture of Chip. As always, the
@link["https://docs.racket-lang.org/teachpack/2htdpimage.html"]{docs} can help
if you don't know what functions to use to, for example, make a
@link["https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._rectangle%29%29"]{@tt{rectangle}}
or
@link["https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._place-image%29%29"]{@tt{place-image}}s
on one another.

@larger{@bold{Ex 6}}: Define a function @tt{in-a-frame} that given a string
color (like "red", "blue", "green") places Chip on a frame of that
color.
