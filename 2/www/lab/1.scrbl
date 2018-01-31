#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")
@(require "../utils.rkt")

@lab-title[1]{Simple Data Definitions with Class}

@section[#:style 'unnumbered #:tag "lab1:intro"]{Introduction(s)}

You'll work in labs and on problem sets in pairs, and we've randomly
assigned your partner for this first lab. Find your partner and
introduce yourself. If your partner is @bold{not} present, let one of
your TAs know.

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


@section[#:tag "lab1:class"]{Meet the Class language}

Make sure you still have DrRacket installed.  If not,
@link["https://download.racket-lang.org"]{download}, install, and run
DrRacket to get started.

Next you will need to install the Class programming language.  To do
this, open DrRacket.  Choose the @tt{File > Install Package...} menu.
In the text area for the "Package Source:" copy and paste the
following URL:

@centered{
@tt{https://github.com/dvanhorn/dpc.git#racket-v6.10}}

Press "Install".  You should see the output of the installation
process.  When it's done, press "Close".

Next you will need to select the @tt{class/0} language.  To do this,
select the drop-down menu in the lower-left-hand corner of DrRacket.
Select "Determine language from source".  You should see @tt{#lang
racket} in the definitions window.  Replace @tt{racket} with
@tt{class/0}.  Press "Run".  If you see @tt{class/0} as the langauge
in the interactions window, then you have successfully installed the
Class programming language.

You are now ready to start exploring the @tt{class/0} language.

You can start by looking at the Class language documentation by
selecting the "Help > Racket Documentation" menu and searching for
"class/0".  This will describe the basic syntax and semantics of
@tt{class/0} programs.

You can also start by writing simple @tt{class/0} programs like those
we've written in class.  Take a moment to review the lecture notes
from class and try running a few examples.

@section[#:tag "lab1:ex"]{Finger exercises}

@larger{@bold{Ex 1}}: ...