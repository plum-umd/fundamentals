#lang scribble/manual
@(require racket/sandbox
          scribble/example)


@title[#:style 'unnumbered]{Notes}

@local-table-of-contents[]

@(define (vidlink url)
   @elem{Audio and video capture from today's class is available @link[url]{here}.})

@section{August 28, 2017}

@vidlink["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=35260fd2-a523-4c5a-91ad-247fdf821b1c"]

@section{August 30, 2017}

@vidlink["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=7ceb62cc-3be8-4ed4-b56d-4b44400c6d56"]

Code from today's lecture:

@itemlist[
@item{@link["distance.rkt"]{distance.rkt}}
@item{@link["eclipse.rkt"]{eclipse.rkt}}
]

@section{September 1, 2017}

@vidlink["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=43327ae6-bd37-40ff-90b5-a2b148c6757e"]

Code from today's lecture:

@itemlist[
@item{@link["fancy-eclipse.rkt"]{fancy-eclipse.rkt}}
]

@section{September 6, 2017}

@include-section["rocket.scrbl"]

@section{September 8, 2017}

@vidlink["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=53e3fff7-1313-4bfa-b497-84657608ae2c"]

@section{September 11, 2017}

@vidlink["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=06890a7d-8455-415f-b292-9d6cfbb52fbd"]

Code from today's lecture:

@itemlist[
@item{@link["days.rkt"]{days.rkt}}
@item{@link["traffic.rkt"]{traffic.rkt}}
]

@section{September 13, 2017}

@vidlink["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=e3bfd03f-c3b4-4895-8ddb-576c2052d2b3"]

Code from today's lecture:

@itemlist[
@item{@link["render-string.rkt"]{render-string.rkt}}
@item{@link["chip0.rkt"]{chip0.rkt}}
]

@section{September 15, 2017}

@vidlink["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=dd5dfd8f-936a-46dc-a38a-5197e7706fbe"]

Code from today's lecture (remember, it has failing test cases):

@itemlist[
@item{@link["chip1.rkt"]{chip1.rkt}}
]

Today's quiz:

A @tt{Coord} is a @tt{(make-posn Integer Integer)}.
Interp: a point on the Cartesian plane.

Write a function @tt{dist : Coord -> Number} that computes the
distance from the origin.

Recall: distance of (x,y) to (0,0) is √(x²+y²).

You do not need to perform all steps of the DR, just define the
function.

@section{September 18, 2017}

@vidlink["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=7bed18c4-092f-4bcc-89bf-90326ea750b5"]

We didn't really write any code; we discussed templates, stubs, the
three kinds of errors, and failing test cases.

Today's quizzes were (1) exactly the quiz from Friday and (2):

@verbatim{
A Lec is one of:
- "M"
- "W"
- "F"
Interp: day of the week for 131A lecture

Write a template for a Lec function.
}


Here are some common issues encountered so far in grading
@secref{assign3}:

@itemize[

@item{Most students place test cases below their def'n
  instead of between signature and def'n.}

@item{ Some people included function examples in comments
  without writing concrete test cases. Some had both.}

@item{Almost @bold{every} submission had some incorrect
  indentation and long lines.}

@item{The @tt{format-month} almost universally lacked helper
  usage and contained long lines.}

@item{Some students defined "helpers" that didn't "help" at
  all, basically like:
  @verbatim{
    (define (supposed-to-implement x)
      (so-called-helper x))
      }
      }

@item{Many submissions had stubs still left in their code either as a
  comment or as a defined function like @tt{(define (format-month-stub
  ...) "Nov")}.}

@item{There were lots of submissions with just templates defined for
  every function, or data templates that were just wrong.}

@item{General disorganization was common, e.g. signatures and test
  cases thrown about randomly and not coupled together with the
  function def'n. Some people renamed the functions to something else
  which made it difficult to grade.}

@item{Lots of long function bodies were defined on the same line as
  the "define".}

@item{Commonly @tt{init-time} was defined as a function instead of
  just as an expression in terms of @tt{init-*} constants.}

@item{A surprising amount of submission weren't even grammatically
  well-formed BSL programs. :(}

]

Make sure you correct any of these issues if they occur in your program
for @secref{assign4}.

@section{September 20, 2017}

@vidlink["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=403e60d0-6858-4efd-88df-b186aa44e8b1"]

Here is the code for simple space invaders designed using the design
recipe and adhering to @secref{style}:

@itemlist[
  @item{@link["invaders.rkt"]{invaders.rkt}}
]

I've made a two part video that develops the invader program from
scratch, resulting in the code above.  You can watch this to get a
complete example of following the DR through on an involved example.

@itemlist[
  @item{@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=1a37f226-9e62-4765-ba88-7df756112a12"]{Part I}}
  @item{@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=d335e86e-39eb-4d33-a2c3-975887822deb"]{Part II}}
]
