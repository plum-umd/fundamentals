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

@include-section["note/rocket.scrbl"]

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


@section{September 22, 2017}

@vidlink["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=644f319e-0d3c-4cd7-bd77-5b75ed3dafaa"]


Code from today:
@itemlist[
@item{@link["shots.rkt"]{shots.rkt}}
]

@include-section{note/m1-drills.scrbl}

@section{September 25, 2017}

@vidlink["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=5a3cb585-3566-4f58-981c-d720d0519c86"]

Here are the comments I made in regard to what we've seen so far in
assignment 4 submissions:
@link["feedback-assign4.pdf"]{feedback-assign4.pdf}.

Here is the code for today (I re-arranged it slightly to follow the
style guidelines):

@itemlist[
@item{@link["shots-draw-on.rkt"]{shots-draw-on.rkt}}
]

@section{Midterm practice exam}

Here is a midterm practice exam (and solution) to help prepare for the upcoming
midterm:
@itemlist[
@item{@link["m1-practice.pdf"]{m1-practice.pdf}}
@item{@link["m1-practice-soln.pdf"]{m1-practice-soln.pdf}}
]


@section{September 27, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=4c651ab2-35ac-467b-bf6b-c4eeb19dc56a}

Here is the code for today; we skipped the section on lists of
strings, so see if you can do that on your own and also have a go at
the functions on lists of lists of numbers we didn't get to.

@itemlist[
@item{@link["lists-1.rkt"]{lists-1.rkt}}
]

Today's quiz:

@verbatim{
;; An Onion is one of:
;; - (make-bulb)
;; - (make-skin Onion)
(define-struct bulb ())
(define-struct skin (inner))}

Write a template for @tt{Onion} functions.

@section{September 29, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=622f0c0f-7f16-4e64-afb8-1ade64a6ca3a}

Today's quiz:

@verbatim{
;; An Onion is one of:
;; - (make-bulb)
;; - (make-skin Onion)
(define-struct bulb ())
(define-struct skin (inner))}

Write the definition of the count-skins function:

@verbatim{
(define b (make-bulb))

;; count-skins : Onion -> Natural
;; Count the number of skins on the onion
(check-expect (count-skins b) 0)
(check-expect (count-skins (make-skin b)) 1)
(check-expect (count-skins (make-skin (make-skin b))) 2)}

Code for today:
@itemlist[
@item{@link["lists-2.rkt"]{lists-2.rkt}}
]

@section{Pair programming Space Invaders with shots}

On Friday, Austin Bourgerie and I sat down to pair program the Space
Invaders portion of @secref{assign5}.  We recorded the session in
hopes of showing:
@itemlist[
@item{(a) how the head and hands model of pair programming
can be effective in rapidly thinking through and solving problems and}
@item{
(b) how sticking to the design process makes short order of the
assignment.}]

We were able to complete that part of the assignment in 1 hour.  As
you watch the video, I hope you'll realize we were able to get through
it so quickly not because we are overly smart, experienced, or have an
encyclopedic knowledge of BSL or Space Invaders---we got through it so
fast because we stuck to the process and went slow to quickly get to a
well-designed program.  We didn't do anything that you couldn't also
do.  We made a few small mistakes along the way, but we found them and
recovered quickly, thanks to the process.

I made only one change to the code after we finished, which is I
deleted all the obsoleted code having to do with @tt{Aim} and
@tt{Fire}.

@itemlist[
@item{@link["invader-shots-dvanhorn-abourg.rkt"]{Code}}
@item{@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=6a35153d-959d-4e1e-a7a4-79946c075452"]{Video}}
]

@section{October 4, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=f26b466e-78bd-43f9-a3a3-23cc776fa74b}

Today's code:

@itemlist[
  @item{@link["sort.rkt"]{sort.rkt}}
]

@section{October 6 & 9, 2017}

@include-section["note/snake.scrbl"]

@section{October 11, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=24a1b149-639b-4230-9cc7-6c007a35d34d}

Snake code:
@itemlist[
 @item{@link["snake-in-progress.rkt"]{snake-in-progress.rkt}}
 @item{@link["snake-complete.rkt"]{snake-complete.rkt}}
]

Today's code:

@itemlist[
  @item{@link["addn-abstract.rkt"]{addn-abstract.rkt}}
  @item{@link["even-odd-abstract.rkt"]{even-odd-abstract.rkt}}
  @item{@link["f-all-abstract.rkt"]{f-all-abstract.rkt}}
]

@section{October 13, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=d58ba7b4-9c7b-42ab-b725-d0de765cb4cf}

Today's code:

@itemlist[
  @item{@link["prod-sum-abstract.rkt"]{prod-sum-abstract.rkt}}
]

@section{October 16, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=7e3f2c63-b17c-4889-ae85-6c1ef6c5236b}



@section{October 18, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=61a0be80-3729-4ec6-b083-4695d06ef9ee}

Today's code: @link["using-abstractions.rkt"]{using-abstractions.rkt}

Today's quiz:

@#reader scribble/comment-reader (racketblock
;; my-append : [X] [Listof X] [Listof X] -> [Listof X]
;; Append the elements of ls1 and ls2
(check-expect (my-append '() '())
              '())
(check-expect (my-append '() (list 4 5 6))
              (list 4 5 6))
(check-expect (my-append (list 1 2 3) '())
              (list 1 2 3))
(check-expect (my-append (list 1 2 3) (list 4 5 6))
              (list 1 2 3 4 5 6))
(define (my-append ls1 ls2)
  (cond [(empty? ls1) ls2]
        [(cons? ls1)
         (cons (first ls1)
               (my-append (rest ls1) ls2))]))

;; Quiz: Give an equivalent definition of my-append
;; in terms of foldr

;; Recall:
;; foldr : [X Y] (X Y -> Y) Y [Listof X] -> Y
)

@section[#:tag "m1-soln-videos"]{Midterm 1 solution videos}

Here is a series of videos going through the exam and constructing
answers:

@itemlist[
@item{@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=0829900b-418b-4465-aa13-860c1f044a4c"]{Problem 1}}
@item{@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=70faff06-27b1-4a1a-997a-6181c8c280c1"]{Problem 2}}
@item{@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=e6dc6f38-8888-43cf-85cd-eec7440102de"]{Problem 3}}
@item{@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=5892d33d-ae17-4416-93a3-3c5590aa6d0a"]{Problem 4}}
@item{@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=4a5a743c-3292-46a2-a89c-f288a7d27fb1"]{Problem 5}}
@item{@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=2579bdfb-95a2-4a99-ada9-9063ac39e7e6"]{Problem 6}}
@item{@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=eeeebf01-6745-4f6e-8c81-78f4da782d4b"]{Problem 7}}
]

@section{October 20, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=87ee9932-ab7b-4f40-b2dc-1a05c39eb4c5}

The code will not be posted for this lecture (but can be seen in the video).

The first quiz from today:

@#reader scribble/comment-reader (racketblock
;; flatten : [X] [Listof [Listof X]] -> [Listof X]
;; Flatten a list of lists of elements into a list of elements
(check-expect (flatten '()) '())
(check-expect (flatten (list (list 1 2 3)) (list 4 5) (list 6))
              (list 1 2 3 4 5 6))
(define (flatten lolox)
  (cond [(empty? lolox) '()]
        [(cons? lolox)
         (append (first lolox)
                 (flatten (rest lolox)))]))

;; Quiz: Give an equivalent definition of flatten
;; in terms of foldr

;; Recall:
;; foldr : [X Y] (X Y -> Y) Y [Listof X] -> Y
)

The second quiz:

@#reader scribble/comment-reader (racketblock
;; largest : [Listof Number] Number -> Number
;; Determine the largest number among all elements of the list and given number
(check-expect (largest '() 5) 5)
(check-expect (largest (list 1 2 3) 5) 5)
(check-expect (largest (list 6 5 4) 5) 6)
(define (largest lon n)
  (cond [(empty? lon) n]
        [(cons? lon)
         (max (first lon)
              (largest (rest lon) n))]))

;; Quiz: Give an equivalent definition of largest
;; in terms of foldr

;; Recall:
;; foldr : [X Y] (X Y -> Y) Y [Listof X] -> Y
)

@section{October 23, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=2157c1f1-ba53-459f-9e73-01fb293c154a}

Again the code will not be posted, but can be seen in the video.

Today's quiz:

@#reader scribble/comment-reader (racketblock
;; all-longer-than? : [Listof String] Number -> Boolean
;; Are all of the strings longer than n characters long?
(check-expect (all-longer-than? '() 1) #true)
(check-expect (all-longer-than? (list "a" "b" "c") 1) #false)
(check-expect (all-longer-than? (list "abc" "def") 1) #true)
(define (all-longer-than? los n)
  (cond [(empty? los) #true]
        [(cons? los)
         (and (> (string-length (first los)) n)
              (all-longer-than? (rest los) n))]))

;; Quiz: Give an equivalent definition of all-longer-than?
;; in terms of andmap

;; Recall:
;; andmap : [X] (X -> Boolean) [Listof X] -> Boolean
)

@section{October 25, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=8a91183a-94c3-4189-9895-26a907e07e2c}

Today's quiz:

@#reader scribble/comment-reader (racketblock
;; erma : ____________________________
(check-expect (erma (list sqr add1)) (list 25 6))
(check-expect (erma (list number->string)) (list "5"))
(define (erma lof)
  (cond [(empty? lof) '()]
        [(cons? lof)
         (local [(define f (first lof))]
           (cons (f 5)
                 (erma (rest lof))))]))

;; QUIZ: Give a parametric signature for erma.
)

Today's code: @link["adventure.rkt"]{adventure.rkt}.

@section{October 27, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=dcd9a394-650e-46a8-93c6-fd6a55d3e47d}

Today's code: @link["adventure2.rkt"]{adventure2.rkt}.

@section{Midterm 2 practice}

Here is the second midterm practice exam: @link["m2-practice.pdf"]{m2-practice.pdf} (@link["m2-practice-soln.pdf"]{m2-practice-soln.pdf}).

@section{October 30, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=4a74da6a-fcf8-4080-b077-646d773e4d80}

Today's code: @link["multi-inputs.rkt"]{multi-inputs.rkt}.

Today's quiz:
@#reader scribble/comment-reader (racketblock
;; wilma : ___________________________
(check-expect (wilma add1 5) (list 6 5 4 3 2 1))
(check-expect (wilma number->string 3) (list "3" "2" "1" "0"))
(define (wilma f n)
  (cond [(zero? n) (list (f 0))]
        [else
          (cons (f n)
                (wilma f (sub1 n)))]))

;; QUIZ: Give a parametric signature for wilma.
)

@section{November 1, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=c22c5ff4-2bd2-44df-97f7-f24c166e6f4f}

Today's code:

@itemlist[
@item{@link["merge.rkt"]{merge.rkt}}
@item{@link["bundle.rkt"]{bundle.rkt}}
]

@include-section{note/m2-drills.scrbl}

@section{November 3, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=87a30bf0-ad24-419f-987e-820778010926}

Code from today: @link["qsort.rkt"]{qsort.rkt}.

@include-section{note/lab-19-soln.scrbl}

@section{November 10, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=413ce506-6e9e-41aa-87bb-9a530bd269ff}

@section{November 13, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=4b05ebf2-b21d-44cb-a4c5-d6d9ee892d87}

@section{November 15, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=bc83ec9e-5938-4daa-a267-1936b796f1a1}

@section{November 17, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=ffcf6b0e-2ab6-4db2-9160-f68ef2643c30}

Code: @link["graph.rkt"]{graph.rkt}.

@section{November 20, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=36c1e7d0-1a25-4865-b0aa-bea72795e530}

Code: @link["graph2.rkt"]{graph2.rkt}.

@section{November 27, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=8f3ea164-1ce0-4562-83b4-6031168b5ba8}

Code: @link["typed.rkt"]{typed.rkt}.

@section{November 29, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=2dfcf5f5-d906-444e-8469-1177733739e1}

Code: @link["typed2.rkt"]{typed2.rkt}.

@section{December 1, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=529f5dfd-c2e9-4dbd-a782-f978052f3d06}

Code: 
@itemize[
@item{@link["typed3.rkt"]{typed3.rkt}}
@item{@link["yo-client.rkt"]{yo-client.rkt}}
@item{@link["yo-server.rkt"]{yo-server.rkt}}
]

@section[#:tag "yo-lec"]{December 4, 2017}

@vidlink{https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=5b014425-d05f-4cb1-a873-7a0351683449}

Code: 
@itemize[
@item{@link["yo-client2.rkt"]{yo-client2.rkt}}
@item{@link["yo-server2.rkt"]{yo-server2.rkt}}
]

The client has been filled-in to be a full implementation.  The server
code is as it was in class.

@include-section{note/final-drills.scrbl}
