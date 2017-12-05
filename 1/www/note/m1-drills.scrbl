#lang scribble/manual
@(require racket/sandbox
          scribble/example
          "../vid.rkt")


@title[#:tag "m1-drills"]{Midterm 1 Drills}

Here are some drill questions to practice for the first midterm.
These only cover topics we've covered so far in class, but more topics
will be on the midterm.  I will post relevant drill problems as
appropriate.

@section{Simple computations}

Determine what the following programs evaluate to:

@itemize[
@item{@racketblock[(- (sqr 5))]}
@item{@racketblock[(modulo 12 5)]}
@item{@racketblock[(+ (/ 4 3) 1)]}
@item{@racketblock[(string-append "Hello" "World")]}
@item{@racketblock[(string-append "" "Fred")]}
@item{@racketblock[(substring "manslaughter" 4)]}
@item{@racketblock[(above (circle 10 "outline" "black") (square 5 "outline" "black"))]}
@item{@racketblock[(substring (string-append "te" "am") 1 3)]}
@item{@racketblock[(+ (posn-x (make-posn 3 4)) (posn-x (make-posn 10 2)))]}
@item{@racketblock[
(define-struct name (first last))
(make-name "James" "Bond")]}
@item{@racketblock[
(define first-name "James")
(define last-name "Bond")
(string-append last-name ", " first-name " " last-name)]}
@item{@racketblock[
(define x 3)
(define y 4)
(sqrt (+ (sqr x) (sqr y)))]}
@item{@racketblock[
(define-struct name (first last))
(define ms (make-name "Michael" "Scott"))
(define sm (make-name (name-last ms) (name-first ms)))
(name-last sm)]}
@item{@racketblock[
(define (f x)
  (/ (sqr x) 2))
(+ (f 5) (f 2))]}
@item{@racketblock[
(define (g y)
  (cond [(<= (string-length y) 4) 3.25]
        [else "yellow"]))
(g "pie")
]}
]

@section{Stepping through computations}

Write out each step of computation.  At each step, underline the
expression being simplified.  Label each step as being "arithmetic"
(meaning any built-in operation), "conditional", "plug" (for plugging
in an argument for a function parameter), or "constant" for replacing
a constant with its value.

@itemize[
@item{@racketblock[(+ (sqr 5) (add1 2))]}
@item{@racketblock[
(define Q 2)
(define (h z)
  (+ (* z 5) Q))
(cond [(= (h 1) 7) (add1 9)]
      [(= (h 2) 12) 4])]}
@item{@racketblock[
(define (hi name)
  (string-append "Hi " name "!"))
(hi (substring "DVH" 0 1))
]}
@item{@racketblock[
(define s (make-posn "GOOG" 99))
(cond [(< 100 (posn-y s)) "sell"]
      [(> 100 (posn-y s)) "buy"]
      [else "nada"])]}
]

@section{Classifying errors}

Classify the following programs as having a syntax error, a run-time
error, logical error, or having no errors.

@itemize[
@item{@racketblock[
(define (f x) (expt 2 x))
(check-expect (f 4))
]}
@item{@racketblock[
(string-append "Hi " (substring "DVH" 1 4) )
]}
@item{
@#reader scribble/comment-reader (racketblock
;; dist : Number Number -> Number
;; Compute distance to origin of (x,y)
(define (dist x y)
  (sqrt (+ (sqr x) (sqr x))))
(dist 3 4))
}
@item{@racketblock[
(define h z (+ z (sqr z)))
]}]

@section{Stubs, Templates}

Assume the following data definitions:

@#reader scribble/comment-reader (racketblock
;; A Name is a (make-name String String)
(define-struct name (first last))

;; A Shape is one of:
;; - (make-rect Integer Integer)
;; - (make-circ Integer)
(define-struct rect (width height))
(define-struct circ (radius))

;; A Drawing is one of:
;; - Shape
;; - (make-posn Shape Shape)

;; A Price is one of
;; - [0,99)
;; - [99,999)

;; A Move is one:
;; - "N"
;; - "E"
;; - "W"
;; - "S"
;; - "NE"
;; - "NW"
;; - "SE"
;; - "SW"

;; A MaybeMove is one of:
;; - Move
;; - #false

;; A Niner is a 9

;; A Biz is one of:
;; - "a"
;; - 7
;; - #true
;; - (make-posn 9 9)
)

Write templates for each of these data definitions.

Write stubs for each of these signatures.

@#reader scribble/comment-reader (racketblock
;; greeting : Name -> String
;; cheap? : Price -> Boolean
;; next-move : Move -> MaybeMove
;; area : Drawing -> Number
;; sign : Shape -> Name
;; inc : Number -> Niner
;; choose : Move Move -> Move
;; cost : Drawing -> Price
;; cap : Shape String Image -> Biz
;; same-price? : Price Price -> Boolean
;; bribe : Price -> Name
;; change-last : Name String -> Name
;; dot : String Integer -> String
;; cross : String Move -> Shape
;; all-on? : Shape Shape Shape Shape -> Boolean
;; flip : Move -> Move
;; rotate : Image MaybeMove -> Shape
)

@section{Designing functions}

@#reader scribble/comment-reader (racketblock
;; A Name is a (make-name String String)
;; Interp: a person's full (first and last) name.
;; Both strings must contain at least one letter.
(define-struct name (first last))
)

Design a function that creates a opening phrase of a letter.  For
example, given the full name David Van Horn, produces @racket["Dear David,"].

Design a function that is given two full names and a first name.  It
should produce a new name using the given first name and a hyphenated
combination of the last names of the two full names.  For example,
given full names Ed Tobin, Laura Hochstadt, and the first name Sam, it
should produce the full name Sam Tobin-Hochstadt.

Design a function that is given a full name and produces a ``private''
version of the name that abbreviates the last name to the first letter
and a period.  So for example, given the full name David Van Horn,
then the full name David V. would be the produced.

Design a function that is given two full names and determines whether
the two people have the same first name.

@#reader scribble/comment-reader (racketblock
;; A Dir is one of:
;; - "N"
;; - "E"
;; - "W"
;; - "S"
;; Interp: North, East, West, and South.
)

Design a function that computes the opposite of a given direction.

Design a function that determines if two directions are the same.

Design a function that determines if two directions are opposites of
each other.

Design a function that computes a right turn from the given direction.

Design a function that given a direction computes a name (as in a
Name) like this: given "S" it should compute "South Southwest" where
South is the first name and Southwest is the last name.  North should
give you North Northwest, etc.

@#reader scribble/comment-reader (racketblock
;; A Coord is a (make-posn Integer Integer)
;; Interp: a Cartesian coordinate
)

Design a function that takes two coordinates and computes a coordinate
representing their sum, e.g. (x1,y1) + (x2,y2) = (x1+x2,y1+y2).

Design a function that, given a coordinate (x,y), computes the area of
the triangle formed by (0,0), (x,0), and (x,y).  Recall the area of a
triangle is 1/2 * base * height.

Design a function that, given a coordinate (x,y), computes the
perimeter of the triangle formed by (0,0), (x,0), (x,y).

Design a function that reflects a coordinate over the x-axis,
e.g. (5,3) becomes (5,-3).

@section{Solutions}

Here are videos going through solutions to each part of the drill
problems:

@itemize[

@item{@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=d3777989-f2b3-4a00-8253-e16ed9e9f655"]{Simple computations}
      @panopto-vid{https://umd.hosted.panopto.com/Panopto/Pages/Embed.aspx?id=d3777989-f2b3-4a00-8253-e16ed9e9f655&v=1}}
@item{@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=fd80e685-e204-4e1c-b323-76ca7478da4e"]{Stepping through computations}
      @panopto-vid{https://umd.hosted.panopto.com/Panopto/Pages/Embed.aspx?id=fd80e685-e204-4e1c-b323-76ca7478da4e&v=1}}
@item{@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=435ebe5b-dded-4922-92c9-48386a9c5c07"]{Classifying errors}
      @panopto-vid{https://umd.hosted.panopto.com/Panopto/Pages/Embed.aspx?id=435ebe5b-dded-4922-92c9-48386a9c5c07&v=1}}
@item{@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=82f70804-e2fd-42bb-9b66-7eb1346ec6e0"]{Templates}
      @panopto-vid{https://umd.hosted.panopto.com/Panopto/Pages/Embed.aspx?id=82f70804-e2fd-42bb-9b66-7eb1346ec6e0&v=1}}
@item{@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=52171455-3691-4a1d-811a-48a219355c68"]{Stubs}
      @panopto-vid{https://umd.hosted.panopto.com/Panopto/Pages/Embed.aspx?id=52171455-3691-4a1d-811a-48a219355c68&v=1}}
@item{@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=7ba10792-a8ae-4b21-aff4-c63afb87e448"]{Designing Name functions}
      @panopto-vid{https://umd.hosted.panopto.com/Panopto/Pages/Embed.aspx?id=7ba10792-a8ae-4b21-aff4-c63afb87e448&v=1}}
@item{Designing Dir functions : the video file was corrupted}
@item{@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=497e4719-12a5-4749-a2f2-274971ba8bf5"]{Designing Coord functions}
      @panopto-vid{https://umd.hosted.panopto.com/Panopto/Pages/Embed.aspx?id=497e4719-12a5-4749-a2f2-274971ba8bf5&v=1}}

]
