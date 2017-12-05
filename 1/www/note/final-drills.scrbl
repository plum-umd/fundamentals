#lang scribble/manual

@; Would be nice if you could run definitions-area examples, but you can't
@;(define isl-eval (make-base-eval #:lang '(special intermediate-lambda)))

@title[#:tag "final-drills"]{Final Exam Drills}

The final exam is cumulative, so you should review @secref{m1-drills}
and @secref{m2-drills}.  The following drills only cover material
introduced since the second midterm.

Solutions: @link["drill-final-exam-soln.rkt"]{drill-final-exam-soln.rkt}.

@section{Designing with accumulators}

Here is a design of the @racket[factorial] function using structural
recursion on natural numnbers:

@#reader scribble/comment-reader (racketblock
;; factorial : Natural -> Natural
;; Compute n!
(check-expect (factorial 5) 120)
(define (factorial n)
  (cond [(zero? n) 1]
        [else 
         (* n (factorial (sub1 n)))])))

Design a variant of @racket[factorial] that uses structural recursion
on @racket[n] with an accumulator that represents the factorial of
all the numbers seen so far.

Here is a design of the @racket[largest] function that uses structural
recursion on a non-empty list of (real) numbers to compute the largest
number in the list:

@#reader scribble/comment-reader (racketblock
;; largest : [NEListof Real] -> Real
;; Finds largest element in given non-empty list
(check-expect (largest (list 7)) 7)
(check-expect (largest (list 2 9 7)) 9)
(define (largest ns)
  (cond [(empty? (rest ns)) (first ns)]
        [else 
         (max (first xs)
              (largest (rest xs)))]))
)

Design a variant of @racket[largest] that uses structural recursion on
@racket[ns] with an accumulator that represents the largest element of
the list seen so far.

Here is a design of the @racket[prod] function that computes the
product of a list of numbers:

@#reader scribble/comment-reader (racketblock
;; prod : [Listof Number] -> Number
;; Compute the product of the list
(check-expect (prod '()) 1)
(check-expect (prod (list 1 2 3 4 5)) 120)
(define (prod xs)
  (cond [(empty? xs) 1]
        [(cons? xs)
         (* (first xs) 
            (prod (rest xs)))]))
)

Here is a design of @racket[sum]:

@#reader scribble/comment-reader (racketblock
;; sum : [Listof Number] -> Number
;; Compute the sum of the list
(check-expect (sum '()) 0)
(check-expect (sum (list 1 2 3 4 5)) 15)
(define (sum xs)
  (cond [(empty? xs) 0]
        [(cons? xs)
         (+ (first xs) 
            (sum (rest xs)))]))
)

Give alternative designs that use an accumulator to represent the
product (resp. sum) of the numbers seen so far.

Based on your accumulator variant of @racket[prod] and @racket[sum],
design an abstraction of these two functions.  Give it the most
general signature you can.  Reformulate your accumulator-based
@racket[prod] and @racket[sum] functions in terms of this abstraction.

What does the abstraction remind you of?

Can you use your abstraction to reformulate @racket[largest]?


Here is the design of a function called @racket[minf] that consumes a
function producing real numnbers and a non-empty list of input
elements and finds the (earliest) element in the list that
@emph{minimizes} the output of the function:

@#reader scribble/comment-reader (racketblock
;; minf : [X] [X -> Real] [NEListof X] -> X
;; Find the earliest element that minimizes f.
(check-expect (minf sqr (list -4 2 8)) 2)
(check-expect (minf sqr (list -4 -2 8 2)) -2)
(define (minf f xs)
  (cond [(empty? (rest xs)) (first xs)]
        [else
         (local [(define x1 (first xs))
                 (define y1 (f x1))
                 (define xn (minf f (rest xs)))
                 (define yn (f xn))]
           (if (<= y1 yn) x1 xn))]))
)

The observant reader will notice that function @racket[f] is called
@math{2 × (n-1)} times (where @math{n} is the length of the list).
Using an accumulator-based design, you can do better, calling the
function at most @math{n} times.  The trick is that you will need two
accumulators: the minimizing element seen so far and the result of
applying the function to that minimizing element; also note that you
start these values off by using the first element of the list (which
is guaranteed to exist).

Design a 2-accumulator-based version of @racket[minf] and argue why
it only calls @racket[f] at most @math{n} times.




@section{Generative recursion}

Suppose we have a data definition for "ascening strings," i.e. strings
that consist of letters that must appear in alpahabetic order in the
string.  For example, @racket["aabcz"] and @racket["bdrtu"] are
ascending, but @racket["efarw"] is not.

@#reader scribble/comment-reader (racketblock
;; An AscString is a String
;; where (explode s) is sorted by string<?
)

Design a function that is given an @racket[AscString] and a
@racket[1String] and determines if the given letter occurs in the
string.  It should use generative recursion to be more efficient than
scanning the list.  (You should not use @racket[string-contains?].)

@;fast-exponent
@;find-root

@section{Invariants}

A @emph{Quad Tree} is an important data structure for representing a
set of spatial coordinates, for example a set of GPS locations, and
supporting very efficient queries about whether a given point is in
the set of locations or not.  

The idea is to represent a set of points using a tree.  Each node in
the tree contains a point and four subtrees.  The four subtrees have
an important invariant: one holds all the points that are to the
Northwest of the point in the node, another holds the points to the
Northeast, another the Southeast, and another the Southwest.

Here is a data definition for quad trees:

@#reader scribble/comment-reader (racketblock
;; A QT (Quad Tree) is one of:
;; - #false
;; - (make-quad Posn QT QT QT QT)
(define-struct quad (pt ne nw se sw))
;; Interp: a set of positions, #false represents an empty set
;; Invariant: 
;;  - all points in ne are north (y is smaller) & east (x is larger) of pt
;;  - all points in nw are north (y is smaller) & west (x is smaller) of pt
;;  - all points in se are south (y is larger) & east (x is larger) of pt
;;  - all points in sw are south (y is larger) & west (x is larger) of pt
)

(Note this uses the convention of graphics coordinates in which the
y-axis grows downward.)

Define the following function, taking advantage of this invariant to 
compute the answer efficiently:

@#reader scribble/comment-reader (racketblock
;; contains? : Posn QT -> Boolean
;; Is the given position in the set?
)

@section{Graphs}

When we studied graphs we used the follow data representation for
graphs (@secref{graph-lec}):

@#reader scribble/comment-reader (racketblock
;; A Graph is [Listof (cons String [Listof String])]
;; Interp: a graph is a list of nodes and neighbors reachable from the node
)

So a graph such as this:

@image{img/graph2.png}

is represented as:

@#reader scribble/comment-reader (racketblock
(define G
  (list (list "A" "B" "E")
        (list "B" "E" "F")
        (list "C" "D" "B")
        (list "D")
        (list "E" "C" "F")
        (list "F" "D" "G")
        (list "G")))
)

Alternatively, we could have represented a graph as a list of edges:

@#reader scribble/comment-reader (racketblock
;; A Graph is [Listof (list String String)]
;; Interp: a graph is a list of edges from one node to another
)

Under this definition, the graph above would be represented as:

@#reader scribble/comment-reader (racketblock
(define G
  (list (list "A" "B")
        (list "A" "E")
        (list "B" "E")
        (list "B" "F")
        (list "C" "D")
        (list "C" "B")
        (list "E" "C")
        (list "E" "F")
        (list "F" "D")
        (list "F" "G")))
)

Re-develop the @racket[path] function in light of this new representation:

@#reader scribble/comment-reader (racketblock
;; path : Graph String String -> [Maybe [Listof String]]
;; Compute a path in the graph between the given nodes, if one exists
;; Produces #false if no path exists
)


@section{Types}

Which of the following programs well-typed (according to Typed
Racket's type system)?

@#reader scribble/comment-reader (racketblock
(: f : (All (A) (Number -> A) -> A))
(define (f x)
  (x 3))

(: g : (U String Number) -> (U String Number))
(define (g x)
  (cond [(string? x) (+ x 3)]
        [(number? x) (string-append "hi" x)]))

(: h : (U String Number) -> (U String Number))
(define (h x)
  (cond [(string? x) (string-append "hi" x)]
        [(number? x) (+ x 3)]))

(: i : Number -> Real)
(define (i x)
  (if (< x 10)
      (sqr x)
      (+ x 50)))

(: j : Number -> [Listof Number])
(define (j x)
  (map (λ ([z : (Number -> Number)]) (z x))
       (list add1 sqr sqrt)))

(: k : Number String -> [Listof String])
(define (k x y)
  (cons (string-append (number->string x) y)))
)


Provide the most general valid type signature for the following functions.
Provide type annotations needed for any @racket[λ] parameters.

@#reader scribble/comment-reader (racketblock
(define (lengths xs)
  (map length xs))
 
(define (total-length xs)
  (foldr (λ (x t) (+ (length x) t)) 0 xs))
 
(define (map-f-zero lof)
  (map (λ (f) (f 0)) lof))
)

Challenge problem (nothing this tricky will be on the exam): provide
the most general type signature and annotions for this function:

@#reader scribble/comment-reader (racketblock
;; Abstraction of outer "product" computations
(define (outer f l1 l2)
  (cond [(empty? l2) '()]
        [else (map (λ (m) (map (λ (n) (f m n)) l2)) l1)]))
)