#lang scribble/manual

@; Would be nice if you could run definitions-area examples, but you can't
@;(define isl-eval (make-base-eval #:lang '(special intermediate-lambda)))

@title[#:tag "final-drills"]{Final Exam Drills}

The final exam is cumulative, so you should review @secref{m1-drills}
and @secref{m2-drills}.  The following drills only cover material
introduced since the second midterm.

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
n-1.

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
  (cond [(empty? xs) 1]
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
;; minf : [X] [X -> Real] [NEListof X] -> Real
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
function only @math{n-1} times.  The trick is that you will need
two accumulators: the minimizing element seen so far and the result
of applying the function to that minimizing element.

Design a 2-accumulator-based version of @racket[minf] and argue why
it only calls @racket[f] @math{n-1} times.




@section{Generative recursion}

@section{Invariants and BSTs}

@section{Graphs}

When we studied graphs we used the follow data representation for graphs:

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
