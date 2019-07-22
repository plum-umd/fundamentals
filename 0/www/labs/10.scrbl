#lang scribble/manual
@(require scribble/core 
          (for-label lang/htdp-beginner-abbr) 
          "helper.rkt"
          "../utils.rkt")

@title[#:style 'unnumbered #:tag "lab10"]{Lab 10: Rothko's Chapel}

@(define ex (make-exerciser "Lab problem"))

@section[#:tag "lab10intro"]{Introduction(s)}

You'll work in labs in pairs.  Find someone to work with for this
first lab and introduce yourself. 

Make sure at least one of you have a laptop to work on for this lab.

The two of you will work as a team to solve problems. At any time, one of you
will be the @bold{Head} and the other will be the @bold{Hands}. The @bold{Head}
does the thinking and the @bold{Hands} does the typing. @bold{Hands} type only
what the @bold{Head} tells them to, but you're free to discuss any issues that
pop up. We'll have you switch off during the lab to make sure each of you get
practice problem solving, dealing with syntax, and getting finger exercises on
the keyboard.

@section[#:tag "lab10:purpose"]{Purpose}

In this lab, you'll practice applying the abstraction process.


@section[#:tag "lab10:abstracting"]{Painting and abstracting}

Here is a partial solution to @secref{ex5}:

@#reader scribble/comment-reader (racketblock
;; LoN -> Natural
;; Count the number of elements in a given list of numbers.
(check-expect (lon-count '()) 0)
(check-expect (lon-count (list 7 1 4)) 3)
(define (lon-count lon)
  (cond [(empty? lon) 0]
        [(cons? lon) (+ 1 (lon-count (rest lon)))]))

;; LoN -> Number
;; Compute the product of the given list of numbers.
(check-expect (lon-product '()) 1)
(check-expect (lon-product (list 7 1 4)) (* 7 1 4))
(define (lon-product lon)
  (cond [(empty? lon) 1]
        [(cons? lon)
         (* (first lon)
            (lon-product (rest lon)))]))

;; LoN -> LoN
;; Add one to every element of a given list.
(check-expect (lon-add1-all '()) '())
(check-expect (lon-add1-all (list 7 1 4)) (list 8 2 5))
(define (lon-add1-all lon)
  (cond [(empty? lon) '()]
        [(cons? lon)
         (cons (add1 (first lon))
               (lon-add1-all (rest lon)))]))

;; LoN -> LoN
;; Compute the absolute value of every element of a given list
(check-expect (lon-abs-all '()) '())
(check-expect (lon-abs-all (list -7 1 -4)) (list 7 1 4))
(define (lon-abs-all lon)
  (cond [(empty? lon) '()]
        [(cons? lon)
         (cons (abs (first lon))
               (lon-abs-all (rest lon)))]))

;; LoN -> LoN
;; Produce a list that contains only the even elements of a given list.
(check-expect (lon-filter-even '()) '())
(check-expect (lon-filter-even (list 7 1 4)) (list 4))
(define (lon-filter-even lon)
  (cond [(empty? lon) '()]
        [(cons? lon)
         (cond [(even? (first lon))
                (cons (first lon) (lon-filter-even (rest lon)))]
               [else (lon-filter-even (rest lon))])]))

;; LoN -> Boolean
;; Determine if every element of a given list is odd.
(check-expect (lon-all-odd? '()) #true)
(check-expect (lon-all-odd? (list 7 1 4)) #false)
(check-expect (lon-all-odd? (list 7 1 3)) #true)
(define (lon-all-odd? lon)
  (cond [(empty? lon) #true]
        [(cons? lon)
         (and (odd? (first lon))
              (lon-all-odd? (rest lon)))]))
)

Here's a related set of problems, where every step but the @emph{code}
step of the design recipe has been completed for you (so finishing
these functions should be straightforward).

@#reader scribble/comment-reader (racketblock
;; LoN -> LoN
;; Produce a list that contains only the positive elements of a given list.
(check-expect (lon-filter-positive '()) '())
(check-expect (lon-filter-positive (list 7 -1 4)) (list 7 4))
(check-expect (lon-filter-positive (list 7 1 0)) (list 7 1))
(define (lon-filter-positive lon)
  (cond [(empty? lon) ...]
        [(cons? lon)
         (... (first lon)
              (lon-filter-positive (rest lon)) ...)]))

;; LoN -> Number
;; Compute the sum of the given list of numbers.
(check-expect (lon-sum '()) 0)
(check-expect (lon-sum (list 7 1 4)) (+ 7 1 4))
(define (lon-sum lon)
  (cond [(empty? lon) ...]
        [(cons? lon)
         (... (first lon)
              (lon-sum (rest lon)) ...)]))

;; LoN -> Boolean
;; Determine if every element of a given list is positive.
(check-expect (lon-all-positive? '()) #true)
(check-expect (lon-all-positive? (list 7 1 4)) #true)
(check-expect (lon-all-positive? (list 7 0 4)) #false)
(define (lon-all-odd? lon)
  (cond [(empty? lon) ...]
        [(cons? lon)
         (... (first lon)
              (lon-all-positive? (rest lon)) ...)]))

;; LoN -> LoN
;; Subtract one to every element of a given list.
(check-expect (lon-sub1-all '()) '())
(check-expect (lon-sub1-all (list 7 1 4)) (list 6 0 3))
(define (lon-sub1-all lon)
  (cond [(empty? lon) '()]
        [(cons? lon)
         (... (first lon)
              (lon-sub1-all (rest lon)) ...)]))

;; LoN -> LoN
;; Compute the log (base e) of every element of a given list
(check-expect (lon-log-all '()) '())
(check-within (lon-log-all (list 7 1 4)) (list (log 7) (log 1) (log 4)) 0.0001)
(define (lon-log-all lon)
  (cond [(empty? lon) ...]
        [(cons? lon)
         (... (first lon)
              (lon-log-all (rest lon)) ...)]))
)

@ex["Code"]{

Complete the definitions of all the functions above.

}

@ex["Spot the difference"]{

For each function, identify the most closely related function from the
given solution for @secref{ex5}.  Once identified, apply the
abstraction process and define a function that encapsulates the
similarities between the two.  Redefine both functions in terms of
your abstraction and verify that the tests still pass.

}

@section[#:tag "lab10:using"]{Painting with abstractions}

Here are some more challenging functions:

@#reader scribble/comment-reader (racketblock
;; LoN -> Number
;; Compute the sum of the squares of the elements
(check-expect (lon-sum-sqrs '()) 0)
(check-expect (lon-sum-sqrs (list 8 18 10)) (+ (sqr 8) (sqr 18) (sqr 10)))
(define (lon-sum-sqrs lon) ...)

;; LoN -> LoN
;; Produce a list that contains only the elements larger than 10.
(check-expect (lon-filter->10 '()) '())
(check-expect (lon-filter->10 (list 8 18 10 12)) (list 18 12))
(define (lon-filter->10 lon) ...)

;; LoN Number -> LoN
;; Are all the numbers in list greater than n?
(check-expect (lon-all->? '() 7) #true)
(check-expect (lon-all->? (list 8 10 12) 7) #true)
(check-expect (lon-all->? (list 7 10 12) 7) #false)
(define (lon-all->? lon n) ...)

;; LoN Number -> LoN
;; Compute the GCD of every number in list and given number
(check-expect (lon-gcd '() 30) '())
(check-expect (lon-gcd (list 7 5 25) 30) (list (gcd 7 30) (gcd 5 25) (gcd 25 30)))
(define (lon-gcd lon n) ...)
)

@ex["Identifying abstractions"]{

For each function, identify which of your abstraction functions should
be applicable to define the function.

}

@ex["Using abstractions with locally defined functions"]{

For each function, complete the function definition by locally
defining functions as needed and using the abstraction function you
identified.

}

