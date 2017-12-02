#lang scribble/manual
@(require racket/sandbox
          scribble/example)

@title[#:tag "m2-drills"]{Midterm 2 Drills}

Here are some drill problems. (Solutions: @link["drill-soln.rkt"]{drill-soln.rkt}.)

Keep in mind that any topic from midterm 1 could show up again on
midterm 2 so it may be worth revisiting older drill problems and the
practice midterm 1.

@section{Programming with functions}

Design and abstraction called @tt{andf} from the following two functions:

@#reader scribble/comment-reader (racketblock
;; even-pos? : Number -> Boolean
;; Is the number even and positive?
(check-expect (even-pos? 4) #true)
(check-expect (even-pos? 3) #false)
(check-expect (even-pos? -1) #false)
(define (even-pos? n)
  (and (even? n) (positive? n)))

;; neg-odd? : Number -> Boolean
;; Is the number negative and odd?
(check-expect (neg-odd? 4) #false)
(check-expect (neg-odd? 3) #false)
(check-expect (neg-odd? -1) #true)
(define (neg-odd? n)
  (and (negative? n) (odd? n)))
)

Be sure to give @tt{andf} the most general parametric signature that is valid.

Use your abstraction to define the following function (you may find
@tt{string-alphabetic?} and @tt{string-lower-case?} helpful):

@#reader scribble/comment-reader (racketblock
;; string-lower-alpha? : String -> Boolean
;; Is the string made up of lower case, alphabetic letters?
(check-expect (string-lower-alpha? "ABC") #false)
(check-expect (string-lower-alpha? "a2c") #false)
(check-expect (string-lower-alpha? "abc") #true)
(define (string-lower-alpha? s) ...)
)

Using only @tt{andf}, @tt{>}, @tt{even?}, and @tt{lambda} expressions, write an expression that
produces a predicate on numbers that will produce true when applied to even numbers greater than 5.

@section{Signatures}

Provide the most general valid signature for the following functions.

@#reader scribble/comment-reader (racketblock
(define (lengths xs)
  (map length xs))

(define (total-length xs)
  (foldr (λ (x t) (+ (length x) t)) 0 xs))

(define (map-f-zero lof)
  (map (λ (f) (f 0)) lof))
)

@section{Using list abstractions}

Re-define the following functions in terms of list abstraction
functions where appropriate.  (Signatures and purpose statements
intentionally omitted):

@#reader scribble/comment-reader (racketblock
(check-expect (rev-append (list "there" "hi")) "hithere")
(define (rev-append los)
  (cond [(empty? los) ""]
        [(cons? los)
         (string-append (rev-append (rest los)) (first los))]))

(check-expect (posns-at-x (list 1 2 3) 7)
              (list (make-posn 7 1) (make-posn 7 2) (make-posn 7 3)))
(define (posns-at-x ys x)
  (cond [(empty? ys) '()]
        [(cons? ys)
         (cons (make-posn x (first ys))
               (posns-at-x (rest ys) x))]))

(check-expect (dist (make-posn 0 0) (make-posn 3 4)) 5)
(define (dist p q)
  (sqrt (+ (sqr (- (posn-x p)
                   (posn-x q)))
           (sqr (- (posn-y p)
                   (posn-y q))))))

(check-expect (close-by (make-posn 0 0) (list (make-posn 3 4) (make-posn 8 8)) 6)
              (list (make-posn 3 4)))
(define (close-by p lop d)
  (cond [(empty? lop) '()]
        [(cons? lop)
         (cond [(<= (dist p (first lop)) d)
                (cons (first lop)
                      (close-by p (rest lop) d))]
               [else
                (close-by p (rest lop) d)])]))

(check-expect (draw-on (list (make-posn 50 50)))
              (place-image (circle 10 "solid" "red") 50 50 (empty-scene 100 100)))
(define (draw-on lop)
  (cond [(empty? lop) (empty-scene 100 100)]
        [(cons? lop)
         (place-image (circle 10 "solid" "red")
                      (posn-x (first lop))
                      (posn-y (first lop))
                      (draw-on (rest lop)))]))
)



@section[#:tag "design2"]{Designing functions}

Design a function that computes the ``dot product'' of two equal
length lists of numbers.  The dot product is the sum of the product of
each corresponding elements in the lists.  For example, the dot
product of @tt{(list 1 2 3)} and @tt{(list 4 5 6)} is @tt{(+ (* 1 4) (*
2 5) (* 3 6))}.

Design a function that computes the ``outer product'' of two lists of numbers.
The outerproduct of @tt{(list 1 2 3)} and @tt{(list 4 5 6 7)} is:
@#reader scribble/comment-reader (racketblock
(list (list (* 1 4) (* 1 5) (* 1 6) (* 1 7))
      (list (* 2 4) (* 2 5) (* 2 6) (* 2 7))
      (list (* 3 4) (* 3 5) (* 3 6) (* 3 7)))
)

Notice that the outer product of a list of length @tt{N} and of length
@tt{M} is a list of @tt{N} lists of @tt{M} numbers, with the exception
that if @tt{M} or @tt{N} is 0, the result is an empty list.

Design a similar function the computes something like the outer product, but for strings.
The outer string product of @tt{(list "a" "b" "c")} and @tt{(list "1" "2" "3" "4")} is:
@#reader scribble/comment-reader (racketblock
(list (list "a1" "a2" "a3" "a4")
      (list "b1" "b2" "b3" "b4")
      (list "c1" "c2" "c3" "c4"))
)

Design an abstraction function for ``outer-product-like" computations
of any kind.  Redefine your two outerproduct functions in terms of it.

Design a function @tt{append-to-each} that consumes a string @tt{s}
and a list of string @tt{los} and produces a list of strings like
@tt{los} but with @tt{s} appended to the front of each element.

Design a function @tt{append-all} that consumes two list of strings @tt{los1}
and @tt{los2} and produces a list of strings that appends each element of @tt{los1} to
all of the elements in @tt{los2}.  For example, @tt{(append-all (list "a" "b") (list "c" "d"))}
should produce @tt{(list "ac" "ad" "bc" "bd")}.

Design a function @tt{brute-n} that consumes a list of @tt{1String}s
@tt{los} and a natural number @tt{n} and produces a list of all strings
of length @tt{n} that can be formed from the given letters.  For example,
@tt{(brute-n (list "a" "b" "c") 2)} produces:
@#reader scribble/comment-reader (racketblock
(list "aa" "ab" "ac" "ba" "bb" "bc" "ca" "cb" "cc")
)

Design a function @tt{brute} that consumes a list of @tt{1String}s and
a natural number, and produces a list of all strings of length less
than @tt{n} that can be formed from the given letters.
For example, @tt{(brute (list "a" "b" "c") 2)} produces:
@#reader scribble/comment-reader (racketblock
(list "" "a" "b" "c" "aa" "ab" "ac" "ba" "bb" "bc" "ca" "cb" "cc")
)
