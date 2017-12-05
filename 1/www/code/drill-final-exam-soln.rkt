;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname drill-final-exam-soln) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; factorial : Natural -> Natural
;; Compute n!
(check-expect (factorial 5) 120)
(define (factorial n)
  (fact/a n 1)
  ;; Original:
  #;
  (cond [(zero? n) 1]
        [else
         (* n (factorial (sub1 n)))]))

;; Accumulator: factorial of numbers seen so far.
(define (fact/a n a)
  (cond [(zero? n) a]
        [else
         (fact/a (sub1 n) (* n a))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; largest : [NEListof Real] -> Real
;; Finds largest element in given non-empty list
(check-expect (largest (list 7)) 7)
(check-expect (largest (list 2 9 7)) 9)
(define (largest ns)
  ;; First element is the largest seen so far
  (largest/a (rest ns) (first ns))
  ;; Original:
  #;
  (cond [(empty? (rest ns)) (first ns)]
        [else
         (max (first xs)
              (largest (rest xs)))]))

;; largest/a : [Listof Real] Real -> Real
;; Find the largest number among elements in given list and n
(define (largest/a ns n)
  (cond [(empty? ns) n]
        [(cons? ns)
         (largest/a (rest ns) (max n (first ns)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; prod : [Listof Number] -> Number
;; Compute the product of the list
(check-expect (prod '()) 1)
(check-expect (prod (list 1 2 3 4 5)) 120)
(define (prod xs)
  ;; Using accumulator abstraction:
  (abs/a * 1 xs)
  ;; Using accumulator:
  #;
  (prod/a xs 1)
  ;; Original:
  #;
  (cond [(empty? xs) 1]
        [(cons? xs)
         (* (first xs)
            (prod (rest xs)))]))

;; prod/a : [Listof Number] Number -> Number
(define (prod/a xs a)
  (cond [(empty? xs) a]
        [(cons? xs)
         (sum/a (rest xs) (* (first xs) a))]))

;; sum : [Listof Number] -> Number
;; Compute the sum of the list
(check-expect (sum '()) 0)
(check-expect (sum (list 1 2 3 4 5)) 15)
(define (sum xs)
  ;; Using accumulator abstraction:
  (abs/a + 0 xs)
  ;; Using accumulator:
  #;
  (sum/a xs 0)
  ;; Original:
  #; 
  (cond [(empty? xs) 0]
        [(cons? xs)
         (+ (first xs)
            (sum (rest xs)))]))

;; sum/a : [Listof Number] Number -> Number
(define (sum/a xs a)
  (cond [(empty? xs) a]
        [(cons? xs)
         (sum/a (rest xs) (+ (first xs) a))]))

;; Abstraction of prod/a, suma:

;; abs/a : [X Y] [X Y -> Y] Y [Listof X] -> Y
(define (abs/a f a xs)
  (cond [(empty? xs) a]
        [(cons? xs)
         (abs/a f (f (first xs) a) (rest xs))]))

;; What does this remind you of?

;; It has the same signature as foldr!  In fact this is foldl.
;; While foldr is the fundamental abstraction of structural recursion on lists,
;; this is the fundamental abstraction of structural recursion with an
;; accumulator on lists.

(check-expect (largest.1 (list 7)) 7)
(check-expect (largest.1 (list 2 9 7)) 9)
(define (largest.1 xs)
  (abs/a max (first xs) (rest xs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; minf : [X] [X -> Real] [NEListof X] -> X
;; Find the earliest element that minimizes f.
(check-expect (minf sqr (list -4 2 8)) 2)
(check-expect (minf sqr (list -4 -2 8 2)) -2)
(define (minf f xs)
  ;; Initialize xn to first element, which is guaranteed to exist and
  ;; is the minimizing element seen so far.
  (minf/a f (rest xs) (first xs) (f (first xs)))

  ;; Original
  #;
  (cond [(empty? (rest xs)) (first xs)]
        [else
         (local [(define x1 (first xs))
                 (define y1 (f x1))
                 (define xn (minf f (rest xs)))
                 (define yn (f xn))]
           (if (<= y1 yn) x1 xn))]))

;; minf/a : [X] [X -> Real] [Listof X] X Real -> X
;; Accumulator: yn = (f xn) where xn is the earliest element
;; seen so far that minimizes f.
(define (minf/a f xs xn yn)
  (cond [(empty? xs) xn]
        [else
         (local [(define x1 (first xs))
                 (define y1 (f x1))]
           (if (<= yn y1)
               (minf/a f (rest xs) xn yn)
               (minf/a f (rest xs) x1 y1)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An AscString is a String
;; where (explode s) is sorted by string<?

;; asc-contains : AscString 1String -> Boolean
;; Does the string contain the given letter?
;; Generative eureka: look at letter in the middle and then either
;;   recur on the left or the right based on comparison
;; Termination: each recursive call is on a strictly shorter string
(check-expect (asc-contains? "" "a") #false)
(check-expect (asc-contains? "aabcz" "a") #true)
(check-expect (asc-contains? "aabcz" "z") #true)
(check-expect (asc-contains? "aabcz" "q") #false)
(define (asc-contains? s l)
  (cond [(string=? s "") #false]
        [else
         (local [(define middle (string-middle s))]
           (cond [(string=? middle l) #true]
                 [(string<? middle l) (asc-contains? (string-right s) l)]
                 [(string>? middle l) (asc-contains? (string-left s) l)]))]))


;; NEString -> 1String
;; Get middle letter of non-empty string
(check-expect (string-middle "a") "a")
(check-expect (string-middle "abcd") "c")
(define (string-middle s)
  (local [(define i (middle-i s))]
    (substring s i (add1 i))))

;; NEString -> String
;; Get string to the left of middle
(check-expect (string-left "a") "")
(check-expect (string-left "abcd") "ab")
(define (string-left s)
  (substring s 0 (middle-i s)))

;; NEString -> String
;; Get string to the right of middle
(check-expect (string-right "a") "")
(check-expect (string-right "abcd") "d")
(define (string-right s)
  (substring s (add1 (middle-i s))))

;; NEString -> Integer
;; Calculate index of middle (tie goes to the right)
(check-expect (middle-i "a") 0)
(check-expect (middle-i "ab") 1)
(check-expect (middle-i "abc") 1)
(define (middle-i s)
  (quotient (string-length s) 2))

                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A QT (Quad Tree) is one of:
;; - #false
;; - (make-quad Posn QT QT QT QT)
(define-struct quad (pt ne nw se sw))
;; Interp: a set of positions, #false represents an empty set
;; Invariant: 
;;  - all points in ne are north (y is smaller) & east (x is larger) of pt
;;  - all points in nw are north (y is smaller) & west (x is smaller) of pt
;;  - all points in se are south (y is larger) & east (x is larger) of pt
;;  - all points in sw are south (y is larger) & west (x is smaller) of pt

(define O (make-posn 0 0))
(define NE (make-posn 25 -4))
(define NW (make-posn -3 -6))
(define SE (make-posn 10 12))
(define SW (make-posn -9 41))
(define QT0
  (make-quad O
             (make-quad NE #f #f #f #f)
             (make-quad NW #f #f #f #f)
             (make-quad SE #f #f #f #f)
             (make-quad SW #f #f #f #f)))

;; contains? : Posn QT -> Boolean
;; Is the given position in the set?
(check-expect (contains? O #false) #false)
(check-expect (contains? O QT0) #true)
(check-expect (contains? NE QT0) #true)
(check-expect (contains? NW QT0) #true)
(check-expect (contains? SE QT0) #true)
(check-expect (contains? SW QT0) #true)
(check-expect (contains? (make-posn  1  1) QT0) #false)
(define (contains? p qt)
  (cond [(false? qt) #false]
        [(quad? qt)
         (local [(define q (quad-pt qt))]
           (cond [(posn=? p q) #true]
                 [(nw? p q) (contains? p (quad-nw qt))]
                 [(ne? p q) (contains? p (quad-ne qt))]
                 [(se? p q) (contains? p (quad-se qt))]
                 [(sw? p q) (contains? p (quad-sw qt))]))]))

;; Helpers:

;; posn=? : Posn Posn -> Boolean
;; Are the posn the same?
(check-expect (posn=? O O) #true)
(check-expect (posn=? O NW) #false)
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(define (d? c-x c-y)
  (λ (p q)
    (and (c-x (posn-x p) (posn-x q))
         (c-y (posn-y p) (posn-y q)))))

;; Is p to the {NW,NE,SE,SW} of q?
;; Posn Posn -> Boolean
(check-expect (nw? NW O) #true)
(check-expect (nw? O NW) #false)
(check-expect (ne? NE O) #true)
(check-expect (ne? O NE) #false)
(check-expect (sw? SW O) #true)
(check-expect (sw? O SW) #false)
(check-expect (se? SE O) #true)
(check-expect (se? O SE) #false)
(define nw? (d? <= <))
(define ne? (d? >  <))
(define se? (d? >  >=))
(define sw? (d? <= >=))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Graph is [Listof (list String String)]
;; Interp: a graph is a list of edges from one node to another

;; A Path is a [Listof String]
;; Interp: an ordered list of connected nodes in a graph

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

;; neighbors : Graph String -> [Listof String]
;; neighbors : Graph String -> [Listof String]
;; Compute the neighbors of the given node in the graph
;; Assume: the node exists in the graph
(check-expect (neighbors G "A") (list "B" "E"))
(check-expect (neighbors G "D") '())
(define (neighbors g n)
  (map second
       (filter (λ (x) (string=? (first x) n)) g)))

;; All of the code below is actually the same as for the other
;; representation.  Only `neighbors` changes!!

;;;;;; BEGIN SAME

;; path : Graph String String -> [Maybe Path]
;; Compute a path in the graph between the given nodes, if one exists
;; Produces #false if no path exists
(check-member-of (path G "A" "D")
                 (list "A" "B" "F" "D")
                 (list "A" "B" "E" "F" "D")
                 (list "A" "B" "E" "C" "D")
                 (list "A" "E" "C" "D")
                 (list "A" "E" "F" "D"))
(check-expect (path G "D" "D")
              (list "D"))
(check-expect (path G "E" "A") #false)

(define (path g src dst)
  (path/a g src dst '()))

;; path/a : Graph String String [Listof String] -> [Maybe Path]
(define (path/a g src dst seen)
  (cond [(string=? src dst) (list src)]
        [(member? src seen) #false] ;; have we tried this src before?
        [else
         (local [(define maybe-path
                   (any-path/a g (neighbors g src) dst (cons src seen)))]
           (cond [(false? maybe-path) #false]
                 [else (cons src maybe-path)]))]))

;; any-path : Graph [Listof String] String -> [Maybe Path]
;; Compute a path, if one exists, from any of the given sources
;; and list of seen srcs
(check-expect (any-path G '() "D") #false)
(check-expect (any-path G (list "F") "D")
              (list "F" "D"))
(check-expect (any-path G (list "F" "G") "D")
              (list "F" "D"))

(define (any-path g srcs dst)
  (any-path/a g srcs dst '()))

;; any-path/a : Graph [Listof String] String [Listof String] -> [Maybe Path]
(define (any-path/a g srcs dst seen)
  (cond [(empty? srcs) #false]
        [(cons? srcs)
         (local [(define maybe-path (path/a g (first srcs) dst seen))]
           (cond [(false? maybe-path)
                  (any-path/a g (rest srcs) dst seen)]
                 [else
                  maybe-path]))]))

;;;;;; END SAME






