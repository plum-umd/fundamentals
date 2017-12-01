#lang typed/racket

;; Incantation to write unit tests in TR (Typed Racket):
(module+ test
  (require/typed rackunit
                 [check-equal? (Any Any -> Any)]))


;; In ISL:
#|
;; f : Number -> Number
(check-expect (f 4) 7)
(define (f x)
  (+ x 3))
|#

;; In TR:
(: f : (Number -> Number))
(module+ test
  (check-equal? (f 4) 7))
(define (f x)
  (+ x 3))


;; A "good" program that is thrown out by the type checker
#;#;
(: good : (Number -> Number))
(define (good x)
  (if (= x x)
      x
      (string-append x x)))



(: h : ((String -> Number) -> Number))
(define (h f)
  (f "8"))



;; A Posn is a (make-posn Number Number)
;; A StringPair is a (make-posn String String)

;; A [Pair X Y] is a (make-posn X Y)
;; A Posn is a [Pair Number Number]
;; A StringPair is a [Pair String String]

;(define-struct posn ([x : Number] [y : Number]))
;(define-struct string-pair ([x : String] [y : String]))

(define-struct (X Y) posn ([x : X] [y : Y]))

(define-type StringPair (posn String String))

(: append-pair : StringPair -> String)
(define (append-pair sp)
  (string-append (posn-x sp) (posn-y sp)))



;; my-map : [X Y] ((X -> Y) [Listof X] -> [Listof Y])
(: my-map : (All (X Y) ((X -> Y) [Listof X] -> [Listof Y])))
(define (my-map f xs)
  (cond [(empty? xs) '()]
        [(cons? xs)
         (cons (f (first xs))
               (my-map f (rest xs)))]))



