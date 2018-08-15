#lang class/0

;; A Point implements
;; - x : -> Real
;; - y : -> Real
;; - dist0 : -> Real
;; - dist : Point -> Real
;; - move : Real Real -> Point
;; - point=? : Point -> Boolean


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Point implementation based on complex numbers

;; A (new complex-point Number) implements Point
(define-class complex-point
  (fields c)

  ;; x : -> Real
  (define (x) (real-part (send this c)))
  ;; y : -> Real
  (define (y) (imag-part (send this c)))
  
  ;; dist0 : -> Real
  ;; Compute the distance to origin from this point
  (check-expect (send (new complex-point 3+4i) dist0) 5)
  (define (dist0)
    (sqrt (+ (sqr (send this x))
             (sqr (send this y)))))

  ;; dist : Point -> Real
  ;; Compute the distance between this point and that point
  (check-expect (send (new complex-point 4+5i) dist (new complex-point 1+1i)) 5)
  (define (dist that)
    (sqrt (+ (sqr (- (send this x) (send that x)))
             (sqr (- (send this y) (send that y))))))

  ;; move : Real Real -> Point
  ;; Move this point by the given amount
  (check-expect (send (new complex-point 2+3i) move -1 2)
                (new complex-point 1+5i))
  (define (move Δx Δy)
    (new complex-point
         (make-rectangular
          (+ (send this x) Δx)
          (+ (send this y) Δy))))

  ;; point=? : Point Point -> Boolean
  ;; Is this point the same as that point?
  (check-expect (send (new complex-point 3+4i) point=? (new complex-point 3+4i))
                #true)
  (check-expect (send (new complex-point 3+4i) point=? (new complex-point 0))
                #false)
  (define (point=? that)
    (and (= (send this x) (send that x))
         (= (send this y) (send that y)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Point implementation based on pair of real numbers

;; A (new point Real Real) implements Point
(define-class point
  (fields x y)
  ;; dist0 : -> Real
  ;; Compute the distance to origin from this point
  (check-expect (send (new point 3 4) dist0) 5)
  (define (dist0)
    (sqrt (+ (sqr (send this x))
             (sqr (send this y)))))

  ;; dist : Point -> Real
  ;; Compute the distance between this point and that point
  (check-expect (send (new point 4 5) dist (new point 1 1)) 5)
  (define (dist that)
    (sqrt (+ (sqr (- (send this x) (send that x)))
             (sqr (- (send this y) (send that y))))))

  ;; move : Real Real -> Point
  ;; Move this point by the given amount
  (check-expect (send (new point 2 3) move -1 2) (new point 1 5))
  (define (move Δx Δy)
    (new point (+ (send this x) Δx) (+ (send this y) Δy)))

  ;; point=? : Point Point -> Boolean
  ;; Is this point the same as that point?
  (check-expect (send (new point 3 4) point=? (new point 3 4)) #true)
  (check-expect (send (new point 3 4) point=? (new point 0 0)) #false)
  (define (point=? that)
    (and (= (send this x) (send that x))
         (= (send this y) (send that y)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define p1 (new point 3 4))
(define p2 (new complex-point 3+4i))

(send p1 point=? p2)
(send p2 point=? p1)















