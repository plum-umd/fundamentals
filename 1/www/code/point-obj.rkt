#lang class/0

;; A Point is a (new point Real Real)
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
  (define (dist fred)
    (sqrt (+ (sqr (- (send this x) (send fred x)))
             (sqr (- (send this y) (send fred y))))))

  ;; move : Real Real -> Point
  ;; Move this point by the given amount
  (check-expect (send (new point 2 3) move -1 2) (new point 1 5))
  ;; Java notation: new Point(2, 3).move(-1, 2)     new Point(1, 5)  
  (define (move Δx Δy)
    (new point (+ (send this x) Δx) (+ (send this y) Δy)))

  ;; point=? : Point Point -> Boolean
  ;; Is this point the same as that point?
  (check-expect (send (new point 3 4) point=? (new point 3 4)) #true)
  (check-expect (send (new point 3 4) point=? (new point 0 0)) #false)
  (define (point=? that)
    (and (= (send this x) (send that x))
         (= (send this y) (send that y)))))
