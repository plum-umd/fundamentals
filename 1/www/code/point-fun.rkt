;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname point-fun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;lang bsl

;; A Point is a (make-point Real Real)
(define-struct point (x y))

;; dist0 : Point -> Real
;; Compute the distance to origin from given point
(check-expect (dist0 (make-point 3 4)) 5)
(define (dist0 p)
  (sqrt (+ (sqr (point-x p))
           (sqr (point-y p)))))

;; dist : Point Point -> Real
;; Compute the distance between given points
(check-expect (dist (make-point 4 5) (make-point 1 1)) 5)
(define (dist p1 p2)
  (sqrt (+ (sqr (- (point-x p1) (point-x p2)))
           (sqr (- (point-y p1) (point-x p2))))))

;; move : Point Real Real -> Point
;; Move the point by the given amount
(check-expect (move (make-point 2 3) -1 2) (make-point 1 5))
(define (move p Δx Δy)
  (make-point (+ (point-x p) Δx) (+ (point-y p) Δy)))

;; point=? : Point Point -> Boolean
;; Are the two points the same?
(check-expect (point=? (make-point 3 4) (make-point 3 4)) #true)
(check-expect (point=? (make-point 3 4) (make-point 0 0)) #false)
(define (point=? p1 p2)
  (and (= (point-x p1) (point-x p2))
       (= (point-y p1) (point-y p2))))
