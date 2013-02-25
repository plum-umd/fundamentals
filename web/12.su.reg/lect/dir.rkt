;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname dir) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Problem: figure out which direction to go to 
;; head toward a given point.  Directions include
;; E, NE, N, etc:

;; choose-dir : Posn Posn -> String
;; Choose name of direction that moves p1 toward p2.

;; My solution: represent directions with unit vectors,
;; move in each direction and pick the one that minimizes
;; the distance.

;; Advantages: inuitive, one-liner, works for arbitrary directions.

;; A Direction is a
;; (make-dir String Posn) where posn is unit vector.
(define-struct dir (string vect))

(define 1/√2 (/ 1 (sqrt 2)))

(define dirs
  (list (make-dir "E"  (make-posn 1 0))
        (make-dir "NE" (make-posn 1/√2 1/√2))
        (make-dir "N"  (make-posn 0 1))
        (make-dir "NW" (make-posn (- 1/√2) 1/√2))
        (make-dir "W"  (make-posn -1 0))
        (make-dir "SW" (make-posn (- 1/√2) (- 1/√2)))
        (make-dir "S"  (make-posn 0 -1))
        (make-dir "SE" (make-posn 1/√2 (- 1/√2)))))

;; dist : Posn Posn -> Number
;; Euclidean distance.
(define (dist p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))

;; move-by : Posn Posn -> Posn
;; Move posn by given vector.
(define (move-by p v)
  (make-posn (+ (posn-x p) (posn-x v))
             (+ (posn-y p) (posn-y v))))

;; choose-dir : Posn Posn -> String
;; Choose best direction that moves p1 toward p2.
(define (choose-dir p1 p2)
  (dir-string (argmin (λ (d) (dist (move-by p1 (dir-vect d)) p2)) dirs)))

