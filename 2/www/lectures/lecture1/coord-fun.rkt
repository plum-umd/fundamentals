;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname coord-fun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A Coord is a (make-coord Real Real)
;; Interp: Cartesian coordinate
(define-struct coord (x y))

;; dist0 : Coord -> Real
;; Distance of given coordinate to origin
(check-expect (dist0 (make-coord 3 4)) 5)
(define (dist0 c) ...)

;; move : Coord Real Real -> Coord
;; Move given coordinate by offsets
(check-expect (move (make-coord 3 4) 2 -1) (make-coord 3 3))
(define (move c dx dy) ...)
