;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname distance) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Simple calculations of Euclidean distance

; (3,4) = 5

;5
;(sqrt (+ (* 3 3) (* 4 4)))

; (7,8)
;(sqrt (+ (* 7 7) (* 8 8)))

; f(x,y) = sqrt(x^2 + y^2)
; f(3,4) = sqrt(3^2 + 4^2) = sqrt(9 + 16) = sqrt(25) = 5

(define (distance x y) (sqrt (+ (sqr x) (sqr y))))

(distance (distance 3 4) 5)




