;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname days) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define SECONDS/DAY (* 24 60 60))
(define NOW (current-seconds))

;; A Date is a Integer
;; Interp: the number of seconds since Jan 1, 1970

;; days : Date Date -> Integer
;; Compute the number of days between d1 and d2.
(check-expect (days 0 0) 0)
(check-expect (days 0 1) 0)
(check-expect (days 0 SECONDS/DAY) 1)
(check-expect (days SECONDS/DAY 0) -1)
(check-expect (days NOW (+ NOW SECONDS/DAY)) 1)
#;(define (days d1 d2) 0)

(define (days d1 d2)
  (quotient (- d2 d1) SECONDS/DAY))








