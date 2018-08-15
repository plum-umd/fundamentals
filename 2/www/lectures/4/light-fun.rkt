;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname light-fun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A Light is one of:
;; - "Red"
;; - "Green"
;; - "Yellow"

;; light-function : Light -> ???
(define (light-function l)
  (cond [(string=? "Red" l) ...]
        [(string=? "Green" l) ...]
        [(string=? "Yellow" l) ...]))

;; next : Light -> Light
;; Next light after the given light
(check-expect (next "Green") "Yellow")
(check-expect (next "Red") "Green")
(check-expect (next "Yellow") "Red")
(define (next l)
  (cond [(string=? "Red" l) "Green"]
        [(string=? "Green" l) "Yellow"]
        [(string=? "Yellow" l) "Red"]))


