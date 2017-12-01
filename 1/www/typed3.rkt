#lang typed/racket

#;(: always-2 : Number -> Number)
#;(: always-2 : Number -> Real)
#;(: always-2 : Real -> Number)
#;(: always-2 : Real -> Real)
(define (always-2 n)
  2)


;; Which of the following are accepted by the type checker?
#|
(always-2 18)
(always-2 3+4i)
(always-2 1/2)

(max 24 (always-2 18))
(max 24 (always-2 3+4i))
(max 24 (always-2 1/2))
|#

#|
some-function : T -> T1
some-argument : T0
T0 <: T

(some-function some-argument) : T1

T <: T
Real <: Number

T1 >: T3
T2 <: T4
==>
(T1 -> T2) <: (T3 -> T4)
|#

; (Number -> Real)
(: apply-to-5 : (Number -> Number) -> Number)
(define (apply-to-5 f)
  (f 0+5i))


(apply-to-5 always-2)


















