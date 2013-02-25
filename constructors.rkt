#lang class/0

;; A Blah implements
;; n : Nat
;; f : X ... -> Y
;; ...

;; The original class
(define-class blah%
  (fields n)
  (define/public (f x ...)
    ...f code...))

;; Now with x defaulting to 5.
(define-class blah/5%
  (define/public (n)
    (send (new blah% 5) n))
  (define/public (f x ...)
    (send (new blah% 5) f x ...)))

;; Constructors are overloaded by statically specifying class name,
;; rather than statically specifying by a string of argument types.
;; eg.
(new blah/5%)
(new blah% 8)

;; That has the nice effect of allowing multiple constructors with 
;; the same signature.

;; (The static restrictions is dropped once we get to first class classes
;; but the mechanism still works fine.)

;; This works for arbitrary constructors, not just defaults:

(define-class blah/est-invariant%
  (fields lax-n)
  (define/public (n)
    (send (new blah% (est-invariant (send this lax-n))) n))
  (define/public (f x ...)
    (send (new blah% (est-invariant (send this lax-n))) f x ...)))

;; A lot of interesting things happen if you allow first class classes:

;; Private constructors:
(define blah/est-invariant% 
  (local [(define-class blah%
            (fields n)
            (define/public (f x ...)  ;; uses blah% constructor
              ...f code...))]
    (class
      (fields lax-n)      
      (define/public (n)
        (send (new blah% (est-invariant (send this lax-n))) n))
      (define/public (f x ...)
        (send (new blah% (est-invariant (send this lax-n))) f x ...)))))