#lang racket
(require rackunit)

;; An Object is [Message -> Method]
;; A Method is [Fields Anything ... -> Anything]
;; A Message is a String
;; A Prototype is Object or False
;; A Superclass is Class or False
;; A Class is [Anything ... -> Object]

;; object% is a Class
(define (object%)
  (make-object (hash) (hash) false))

;; lookup : [Object or False] Message -> Method
(define (lookup obj msg)
  (cond [(false? obj)
         (error "no such method")]
        [(hash-has-key? (object-methods obj) msg)
         (get (object-methods obj) msg)]
        [else
         (lookup (object-super obj) msg)]))

;; An Object is (make-object Fields Methods [Object or False])
;; A Methods is [Message -> Method]
(define-struct object (fields methods super))

;; A Fields is a [HashTable String Anything]
;; A FieldsName is a String
;; get : Fields FieldName -> Anything
(define (get flds fldname)
  (hash-ref flds fldname))

;; rect% is a Class
;; rect-mixin% is [Class -> Class] 
(define (rect-mixin% object%)
  (define meths
    (hash "w" (λ (fields) (get fields "w"))
          "h" (λ (fields) (get fields "h"))
          "area"
          (λ (fields) 
            (* (get fields "h")
               (get fields "w")))))
  ;; rect% is a Class  
  (define (rect% w h)
    (define super (object%))
    (define fields (hash "w" w "h" h))     
    (make-object fields meths super))
  rect%)

;; simple-rect% is a Class -- [Number Number -> Object]
(define simple-rect% (rect-mixin% object%))

(define (window%)
  (define super (object%))
  (define methods
    (hash "open" (λ (fields)
                   "the window is now open")
          "change"
          (λ (fields new) 
            (string-append "the window is now " new))))
  (make-object (hash) methods super))

(define rect-window% (rect-mixin% window%))
(define rw (rect-window% 17 4))  

;; send : Object Message Arguments ... -> Anything
(define (send o method . args)
  (apply (lookup o method)
         (object-fields o)
         args))

(check-equal? (send (simple-rect% 5 10) "w") 5)
(check-equal? (send (simple-rect% 5 10) "h") 10)
(check-equal? (send rw "w") 17)
(check-equal? (send rw "open") "the window is now open")
(check-equal? (send rw "change" "open") "the window is now open")
(check-equal? (send rw "change" "closed") "the window is now closed")
