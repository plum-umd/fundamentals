#lang class/2

;; A JSON is one of:
;; - (new s% String)
;; - Array
;; and implements
;; - Count the number of strings in this object
;;   count-strings : -> Natural
;; - Find the first string in this JSON Jr object satisfying
;;   given predicate, or #f if there’s no such string
;;   find : StringPred -> String or #f
;;
;; An Array is one of
;; - (new a0%)
;; - (new a+% JSON Array)

(define-class s%
  (fields val)
  (define (count-strings) 1)
  (define (find p)
    (if (p . apply (this . val))
        (this . val)
        #f)))

(define-class a0%
  (define (count-strings) 0)
  (define (find p) false))

(define-class a+%
  (fields hd tl)
  (define (count-strings)
    (+ (this . hd . count-strings)
       (this . tl . count-strings)))
  (define (find p)
    (local [(define r (this . hd . find p))]
      (if (false? r)
          (this . tl . find p)
          r))))

;; A (new eq% String) implements StringPred
(define-class eq%
  (fields x)
  (define (apply y)
    (string=? (this . x) y)))

(define eg1 (new s% "This is JSON"))
(define eg2 (new a+% eg1 (new a+% (new s% "Jr.") (new a0%))))
(define eg3 (new a+% (new a0%)
                 (new a+% (new a+% (new s% "So")
                               (new a+% (new s% "is") (new a0%)))
                      (new a+% (new s% "this")
                           (new a0%)))))

(check-expect (eg1 . count-strings) 1)
(check-expect (eg2 . count-strings) 2)
(check-expect (eg3 . count-strings) 3)

(check-expect (eg1 . find (new eq% "This is JSON")) "This is JSON")
(check-expect (eg1 . find (new eq% "this")) false)

(check-expect (eg2 . find (new eq% "This is JSON")) "This is JSON")
(check-expect (eg2 . find (new eq% "this")) false)

(check-expect (eg3 . find (new eq% "This is JSON")) false)
(check-expect (eg3 . find (new eq% "this")) "this")


