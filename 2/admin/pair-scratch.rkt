#lang racket
(define pairs1
  '((acalo joejeno)
    (kevin08 kmiao)
    (sripley7 ajacks)
    (merylas bjliuy)
    (michng goodwin)
    (guom dsimond)
    (jcaron jwall)
    (deema psanshi)
    (schwarta gabriel)
    (jbrooks2 ascherj)
    (bmccabe4 jkosof)
    (cungtran gudosha)
    (yusuf.m nnamdi93)
    (jinwow gudosha)))

#;
(for-each
 (lambda (p)
   (make-directory (format "~a" (first p)))
   (make-directory (format "~a" (second p))))
 pairs1)


(define (print u)
  (printf "[cs2510summer2012:/~a]~n" u)
  (printf "~a = rw~n~n" u))
(for-each 
 (lambda (p) 
   (print (first p))
   (print (second p)))
 pairs1)
   
   


(define i 1)
(for-each
 (lambda (p)
   (printf "pair~a: ~a ~a~n"
           (i->str i)
           (first p)
           (second p))
   (set! i (add1 i)))
 pairs1)



(for-each
 (lambda (p) 
   (make-directory (format "pair~a"
                           (i->str i)))
   (set! i (add1 i)))
 pairs1)

(define (i->str i)
  (regexp-replace* " " (format "~2F" i) "0"))

(for-each
 (lambda (p)
   (printf "[cs2510summer2012:/pair~a]~n~a = rw~n~a = rw~n~n"
           (i->str i)
           (first p)
           (second p))
   (set! i (add1 i)))
 pairs1)