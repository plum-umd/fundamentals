;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname zip) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Session ID: 887722

;; A ListofInt is one of:
;; - '()
;; - (cons Integer ListofInt)

;; A ListofPairofInt is one of:
;; - '()
;; - (cons PairofInt ListofPairofInt)

;; A PairofInt is a (make-pair Integer Integer)
(define-struct pair (left right))


;; zip : ListofInt ListofInt -> ListofPairofInt
;; Zip together the given lists into a list of pairs
;; Stops zipping at end of the shortest list

(check-expect (zip (list 1 2 3) (list 4 5 6))
              (list (make-pair 1 4)
                    (make-pair 2 5)
                    (make-pair 3 6)))
(check-expect (zip (list 1 2) (list 4 5 6))
              (list (make-pair 1 4)
                    (make-pair 2 5)))
(check-expect (zip (list 1 2 3) (list 4 5))
              (list (make-pair 1 4)
                    (make-pair 2 5)))
#;
(define (zip ls1 ls2)
  (cond [(empty? ls1) '()]
        [(cons? ls1)
         (cond [(empty? ls2) '()]
               [(cons? ls2)                
                (cons (make-pair (first ls1) (first ls2))
                      (zip (rest ls1) (rest ls2)))])]))

(define (zip ls1 ls2)
  (cond [(empty? ls1) '()]
        [(cons? ls1)
         (zip-cons ls2 (first ls1) (rest ls1))]))

;; zip-cons : ListofInt Integer ListofInteger -> ListofPairofInteger
(define (zip-cons ls2 first-ls1 rest-ls1)
  (cond [(empty? ls2) '()]
        [(cons? ls2)                
         (cons (make-pair first-ls1 (first ls2))
               (zip rest-ls1 (rest ls2)))]))
        


