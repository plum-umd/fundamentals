;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lon) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;    +---------------------------------+
;;    |                                 |
;;    v                                 |
;; A LoN (list of numbers) is one of:   |
;; - '()                                |
;; - (cons Number LoN)                  |
;;                 |                    |
;;                 +--------------------+

(define (lon-template lon)
  (cond [(empty? lon) ...]
        [(cons? lon)
         (... (first lon)
              ...
              (lon-template (rest lon))
              ...)]))

#|
cons   make-cons
first  cons-first
rest   cons-rest
cons?  cons?
|#

;; count : LoN -> Natural
;; Compute how many numbers there are in the list.
(check-expect (count '()) 0)
(check-expect (count (cons 8 (cons 3 (cons 1 '())))) 3)
(define (count lon)
  (cond [(empty? lon) 0]
        [(cons? lon)
         (add1 (count (rest lon)))]))

;; contains-five? : LoN -> Boolean
;; Are any of the numbers 5?
(check-expect (contains-five? '()) #false)
(check-expect (contains-five? (cons 3 (cons 4 '()))) #false)
(check-expect (contains-five? (cons 3 (cons 5 '()))) #true)
(define (contains-five? lon)
  (cond [(empty? lon) #false]
        [(cons? lon)
         (or (= (first lon) 5)
             (contains-five? (rest lon)))]))

;; sum : LoN -> Number
;; Compute the sum of all given numbers.
(check-expect (sum '()) 0)
(check-expect (sum (cons 3 (cons 4 (cons 2 '())))) 9)
(define (sum lon)
  (cond [(empty? lon) 0]
        [(cons? lon)
         (+ (first lon)         
            (sum (rest lon)))]))

;; prod : LoN -> Number
;; Compute the product of all given numbers.
(check-expect (prod '()) 1)
(check-expect (prod (cons 3 (cons 4 (cons 2 '())))) 24)
(define (prod lon)
  (cond [(empty? lon) 1]
        [(cons? lon)
         (* (first lon)
            (prod (rest lon)))]))

;; add1-all : LoN -> LoN
;; Compute list which is one more than every number in given list.
(check-expect (add1-all '()) '())
(check-expect (add1-all (cons 3 (cons 4 (cons 5 '()))))
              (cons 4 (cons 5 (cons 6 '()))))
(define (add1-all lon)
  (cond [(empty? lon) '()]
        [(cons? lon)
         (cons (add1 (first lon))
               (add1-all (rest lon)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;    +---------------------------------+
;;    |                                 |
;;    v                                 |
;; A LoS (list of strings) is one of:   |
;; - '()                                |
;; - (cons String LoS)                  |
;;                 |                    |
;;                 +--------------------+

(define (los-template los)
  (cond [(empty? los) ...]
        [(cons? los)
         (... (first los)
              (los-template (rest los)) ...)]))
                           
;; contains-fred? : LoS -> Boolean
;; Are any of the strings "Fred"?
(check-expect (contains-fred? '()) #f)
(check-expect (contains-fred? (cons "Fred" '())) #t)
(check-expect (contains-fred? (cons "Wilma" '())) #f)
(check-expect (contains-fred? (cons "Wilma" (cons "Fred" '()))) #t)
(check-expect (contains-fred? (cons "Wilma" (cons "Wilma" '()))) #f)
(define (contains-fred? los)
  (cond [(empty? los) #f]
        [(cons? los)
         (or (string=? "Fred" (first los))
             (contains-fred? (rest los)))]))

;; Step through:
; (contains-fred? (cons "Wilma" (cons "Fred" '())))

;; count-length : LoS -> Natural
;; Compute the total length of all strings in the list.
(check-expect (count-length '()) 0)
(check-expect (count-length (cons "Fred" '()))
              (+ 0 (string-length "Fred")))
(check-expect (count-length (cons "Fred" (cons "Wilma" '())))
              (+ 0 (string-length "Fred") (string-length "Wilma")))
(define (count-length los)
  (cond [(empty? los) 0]
        [(cons? los)
         (+ (string-length (first los))
            (count-length (rest los)))]))

;; remove-all-freds : LoS -> LoS
;; Remove all occurrences of "Fred" in given list of strings
(check-expect (remove-all-freds '()) '())
(check-expect (remove-all-freds (cons "Fred" '())) '())
(check-expect (remove-all-freds (cons "Wilma" '())) (cons "Wilma" '()))
(check-expect (remove-all-freds (cons "Wilma" (cons "Fred" (cons "Fred" '()))))
              (cons "Wilma" '()))
(define (remove-all-freds los)
  (cond [(empty? los) '()]
        [(cons? los)
         (cond [(string=? "Fred" (first los))
                (remove-all-freds (rest los))]
               [else
                (cons (first los)
                      (remove-all-freds (rest los)))])]))

;; remove-first-fred : LoS -> LoS
;; Remove first occurrence of "Fred", if there is one
#;(check-expect (remove-first-fred '()) ____)
#;(check-expect (remove-first-fred (cons "Fred" '())) ____)
#;(check-expect (remove-first-fred (cons "Wilma" '())) ____)
#;(check-expect (remove-first-fred (cons "Wilma" (cons "Fred" (cons "Fred" '()))))
                ____)
(define (remove-first-fred los) '())

;; string-lengths : LoS -> LoN
;; Compute the length of each string and collect results in a list
(define (string-lengths los) '())

;; remove-empty-strings : LoS -> LoS
;; Remove all occurrences of the empty string in given list
(define (remove-empty-strings los) '())

;; remove-long-strings : LoS Natural -> LoS
;; Remove all strings longer than n characters in given list
(define (remove-long-strings los n) '())

;; keep-long-strings : LoS Natural -> LoS
;; Keep only strings longer than n characters in given list
(define (keep-long-strings los n) '())

;; count-long-strings : LoS Natural -> Natural
;; Count the number of strings longer than n characters in given list
(define (count-long-strings los n) 0)

;; count-string=? : LoS String -> Natural
;; Count the number of times the given string occurs in the list
(define (count-string=? los s) 0)

;; remove-string=? : LoS String -> LoS
;; Remove all the strings that are equal to the given one from the list
(define (remove-string=? los s) '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;    +--------------------------------------------+
;;    |                                            |
;;    v                                            |
;; A LoLoN (list of list of numbers) is one of:    |
;; - '()                                           |
;; - (cons LoN LoLoN)                              |
;;          |    |                                 |
;;   +------+    +---------------------------------+
;;   |
;;   | +---------------------------------+
;;   | |                                 |
;;   v v                                 |
;; A LoN (list of numbers) is one of:    |
;; - '()                                 |
;; - (cons Number LoN)                   |
;;                 |                     |
;;                 +---------------------+

(define (lolon-template lln)
  (cond [(empty? lln) ...]
        [(cons? lln)
         (... (lon-template (first lln))
              ...
              (lolon-template (rest lln))
              ...)]))
  

;; sum-lists : LoLoN -> Number
;; Sum all of the numbers in all of the lists.
(check-expect (sum-lists '()) 0)
(check-expect (sum-lists (cons (cons 3 (cons 4 '()))
                               (cons (cons 5 '()) '())))
              12)

(define (sum-lists lln)
  (cond [(empty? lln) 0]
        [(cons? lln)
         (+ (sum (first lln))
            (sum-lists (rest lln)))]))

;; sum-each-list : LoLoN -> LoN
;; Compute a list of sums for each list of numbers.
(define (sum-each-list lln) '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Directory is a (make-dir String DirContent)
;; Interp: a directory has a name and a list of files or directories
(define-struct dir (name content))

;; A DirContent is one of:
;; - '()
;; - (cons FileOrDir DirContent)

;; A FileOrDir is one of:
;; - String
;; - Directory

;; dir-contains-file-fred.txt? : Directory -> Boolean
;; Does the directory (or any sub-directory) contain a file called "fred.txt"?
(define (dir-contains-file-fred.txt? dir) #false)

;; count-files : Directory -> Natural
;; Count the number of files in the directory and all sub-directories.
(define (count-files dir) 0)


;; This is a challenge problem if you're feeling really confident with
;; the above material
#|
;; list-paths : Directory -> LoS
;; List all paths to files and directory names in the directory (and sub-dirs).

(define HD
  (make-dir "home"
            (cons "notes.txt"
                  (cons (make-dir "131A"
                                  (cons "1.rkt" '()))
                        (cons (make-dir "132"
                                        (cons "1.java"
                                              (cons "2.java" '())))
                              '())))))

(check-expect (list-paths (make-dir "home" '())) (cons "home/"))
(check-expect (list-paths HD)
              (cons "home/"
                    (cons "home/notes.rkt"
                          (cons "home/131A/"
                                (cons "home/131A/1.rkt"
                                      (cons "home/132/"
                                            (cons "home/132/1.java"
                                                  (cons "home/132/2.java"
                                                        '()))))))))


|#


