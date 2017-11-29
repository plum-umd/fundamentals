#lang typed/racket
;; Incantation to write unit tests in TR (Typed Racket):
(module+ test
  (require/typed rackunit
                 [check-equal? (Any Any -> Any)]))


;; An Even is Number that is even

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: qsort : [Listof Real] -> [Listof Real])
;; Sort the list in ascending order
(module+ test
  (check-equal? (qsort (list 11 8 14 7)) (list 7 8 11 14))
  (check-equal? (qsort (list 1 1 1 1)) (list 1 1 1 1)))
(define (qsort lon)
  (cond [(empty? lon) '()]
        [(cons? lon)
         (local [(define pivot (first lon))]
           (append (qsort (smaller (rest lon) pivot))
                   (list pivot)
                   (qsort (bigger (rest lon) pivot))))]))

(: smaller : ([Listof Real] Real -> [Listof Real]))
;; Produce a list of elements smaller than the given number
(module+ test
  (check-equal? (smaller (list 1 2 3) 2) (list 1)))
(define (smaller lon n)
  (filter (λ ([fred : Real]) (< fred n)) lon))

(: my-filter : (All (X) ((X -> Boolean) [Listof X] -> [Listof X])))
(define (my-filter p lox)
  (cond [(empty? lox) '()]
        [(cons? lox)
         (if (p (first lox))
             (cons (first lox)
                   (my-filter p (rest lox)))
             (my-filter p (rest lox)))]))

(: bigger : ([Listof Real] Real -> [Listof Real]))
;; Produce a list of elements bigger than or equal to the given number
(module+ test
  (check-equal? (bigger (list 1 2 3) 2) (list 2 3)))
(define (bigger lon n)
  (filter (λ ([barney : Real]) (>= barney n)) lon))


















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;       +----------------------------------------+
;;       |  +-----------------------------------+ |
;;       v  v                                   | |
;; An Adventure is one of:                      | |
;; - String                                     | |
;; - (make-choice String Adventure Adventure)   | |
;;                           |         |        | |
;;                           |         +--------+ |
;;                           +--------------------+

;; Interp: a choose-your-own-adventure book where a string
;; is a conclusion and a choice is a yes/no question and
;; the two adventures that follow as a consequnce of the answer.

(define-type Adventure
  (U String
     (choice String Adventure Adventure)))

(define-struct (A B C) choice ([q : A] [yes : B] [no : C]))

(define CMSC131A : Adventure
  (make-choice
   "Do you take CMSC131A?"
   (make-choice
    "Do you read the book?"
    (make-choice
     "Do you do the labs?"
     (make-choice
      "A string"
      "A fulfilling life of creative endeavors awaits you."
      "You should.")
     "You're making things harder than they need to")
    (make-choice
     "Do you read other books?"
     (make-choice
      "Do you read PKD?"
      "Niiiiice"
      "You really should")
     "Life is hard... and meaningless"))
   "Life is easy, but meaningless"))

(: count-choices : (Adventure -> Number))
;; Count the number of choices in an adventure
(module+ test
  (check-equal? (count-choices "You lose!") 0)
  (check-equal? (count-choices (make-choice "Q?" "E1" "E2")) 1)
  (check-equal? (count-choices CMSC131A) 6))
(define (count-choices adv)
  (cond [(string? adv) 0]
        [(choice? adv)
         (+ 1
            (count-choices (choice-yes adv))
            (count-choices (choice-no adv)))]))


(: count-stories (Adventure -> Number))
;; Count the number of stories in an adventure
(module+ test
  (check-equal? (count-stories "You lose!") 1)
  (check-equal? (count-stories (make-choice "Q?" "E1" "E2")) 2)
  (check-equal? (count-stories CMSC131A) 7))
(define (count-stories adv)
  (cond [(string? adv) 1]
        [(choice? adv)
         (+ (count-stories (choice-yes adv))
            (count-stories (choice-no adv)))]))

(: yesss-ending (Adventure -> String))
;; Get the final outcome of always saying yes
(module+ test
  (check-equal? (yesss-ending "You lose!") "You lose!")
  (check-equal? (yesss-ending CMSC131A)
                "A fulfilling life of creative endeavors awaits you."))
(define (yesss-ending adv)
  (cond [(string? adv) adv]
        [(choice? adv)
         (yesss-ending (choice-yes adv))]))


#|
;; yesss-story : Adventure -> [Listof String]
;; Compute the story of always saying yes
(check-expect (yesss-story "You lose!") (list "You lose!"))
(check-expect (yesss-story (make-choice "Q?" "A1" "A2"))
              (list "Q?" "A1"))
(define (yesss-story adv)
  (cond [(string? adv) (list adv)]
        [(choice? adv)
         (cons (choice-q adv)
               (yesss-story (choice-yes adv)))]))

;; max-length : Adventure -> Number
;; Compute max number of choices that could be made in adv
(check-expect (max-length "You lose!") 0)
(check-expect (max-length (make-choice "Q?" "A1" "A2")) 1)
(check-expect (max-length (make-choice "Q?"
                                       (make-choice "Q2?" "A1" "A2")
                                       "A3"))
              2)
(check-expect (max-length (make-choice "Q?"
                                       "A3"
                                       (make-choice "Q2?" "A1" "A2")))
              2)
(check-expect (max-length CMSC131A) 4)
(define (max-length adv)
  (cond [(string? adv) 0]
        [(choice? adv)
         (+ 1 (max (max-length (choice-yes adv))
                   (max-length (choice-no adv))))]))

;; all-endings : Adventure -> [Listof String]
;; Compute a list of all endings
(check-expect (all-endings "You lose!") (list "You lose!"))
(check-expect (all-endings (make-choice "Q?" "A1" "A2"))
              (list "A1" "A2"))
(check-expect (all-endings (make-choice "Q?"
                                        (make-choice "Q2?" "A1" "A2")
                                        "A3"))
              (list "A1" "A2" "A3"))

(define (all-endings adv)
  (cond [(string? adv) (list adv)]
        [(choice? adv)
         (append (all-endings (choice-yes adv))
                 (all-endings (choice-no adv)))]))
|#

(: all-stories (Adventure -> [Listof [Listof String]]))
;; Compute all possible stories w/ yes & no answers
(module+ test
  (check-equal? (all-stories "You lose!") (list (list "You lose!")))
  (check-equal? (all-stories (make-choice "Q?" "A" "B"))
                (list (list "Q?" "Yes" "A")
                      (list "Q?" "No" "B")))
  (check-equal? (all-stories "A") (list (list "A"))) 
  (check-equal? (all-stories "B") (list (list "B")))
  (check-equal? (all-stories (make-choice "Q?"
                                          (make-choice "Q2?" "A1" "A2")
                                          "A3"))
                (list (list "Q?" "Yes" "Q2?" "Yes" "A1")
                      (list "Q?" "Yes" "Q2?" "No" "A2")
                      (list "Q?" "No" "A3"))))
(define (all-stories adv)
  (cond [(string? adv) (list (list adv))]
        [(choice? adv)
         (append
          (map (λ ([s : [Listof String]]) (cons (choice-q adv) (cons "Yes" s)))
               (all-stories (choice-yes adv)))
          (map (λ ([s : [Listof String]]) (cons (choice-q adv) (cons "No" s)))
               (all-stories (choice-no adv))))]))
