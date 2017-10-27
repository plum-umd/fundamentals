;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname adventure) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Choose your own adventure

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

(define-struct choice (q yes no))

(define (adventure-template adv)
  (cond [(string? adv) ...]
        [(choice? adv)
         (... (choice-q adv)
              (adventure-template (choice-yes adv))
              (adventure-template (choice-no adv))
              ...)]))

(define CMSC131A
  (make-choice
   "Do you take CMSC131A?"
   (make-choice
    "Do you read the book?"
    (make-choice
     "Do you do the labs?"
     (make-choice
      "Do you do the assignments?"
      "A fulfilling life of creative endeavors awaits you."
      "You should.")
     "You're making things harder than they need to be")
    (make-choice
     "Do you read other books?"
     (make-choice
      "Do you read PKD?"
      "Niiiiice"
      "You really should")
     "Life is hard... and meaningless"))
   "Life is easy, but meaningless"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 2htdp/image)
(require 2htdp/universe)

;; main : Adventure -> Adventure
(define (main adv)
  (big-bang adv
            [to-draw adv-draw]
            [on-key adv-key]))

;; adv-draw : Adventure -> Image
(define (adv-draw adv)
  (overlay (text (cond [(string? adv) adv]
                       [(choice? adv) (choice-q adv)])
                 30
                 "black")
           (empty-scene 800 100)))

;; adv-key : Adventure KeyEvent -> Adventure
(define (adv-key adv ke)
  (cond [(string? adv) adv]
        [(choice? adv)
         (cond [(string=? ke "y")
                (choice-yes adv)]
               [(string=? ke "n")
                (choice-no adv)]
               [else adv])]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; count-choices : Adventure -> Number
;; Count the number of choices in an adventure
(check-expect (count-choices "You lose!") 0)
(check-expect (count-choices (make-choice "Q?" "E1" "E2")) 1)
(check-expect (count-choices CMSC131A) 6)
(define (count-choices adv)
  (cond [(string? adv) 0]
        [(choice? adv)
         (+ 1
            (count-choices (choice-yes adv))
            (count-choices (choice-no adv)))]))

;; count-stories : Adventure -> Number
;; Count the number of stories in an adventure
(check-expect (count-stories "You lose!") 1)
(check-expect (count-stories (make-choice "Q?" "E1" "E2")) 2)
(check-expect (count-stories CMSC131A) 7)
(define (count-stories adv)
  (cond [(string? adv) 1]
        [(choice? adv)
         (+ (count-stories (choice-yes adv))
            (count-stories (choice-no adv)))]))

;; yesss-ending : Adventure -> String
;; Get the final outcome of always saying yes
(check-expect (yesss-ending "You lose!") "You lose!")
(check-expect (yesss-ending CMSC131A)
              "A fulfilling life of creative endeavors awaits you.")
(define (yesss-ending adv)
  (cond [(string? adv) adv]
        [(choice? adv)
         (yesss-ending (choice-yes adv))]))

#|
;; yesss-story : Adventure -> [Listof String]
;; Compute the story of always saying yes
(check-expect (yesss-story "You lose!") (list "You lose!"))
(define (yesss-story adv) (list "You lose!"))

;; max-length : Adventure -> Number
;; Compute max number of choices that could be made in adv
(check-expect (max-length "You lose!") 0)
(define (max-length adv) 0)

;; all-endings : Adventure -> [Listof String]
;; Compute a list of all endings
(check-expect (all-endings "You lose!") (list "You lose!"))
(define (all-endings adv) (list "You lose!"))

;; all-stories : Adventure -> [Listof [Listof String]]
;; Compute all possible stories w/ yes & no answers
(check-expect (all-stories "You lose!") (list (list "You lose!")))
(check-expect (all-stories (make-choice "Q?" "A" "B"))
              (list (list "Q?" "Yes" "A")
                    (list "Q?" "No" "B")))
(define (all-stories adv) '())


;; A Choices is a [Listof Boolean]
;; Interp: a list of Y/N choices to make in a CYOA book
;; Choices are listed in the order they are made
;; Yes = #true

|#



      