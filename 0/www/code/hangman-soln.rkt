;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hangman) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main

;; String -> Hangman
;; Play a game of hangman trying to guess s
(define (main s)
  (big-bang (make-hangman (string-hide s) '())
    [on-key hangman-key]
    [to-draw hangman-draw]
    [stop-when hangman-done? hangman-draw-done]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data definitions

;; A Hangman is a (make-hangman Word Lo1String)
(define-struct hangman (word misses))
;; Interpretation: word is the (partially) guessed word
;; misses is a list of incorrect guesses

;; A Word is one of:
;; - '()
;; - (cons Letter Word)
;; Interpretation: a word where some letters maybe hidden

#|
(define (word-template w)
  (cond [(empty? w) ...]
        [(cons? w)
         (... (first w) ; Letter
              (word-template (rest w))
              ...)]))
|#


;; A Letter is one of:
;; - (make-hidden 1String)
;; - 1String
;; Interpretation: (make-hidden s) means s has not been revealed,
;; a string s means s has been revealed
(define-struct hidden (sec))

;; A Lo1String is one of:
;; - '()
;; - (cons 1String Lo1String)

;; A 1String is a String of length 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants

(define SIZE 80) ; px size of square for each letter
(define SQ (square SIZE "outline" "black"))

;; Constants for testing
(define H_E_L_L_O_
  (cons (make-hidden "h")
        (cons (make-hidden "e")
              (cons (make-hidden "l")
                    (cons (make-hidden "l")
                          (cons (make-hidden "o") '()))))))

(define H_E_L*L*O_
  (cons (make-hidden "h")
        (cons (make-hidden "e")
              (cons "l"
                    (cons "l"
                          (cons (make-hidden "o") '()))))))
(define H0
  (make-hangman H_E_L_L_O_ '()))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hangman functions

;; Hangman KeyEvent -> Hangman
;; Handle alphabetic keys by guessing, ignore others
(check-expect (hangman-key H0 "a") (hangman-guess H0 "a"))
(check-expect (hangman-key H0 "left") H0)
(define (hangman-key hm ke)
  (cond [(1string-alphabetic? ke) (hangman-guess hm ke)]
        [else hm]))

;; Hangman -> Image
;; Draw game, showing each letter
(check-expect (hangman-draw H0) (word-draw H_E_L_L_O_))
(define (hangman-draw hm)
  (word-draw (hangman-word hm)))

;; Hangman 1String -> Hangman
;; Guess s in given game
(check-expect (hangman-guess (make-hangman H_E_L_L_O_ '()) "l")
              (make-hangman H_E_L*L*O_ '()))
(check-expect (hangman-guess (make-hangman H_E_L_L_O_ '()) "a")
              (make-hangman H_E_L_L_O_ (cons "a" '())))
(check-expect (hangman-guess (make-hangman H_E_L_L_O_ (cons "A" '())) "a")
              (make-hangman H_E_L_L_O_ (cons "A" '())))
(define (hangman-guess hm s)  
  (cond [(hangman-hiding? hm s)
         (make-hangman (word-reveal (hangman-word hm) s)
                       (hangman-misses hm))]
        [(hangman-already-missed? hm s) hm]        
        [else
         (make-hangman (hangman-word hm)
                       (cons s (hangman-misses hm)))]))

;; Hangman -> Boolean
;; Is s a hidden letter in the given game?
(check-expect (hangman-hiding? (make-hangman H_E_L*L*O_ '()) "h") #true)
(check-expect (hangman-hiding? (make-hangman H_E_L*L*O_ '()) "l") #false)
(check-expect (hangman-hiding? (make-hangman H_E_L*L*O_ '()) "a") #false)
(define (hangman-hiding? hm s)
  (word-hiding? (hangman-word hm) s))

;; Hangman 1String -> Boolean
;; Was s already guessed in given game?
(define (hangman-already-missed? hm s)
  (los-contains? (hangman-misses hm) s))

;; Hangman -> Boolean
;; Is the game over with a loss?
(check-expect (hangman-done? H0) #false)
(check-expect (hangman-done? (make-hangman '() (explode "abcxyz"))) #false)
(check-expect (hangman-done? (make-hangman '() (explode "abcdxyz"))) #true)
(define (hangman-done? hm)
  (> (length (hangman-misses hm)) 6))

;; Hangman -> Image
;; Draw losing hangman game over scene
(check-expect (hangman-draw-done (make-hangman (string-hide "abc") '()))
              (overlay (text "ded" SIZE "purple")
                       (hangman-draw (make-hangman (string-hide "abc") '()))))
(define (hangman-draw-done hm)
  (overlay (text "ded" SIZE "purple")
           (hangman-draw hm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Word functions

;; Word -> Image
;; Draw each letter of word
(check-expect (word-draw '()) empty-image)
(check-expect (word-draw H_E_L_L_O_)
              (beside (letter-draw "_") (letter-draw "_")
                      (letter-draw "_") (letter-draw "_")
                      (letter-draw "_")))
(check-expect (word-draw H_E_L*L*O_)
              (beside (letter-draw "_") (letter-draw "_")
                      (letter-draw "l") (letter-draw "l")
                      (letter-draw "_")))
(define (word-draw w)
  (cond [(empty? w) empty-image]
        [(cons? w)
         (beside (letter-draw (first w))
                 (word-draw (rest w)))]))

;; Word String -> Boolean
;; Is hw hiding s?
(check-expect (word-hiding? '() "a") #false)
(check-expect (word-hiding? H_E_L*L*O_ "a") #false)
(check-expect (word-hiding? H_E_L*L*O_ "l") #false)
(check-expect (word-hiding? H_E_L*L*O_ "o") #true)
(define (word-hiding? hw s)
  (cond [(empty? hw) #false]
        [(cons? hw)
         (cond [(hidden? (first hw))
                (or (string=? (hidden-sec (first hw)) s)
                    (word-hiding? (rest hw) s))]
               [else
                (word-hiding? (rest hw) s)])]))

;; Word 1String -> Word
;; Reveal every hidden occurrence of s in hw
(check-expect (word-reveal '() "a") '())
(check-expect (word-reveal H_E_L_L_O_ "l") H_E_L*L*O_)
(check-expect (word-reveal (rest (rest H_E_L_L_O_)) "l")
              (rest (rest H_E_L*L*O_)))
(check-expect (word-reveal H_E_L_L_O_ "a") H_E_L_L_O_)
(define (word-reveal w s)
  (cond [(empty? w) '()]
        [(cons? w)
         (cons (letter-reveal (first w) s)
               (word-reveal (rest w) s))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Letter functions

;; Letter String -> Letter
;; Reveal letter if it is hiding s (otherwise no change)
(check-expect (letter-reveal (make-hidden "A") "a") "A")
(check-expect (letter-reveal (make-hidden "B") "a") (make-hidden "B"))
(check-expect (letter-reveal "C" "a") "C")
(define (letter-reveal hl s)
  (cond [(hidden? hl)
         (cond [(string-ci=? (hidden-sec hl) s) (hidden-sec hl)]
               [else hl])]
        [(string? hl) hl]))

;; Letter -> Letter
;; Draw a hidden letter
(check-expect (letter-draw "a")
              (overlay (text "a" SIZE "red") SQ))
(check-expect (letter-draw (make-hidden "a"))
              (overlay (text "_" SIZE "red") SQ))
(define (letter-draw hl)
  (overlay (text (cond [(hidden? hl)  "_"]
                       [(string? hl) hl])
                 SIZE
                 "red")
           SQ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc functions

;; String -> Word
;; Make a word hiding all of s
(check-expect (string-hide "ab")
              (cons (make-hidden "a")
                    (cons (make-hidden "b") '())))
(define (string-hide s)
  (cond [(string=? s "") '()]
        [else
         (cons (make-hidden (substring s 0 1))
               (string-hide (substring s 1)))]))

;; LoString String -> Boolean
;; Does los contain s?
(check-expect (los-contains? '() "a") #false)
(check-expect (los-contains? (cons "a" '()) "a") #true)
(check-expect (los-contains? (cons "A" '()) "a") #true)
(check-expect (los-contains? (cons "b" '()) "a") #false)
(check-expect (los-contains? (cons "b" (cons "a" '())) "a") #true)
(define (los-contains? los s)
  (cond [(empty? los) #false]
        [(cons? los)
         (or (string-ci=? s (first los))
             (los-contains? (rest los) s))]))

;; String -> Boolean
;; Is the string an alphabetic 1String?
(check-expect (1string-alphabetic? "left") #false)
(check-expect (1string-alphabetic? "8") #false)
(check-expect (1string-alphabetic? "a") #true)
(define (1string-alphabetic? s)
  (and (string-alphabetic? s)
       (= 1 (string-length s))))