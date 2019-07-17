#lang scribble/manual
@(require scribble/core 
          scribble/examples
          racket/sandbox
          (for-label lang/htdp-beginner) 
          (for-label (except-in 2htdp/image image?) 2htdp/universe)
          ;"helper.rkt" 
	  "../utils.rkt"
          "../defns.rkt")

@(define-syntax-rule (result e) 
   @examples[#:eval the-eval #:result-only e])


@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (only-in lang/htdp-intermediate check-expect empty? define-struct cons? first rest cons explode)))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (only-in lang/htdp-beginner identity string-whitespace?)))
    (the-eval '(require (prefix-in r: racket)))
the-eval))


@examples[#:eval the-eval #:hidden
;; place code here
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
  (cond [(hangman-already-missed? hm s) hm]
        [(hangman-hiding? hm s)
         (make-hangman (word-reveal (hangman-word hm) s)
                       (hangman-misses hm))]
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
  (los-contains-ci? (hangman-misses hm) s))

;; Hangman -> Boolean
;; Is the game over with a loss?
(check-expect (hangman-done? H0) #false)
(check-expect (hangman-done? (make-hangman '() (explode "abcxyz"))) #false)
(check-expect (hangman-done? (make-hangman '() (explode "abcdxyz"))) #true)
(define (hangman-done? hm)
  (> (length (hangman-misses hm)) 6))

;; Hangman -> Image
;; Draw losing hangman game over scene
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
(define (word-draw hw)
  (cond [(empty? hw) empty-image]
        [(cons? hw)
         (beside (letter-draw (first hw))
                 (word-draw (rest hw)))]))

;; Word String -> Boolean
;; Is hw hiding s?
(check-expect (word-hiding? '() "a") #false)
(check-expect (word-hiding? H_E_L*L*O_ "a") #false)
(check-expect (word-hiding? H_E_L*L*O_ "l") #false)
(check-expect (word-hiding? H_E_L*L*O_ "o") #true)
(define (word-hiding? hw s)
  (cond [(empty? hw) #false]
        [(cons? hw)
         (or (and (hidden? (first hw))
                  (string-ci=? (hidden-sec (first hw)) s))
             (word-hiding? (rest hw) s))]))

;; Word 1String -> Word
;; Reveal every hidden occurrence of s in hw
(check-expect (word-reveal '() "a") '())
(check-expect (word-reveal H_E_L_L_O_ "l") H_E_L*L*O_)
(check-expect (word-reveal H_E_L_L_O_ "a") H_E_L_L_O_)
(define (word-reveal hw s)
  (cond [(empty? hw) '()]
        [(cons? hw)
         (cons (letter-reveal (first hw) s)
               (word-reveal (rest hw) s))]))

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
;; Does los contain s (case insensitively)?
(check-expect (los-contains-ci? '() "a") #false)
(check-expect (los-contains-ci? (cons "a" '()) "a") #true)
(check-expect (los-contains-ci? (cons "A" '()) "a") #true)
(check-expect (los-contains-ci? (cons "b" '()) "a") #false)
(define (los-contains-ci? los s)
  (cond [(empty? los) #false]
        [(cons? los)
         (or (string-ci=? (first los) s)
             (los-contains-ci? (rest los) s))]))

;; String -> Boolean
;; Is the string an alphabetic 1String?
(check-expect (1string-alphabetic? "left") #false)
(check-expect (1string-alphabetic? "8") #false)
(check-expect (1string-alphabetic? "a") #true)
(define (1string-alphabetic? s)
  (and (string-alphabetic? s)
       (= 1 (string-length s))))

]

@title[#:style 'unnumbered #:tag "lab7"]{Lab 7: Rite of Words and Life}

@(define ex (make-exerciser "Lab problem"))


@section[#:tag "lab7intro"]{Introduction(s)}

You'll work in labs in pairs.  Find someone to work with for this
first lab and introduce yourself. 

Make sure at least one of you have a laptop to work on for this lab.

The two of you will work as a team to solve problems. At any time, one of you
will be the @bold{Head} and the other will be the @bold{Hands}. The @bold{Head}
does the thinking and the @bold{Hands} does the typing. @bold{Hands} type only
what the @bold{Head} tells them to, but you're free to discuss any issues that
pop up. We'll have you switch off during the lab to make sure each of you get
practice problem solving, dealing with syntax, and getting finger exercises on
the keyboard.

@section[#:tag "lab7:purpose"]{Purpose}

In this lab, you'll practice 
@itemlist[
@item{refining programs designed by others and}
@item{using lists to represent arbitrarily large collections of data.}]

@section{Hangman}

In this lab, you will be finishing a program designed to implement (a
simplified version of)
@link["https://en.wikipedia.org/wiki/Hangman_(game)"]{Hangman}.

In this simplified version of the game, you are presented with a
number of boxes, indicating that your job is to guess a word with that
many letters in it.  Pressing any alphabetic key will register a guess
of that letter.  If the letter is present in the word you're trying to
guess, the appropriate boxes will reveal that letter.  If it is not
present, you've registered a wrong guess and are one step closer to
losing (although there will be no visual indication of this other than
no letters appear).  Make more than 6 wrong guesses and you've lost
and the game will end.

An almost complete design of the program is available here:
@link["hangman.rkt"]{@tt{hangman.rkt}}.

Here are the critical data definitions for the program:

@codeblock[#:keep-lang-line? #false]|{
#lang racket
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
}|


@subsection{Letter functions}

Let's start by looking at the @tt{Letter} data definition.  A letter
can be in one of two states: either it is hidden (and not shown to the
player) or it has been guessed and revealed.  There are two functions
for operating on letters, @racket[letter-draw] and
@racket[letter-reveal].  Here are some examples to illustrate their
use.

@examples[#:eval the-eval
(letter-draw (make-hidden "a"))
(letter-draw "a")

(letter-draw (letter-reveal (make-hidden "a") "a"))
(letter-draw (letter-reveal (make-hidden "a") "b"))
(letter-draw (letter-reveal "a" "b"))
]

@subsection{Word functions}

Words are simply lists of letters.  

Your first job is to design the functions @racket[word-draw] and
@racket[word-reveal], which are like the functions for letters, but
work at the level of words. (Stubs are included in the code provided.)

There's a convenience function provided which takes a string an makes
a @tt{Word} that hides the complete string.  Here's how
@racket[word-draw] and @racket[word-reveal] should work.

@examples[#:eval the-eval
(word-draw (string-hide "abc"))
(word-draw (word-reveal (string-hide "abc") "a"))
(word-draw (word-reveal (string-hide "abc") "b"))
(word-draw (word-reveal (word-reveal (string-hide "abc") "a") "b"))
]

@ex["Word functions"]{

Finish the design of @racket[word-reveal] and @racket[word-draw].

}

@subsection{Hangman functions}

Of the functions that operate on @tt{Hangman} values, the critical one
is @tt{hangman-guess}, which consumes the representation of a hangman
game state and updates it according to a guess.

Three possible scenarios can occur:

@itemlist[
@item{The guess is correct and letters are revealed.}
@item{The guess is incorrect, but previously made, so no update occurs.}
@item{The guess is incorrect and @emph{not} previously made, so the guess is
added to the list of bad guesses.}
]

Here's a design for the @racket[hangman-guess] function (tests are in
the code provided):

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; Hangman 1String -> Hangman
;; Guess s in given game
(define (hangman-guess hm s)  
  (cond [(hangman-hiding? hm s)
         (make-hangman (word-reveal (hangman-word hm) s)
                       (hangman-misses hm))]
        [(hangman-already-missed? hm s) hm]        
        [else
         (make-hangman (hangman-word hm)
                       (cons s (hangman-misses hm)))]))
}|

It relies upon two helper functions: @racket[hangman-already-missed?],
which determines if the given guess was previously guessed (and
wrong), and @racket[hangman-hiding?] which determines if a given guess
is hidden in the current game.

Here are some examples.  We're using the @racket[explode] function as
a convenience for turning a string into a list of its @tt{1String}
parts.

@examples[#:eval the-eval
(hangman-already-missed? (make-hangman (string-hide "Hello") '()) "b")
(hangman-already-missed? (make-hangman (string-hide "Hello") (explode "abc")) "b")
(hangman-already-missed? (make-hangman (string-hide "Hello") (explode "abc")) "d")
(hangman-hiding? (make-hangman (string-hide "Hello") '()) "e")
(hangman-hiding? (make-hangman (string-hide "Hello") '()) "z")
(hangman-hiding? (make-hangman (word-reveal (string-hide "Hello") "e") '()) "e")
]

The @racket[hangman-already-missed?] function needs to check for a
given guess in the list of bad guesses in the given hangman.  This
task calls for another helper function which consumes a list of string
and a string and determines if the string is in the list.  With such a
function, @racket[hangman-already-missed?] becomes easy:

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; Hangman 1String -> Boolean
;; Was s already guessed in given game?
(define (hangman-already-missed? hm s)
  (los-contains? (hangman-misses hm) s))
}|

@ex["List of strings function"]{

Design the @racket[los-contains?] function.

}

The @racket[hangman-hiding?] function needs to check the word to see
if contains a hidden letter corresponding to the guess.  This task
also suggest the need for a helper function that makes the design of
@racket[hangman-hiding?] trivial:

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; Hangman -> Boolean
;; Is s a hidden letter in the given game?
(define (hangman-hiding? hm s)
  (word-hiding? (hangman-word hm) s))
}|

@ex["Word hiding function"]{

Design the @racket[word-hiding?] function.

}

@section{A Trip to the Gallows}

Putting the pieces together is pretty easy.  Here's the main function:

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main

;; String -> Hangman
;; Play a game of hangman trying to guess s
(define (main s)
  (big-bang (make-hangman (string-hide s) '())
    [on-key hangman-key]
    [to-draw hangman-draw]
    [stop-when hangman-done? hangman-draw-done]))
}|

Note the use of a @racket[stop-when] clause.  This new
@racket[big-bang] clause is used to specify a predicate to determine
if the game is over.  Whenever it returns true of the current game
state, the program stops, and the final state is rendered using the
given drawing function.

If you're curious about @racket[hangman-key], @racket[hangman-done?],
or @racket[hangman-draw-done], take a look at the provided code.

Once you've completed the above problems, give it a try with
@racket[(main "tangerine")] or whichever word you'd like to use.
