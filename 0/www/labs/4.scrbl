#lang scribble/manual
@(require scribble/core 
          scribble/examples
          racket/sandbox
          (for-label lang/htdp-beginner) 
          (for-label (except-in 2htdp/image image?))
          ;"helper.rkt" 
	  "../utils.rkt"
          "../defns.rkt")

@(define-syntax-rule (result e) 
   @examples[#:eval the-eval #:result-only e])


@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (only-in lang/htdp-intermediate check-expect)))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (only-in lang/htdp-beginner identity string-whitespace?)))
    (the-eval '(require (prefix-in r: racket)))
the-eval))


@examples[#:eval the-eval #:hidden
(define (small left right)
  (add-outline (add-cursor (text left 25 "black")
                           (text right 25 "black"))))

(define (add-outline img)
  (overlay img (empty-scene (image-width img) (image-height img))))

(define (show left right)
  (add-cursor (text left 50 "black")
              (text right 50 "black")))

(define (add-cursor img1 img2)
  (beside img1
          (rectangle 2
                     (max (image-height img1) (image-height img2))                     
                     "solid"
                     "red")
          img2))

]



@title[#:style 'unnumbered #:tag "lab4"]{Lab 3: Typing, Cursing, Texting}

@(define ex (make-exerciser "Lab problem"))


@section[#:tag "lab4intro"]{Introduction(s)}

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

@section[#:tag "lab4:purpose"]{Purpose}

In this lab, you'll practice writing functions and building simple
world programs using the @bold{design recipe} for systematic
computational problem solving.

@section{A simple line text editor}

In this lab, you will write the core functions that are part of a very
simple text editor for a single line of text (this could be part of a
messaging app, a search bar, an SMS application, etc.).  This program
lets a user type in a line of text and move a cursor around and edit
that text.

The heart of this program is the following data definition:

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; A Line is a (make-txt String String)
;; Interpretation: (make-txt string1 string2) is a line of text consisting
;; of string1 and string2 with a cursor placed between them.
(define-struct txt (left right))
}|

So for example if someone has typed something that looks like this
(where the red bar is the cursor):

@result[(show "where u" " at?")]

it would be represented with the data 
@racket[(make-txt "where u" " at?")].

The user could move the cursor left causing the line to look like:

@result[(show "where " "u at?")]

which would be represented as @racket[(make-txt "where " "u at?")].

The user could use the backspace (or delete) key to erase the
character to the left of the cursor:

@result[(show "where" "u at?")]

which would be represented as @racket[(make-txt "where" "u at?")].


You will @bold{not} be making the text editor (yet), but rather will
design a handful of functions that will be useful in building the
editor.  For each function, you will be given a signature and a couple
of examples @emph{presented visually}.  It will be up to you to map
this visual presentation of the information in to the data
representation.

For each function, use the steps of the design recipe to arrive at a
correct solution for the function.

You will need to copy the data definition from above in to the top of
your file.


@ex[@racket[txt-length]]{

Design the function @tt{txt-length : Line -> Number}.

@racket[(txt-length #,(result (small "" "")))] ⟹  @result[0]

@racket[(txt-length #,(result (small "Rip Torn" "")))] ⟹  @result[8]

@racket[(txt-length #,(result (small "" "Rip Torn")))] ⟹  @result[8]

@racket[(txt-length #,(result (small "Rip" " Torn")))] ⟹  @result[8]

}

@ex[@racket[txt-string]]{

Design the function @tt{txt-string : Line -> Number}.

@racket[(txt-length #,(result (small "" "")))] ⟹  @result[""]

@racket[(txt-length #,(result (small "Rip Torn" "")))] ⟹  @result["Rip Torn"]

@racket[(txt-length #,(result (small "" "Rip Torn")))] ⟹  @result["Rip Torn"]

@racket[(txt-length #,(result (small "Rip" " Torn")))] ⟹  @result["Rip Torn"]

}


@ex[@racket[txt-insert]]{

Design the function @tt{txt-insert : Line String -> Line}.

@racket[(txt-insert #,(result (small "" "")) "R")] ⟹  @result[(small "R" "")]

@racket[(txt-insert #,(result (small "Rip Tor" "")) "n")] ⟹  @result[(small "Rip Torn" "")]

@racket[(txt-insert #,(result (small "Rip T" "rn")) "o")] ⟹  @result[(small "Rip To" "rn")]

}


@ex[@racket[txt-forward]]{

Design the function @tt{txt-forward : Line -> Line}.

@racket[(txt-forward #,(result (small "" "")))] ⟹  @result[(small "" "")]

@racket[(txt-forward #,(result (small "Rip Torn" "")))] ⟹  @result[(small "Rip Torn" "")]

@racket[(txt-forward #,(result (small "Rip To" "rn")))] ⟹  @result[(small "Rip Tor" "n")]

@racket[(txt-forward #,(result (small "" "Rip Torn")))] ⟹  @result[(small "R" "ip Torn")]

}


@ex[@racket[txt-backward]]{

Design the function @tt{txt-backward : Line -> Line}.

@racket[(txt-backward #,(result (small "" "")))] ⟹  @result[(small "" "")]

@racket[(txt-backward #,(result (small "Rip Torn" "")))] ⟹  @result[(small "Rip Tor" "n")]

@racket[(txt-backward #,(result (small "Rip To" "rn")))] ⟹  @result[(small "Rip T" "orn")]

@racket[(txt-backward #,(result (small "" "Rip Torn")))] ⟹  @result[(small "" "Rip Torn")]

}

@ex[@racket[txt-backspace]]{

Design the function @tt{txt-backspace : Line -> Line}.

@racket[(txt-backspace #,(result (small "" "")))] ⟹  @result[(small "" "")]

@racket[(txt-backspace #,(result (small "Rip Torn" "")))] ⟹  @result[(small "Rip Tor" "")]

@racket[(txt-backspace #,(result (small "Rip To" "rn")))] ⟹  @result[(small "Rip T" "rn")]

@racket[(txt-backspace #,(result (small "" "Rip Torn")))] ⟹  @result[(small "" "Rip Torn")]

}

@ex[@racket[txt-clear-left]]{

Design the function @tt{txt-clear-left : Line -> Line}.

@racket[(txt-clear-left #,(result (small "" "")))] ⟹  @result[(small "" "")]

@racket[(txt-clear-left #,(result (small "Rip Torn" "")))] ⟹  @result[(small "" "")]

@racket[(txt-clear-left #,(result (small "Rip To" "rn")))] ⟹  @result[(small "" "rn")]

@racket[(txt-clear-left #,(result (small "" "Rip Torn")))] ⟹  @result[(small "" "Rip Torn")]

}

@ex[@racket[txt-clear-right]]{

Design the function @tt{txt-clear-right : Line -> Line}.

@racket[(txt-clear-right #,(result (small "" "")))] ⟹  @result[(small "" "")]

@racket[(txt-clear-right #,(result (small "Rip Torn" "")))] ⟹  @result[(small "Rip Torn" "")]

@racket[(txt-clear-right #,(result (small "Rip To" "rn")))] ⟹  @result[(small "Rip To" "")]

@racket[(txt-clear-right #,(result (small "" "Rip Torn")))] ⟹  @result[(small "" "")]

}

@ex[@racket[txt-start]]{

Design the function @tt{txt-start : Line -> Line}.

@racket[(txt-start #,(result (small "" "")))] ⟹  @result[(small "" "")]

@racket[(txt-start #,(result (small "Rip Torn" "")))] ⟹  @result[(small "" "Rip Torn")]

@racket[(txt-start #,(result (small "Rip To" "rn")))] ⟹  @result[(small "" "Rip Torn")]

@racket[(txt-start #,(result (small "" "Rip Torn")))] ⟹  @result[(small "" "Rip Torn")]

}

@ex[@racket[txt-end]]{

Design the function @tt{txt-end : Line -> Line}.

@racket[(txt-end #,(result (small "" "")))] ⟹  @result[(small "" "")]

@racket[(txt-end #,(result (small "Rip Torn" "")))] ⟹  @result[(small "Rip Torn" "")]

@racket[(txt-end #,(result (small "Rip To" "rn")))] ⟹  @result[(small "Rip Torn" "")]

@racket[(txt-end #,(result (small "" "Rip Torn")))] ⟹  @result[(small "Rip Torn" "")]

}

@ex[@racket[txt-transpose]]{

Design the function @tt{txt-transpose : Line -> Line}.

@racket[(txt-transpose #,(result (small "" "")))] ⟹  @result[(small "" "")]

@racket[(txt-transpose #,(result (small "Rip Torn" "")))] ⟹  @result[(small "Rip Torn" "")]

@racket[(txt-transpose #,(result (small "Rip To" "rn")))] ⟹  @result[(small "Rip Tr" "on")]

@racket[(txt-transpose #,(result (small "" "Rip Torn")))] ⟹  @result[(small "" "Rip Torn")]

}

