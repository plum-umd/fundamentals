#lang scribble/manual
@(require scribble/core 
          scribble/examples
          racket/sandbox
          (for-label lang/htdp-beginner) 
          (for-label (except-in 2htdp/image image?))
          "helper.rkt" 
	  "../utils.rkt"
          "../defns.rkt")

@(define the-eval
  (let ([the-eval (make-base-eval)])
    ;(the-eval '(require lang/htdp/intermediate))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
the-eval))

@title{Have a Nice Day!}

@examples[
  #:eval the-eval
  #:hidden  

(define RADIUS 100)
(define SMILE-FACTOR 8/10)
(define EYE-FACTOR 1/3)

(define SMILE-RADIUS (* RADIUS SMILE-FACTOR))

;; hspace : Number -> Image
;; Create given amount of horizontal space as an image
;(check-expect (hspace 10) (rectangle 10 0 "solid" "black"))
(define (hspace x)
  (rectangle x 0 "solid" "black"))

;; bottom-half : Image -> Image
;; Crop an image to just it's bottom half
;(check-expect (bottom-half (rectangle 100 60 "solid" "red"))
;              (rectangle 100 30 "solid" "red"))
(define (bottom-half i)
  (crop 0
        (/ (image-height i) 2)
        (image-width i)
        (/ (image-height i) 2) i))

;; Construct expressions that produce each of the following:
(define HEAD
  (circle RADIUS "solid" "yellow"))

(define EYE
  (ellipse (* EYE-FACTOR RADIUS)
           (* 2 EYE-FACTOR RADIUS)
           "solid"
           "black"))

(define EYES
  (beside EYE (hspace (* RADIUS EYE-FACTOR)) EYE))

(define SMILE-BLACK-CIRCLE
  (circle SMILE-RADIUS "solid" "black"))

(define SMILE-YELLOW-CIRCLE
  (circle (* 9/10 SMILE-RADIUS) "solid" "yellow"))

(define SMILE
  (bottom-half
   (overlay (circle (* 9/10 SMILE-RADIUS) "solid" "yellow")
            (circle SMILE-RADIUS "solid" "black"))))

;; center-top-pinhole : Image -> Image
;; Put pinhole halfway across and at top of given image
#;(check-expect (center-top-pinhole HEAD)
              (put-pinhole RADIUS 0 HEAD))
(define (center-top-pinhole i)
  (put-pinhole (/ (image-width i) 2) 0 i))

;; center-bottom-pinhole : Image -> Image
;; Put pinhole halfway across and at bottom of given image
#;(check-expect (center-bottom-pinhole HEAD)
              (put-pinhole RADIUS (* 2 RADIUS) HEAD))
(define (center-bottom-pinhole i)
  (put-pinhole (/ (image-width i) 2) (image-height i) i))
]



Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/beginner.html"]{Beginning Student
Language}. Require the HtDP2e image library at the top of your definitions:
@racketblock[(require 2htdp/image)]

@colorize["red"]{Hint}: You can search for documentation by right-clicking any
identifier @tt{foo} in the @italic{definitions} or @italic{interactions} windows
and choosing 'Search in Help Desk for "foo"' menu option. You can also press
<F1> to search for documentation about any identifier under the cursor.


@section{Oveview}

@image[#:style float-right]{img/smiley.png}

The goal of this lab is to use the ``arithmetic of images,'' as
provided by the @racketmodname[2htdp/image] library, to construct an
image that looks like the iconic
@link["https://en.wikipedia.org/wiki/Smiley"]{Smiley}, ``a stylized
representation of a smiling humanoid face that is a part of popular
culture worldwide.''

An example of the desired result is given to the right.

This problem can be broken into two sub-problems:
@itemlist[
@item{construct elements of the smiley (head, eyes, smile)}
@item{compose those elements to create a smiley}
]

@section{Deconstructing happiness}

Before you begin, open a DrRacket window and @bold{actively
read} the documentation for @racket[circle], @racket[ellipse],
@racket[rectangle], @racket[beside], @racket[overlay],
@racket[image-height], @racket[image-width], and @racket[crop].

@margin-note{Actively read means more than just reading; it means
reading, writing examples, playing, and experimenting as you go.}

The smiley consists of a few distinct parts:

@(define-syntax-rule (result e) 
   @examples[#:eval the-eval #:result-only e])

@itemlist[
@item{the head: @result[HEAD]}
@item{the eyes: @result[EYES]}
@item{the smile: @result[SMILE]}
]

Some of these parts can in turn are constructed out of
futher subparts: 
@itemlist[
@item{the eyes consist of two eyes (with space between
them): @result[EYE]}
@item{the smile can be constructed from two circles, overlaying them, and taking the bottom half:
@result[(circle (* 9/10 SMILE-RADIUS) "solid" "yellow")]
@result[(circle SMILE-RADIUS "solid" "black")]
@result[(overlay (circle (* 9/10 SMILE-RADIUS) "solid" "yellow") (circle SMILE-RADIUS "solid" "black"))]
}
]


@exercise{

To help construct these images, complete the following helper function
definitions, which have been stubbed for you:

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; hspace : Number -> Image
;; Create given amount of horizontal space as an image
(check-expect (hspace 10) (rectangle 10 0 "solid" "black"))
(define (hspace x) 
  empty-image) ; stub

;; bottom-half : Image -> Image
;; Crop an image to just it's bottom half
(check-expect (bottom-half (rectangle 100 60 "solid" "red"))
              (rectangle 100 30 "solid" "red"))
(define (bottom-half i) 
  i) ; stub
}|

}


Now write expressions that produce each of the images show in this lab
write-up.  You may find @racket[hspace] and @racket[bottom-half]
useful.

@exercise{

Give three definitions that correctly define the head, eyes, and smile
images:

@codeblock[#:keep-lang-line? #false]|{
#lang racket
(define HEAD ...)
(define EYES ...)
(define SMILE ...)
}|
}

@exercise{

Define a constant @racket[RADIUS], which determines the radius
of the smiley face.  Reformulate the definitions of @racket[HEAD],
@racket[EYES], and @racket[SMILE] in terms of @racket[RADIUS].

Use the following proportions to calculate the sizes of the other
images:

@itemlist[

@item{The eyes should have a height of 2/3 * @racket[RADIUS],
width of 1/3 * @racket[RADIUS], and space of 1/3 *
@racket[RADIUS] between them.}

@item{The smile should be constructed from a black circle that has a
radius of 8/10 * @racket[SMILE-RADIUS] and a yellow circle that is 9/10
of that quantity.}
]

Make sure that changing the value of @racket[RADIUS] doesn't
break your program and produces a smiley image that is consistently
scaled up or down.
}


@section{Dancing on a pinhole}

Now it's time to put the pieces together.

Actively read the documentation for @racket[put-pinhole],
@racket[clear-pinhole], and @racket[overlay/pinhole].

@exercise{

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; center-top-pinhole : Image -> Image
;; Put pinhole halfway across and at top of given image
(check-expect (center-top-pinhole HEAD)
              (put-pinhole RADIUS 0 HEAD))
(define (center-top-pinhole i)
  i) ; stub

;; center-bottom-pinhole : Image -> Image
;; Put pinhole halfway across and at bottom of given image
(check-expect (center-bottom-pinhole HEAD)
              (put-pinhole RADIUS (* 2 RADIUS) HEAD))
(define (center-bottom-pinhole i)
  i) ; stub
}|

}

