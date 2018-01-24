#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")

@title[#:style 'unnumbered #:tag "lab6"]{Lab 6: Chatting about Design}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/beginner.html"]{Beginning Student
Language}. Require the HtDP2e
@link["https://docs.racket-lang.org/teachpack/2htdpimage.html"]{image} library
at the top of your definitions: @racketblock[(require 2htdp/image)]

Make sure you follow
@link["https://cs.umd.edu/class/fall2017/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.

Make sure you save and submit your definitions, we will be extending this
program in future labs. Choose the initial @bold{Head} and @bold{Hands}, and get
started!


@section[#:style 'unnumbered #:tag "lab6:msg"]{Messages}

Recall @tt{add-message : String String Image -> Image} from the second
assignment. The @italic{message} @tt{add-message} adds to the history has two
parts: the name and the content.

@larger{@bold{Ex 1}}: Using the given structure definition @tt{msg}, design a
data definition and template for @tt{Message}s.

@#reader scribble/comment-reader (racketblock
(define-struct msg (name content))
;; A Message is a ______.
(define (message-template msg) ______)
(define msg1 (make-msg "DVH" "Hello, world!"))
(define msg2 (make-msg "Nick & Sam" "Help! What should we do?!"))
(define msg3 (make-msg "DVH" "Do not be afraid; do not be discouraged, for the Design Recipe will be with you wherever you go."))
)

@larger{@bold{Ex 2}}: Give three data examples of @tt{Message}s (other than the
three we provided).

@larger{@bold{Ex 3}}: Design the function @tt{message->image} that returns an
image representation of the given @tt{Message}. The name and content of the
@tt{Message} must have different colors.  Be sure to write tests for your three
@tt{Message} examples before implementing the function.

@larger{@bold{Ex 4}}: Change the color of the font you used for the message
name. Make sure you update your tests as well.

@larger{@bold{Ex 5}}: Change the size of the font you used for the message
content. Make sure you update your tests as well.

@larger{@bold{Ex 6}}: If you haven't by now, define and name all constants used
in the function @tt{message->image} for easy customization.


@section[#:style 'unnumbered #:tag "lab6:redux"]{Add-Message Redux}

Swap @bold{Head} and @bold{Hands}!

Add the following snippet to your definitions window.

@#reader scribble/comment-reader (racketblock
;; A History is an Image.
(define empty-history empty-image)
(define hist1 (add-message msg1 empty-history))
(define hist2 (add-message msg2 hist1))
(define hist3 (add-message msg3 hist2))
)

@larger{@bold{Ex 7}}: What kind of data is a @tt{History} (e.g. enumeration,
atomic, composite, recursive)? Answer in a comment in the definitions window.

@larger{@bold{Ex 8}}: Design the function @tt{add-message} that adds the given
@tt{Message} below the given @tt{History}, returning a new @tt{History}. The
text of the messages should be aligned on the left. Make sure you write your
tests before you implement the function.


@section[#:style 'unnumbered #:tag "lab6:textbox"]{Writing Messages}

We want to be able to send messages, but first we need to know how to write
messages!

Take a look at the following data definition, then copy it into your definitions
window.

@#reader scribble/comment-reader (racketblock
;; A Natural is a non-negative integer.

(define-struct textbox (content cursor))
;; A Textbox is a (make-textbox String Natural)
;; Interp: a textbox (make-textbox str n) has its cursor immediately before the
;; Nth character of STR. The cursor's value must never exceed the length of
;; the string.
(define tb0 (make-textbox "foo " 0))
(define tb1 (make-textbox "foo " 1))
(define tb2 (make-textbox "foo " 2))
(define tb3 (make-textbox "foo " 3))
(define tb4 (make-textbox "foo " 4))
)

@larger{@bold{Ex 9}}: Design the function @tt{before-cursor : Textbox -> String}
which returns the string content of the given @tt{Textbox} that comes before the
cursor.

@larger{@bold{Ex 10}}: Design the function @tt{after-cursor : Textbox -> String}
which returns the string content of the given @tt{Textbox} that comes after the
cursor.

@larger{@bold{Ex 11}}: Implement the function @tt{textbox->image} that creates
an image representation of a @tt{Textbox}. The content can be any size or color
you want, and you should draw the cursor as a tall, skinny rectangle between the
content that comes before and after the cursor.

@#reader scribble/comment-reader (racketblock
;; textbox->image : Textbox -> Image
;; Create an image representation of the given Textbox.
(define TBCTNT-COLOR "orange")
(define TBCTNT-SIZE 16)
(define TBCURS-COLOR "red")
(define TBCURS-HEIGHT 20)
(define TBCURS-WIDTH 2)
(define TBCURS (rectangle TBCURS-WIDTH TBCURS-HEIGHT 'solid TBCURS-COLOR))

(define (textbox->image tb) empty-image) ; <- stub

(check-expect (textbox->image tb0) ...) ; => |foo 
(check-expect (textbox->image tb1) ...) ; => f|oo 
(check-expect (textbox->image tb2) ...) ; => fo|o 
(check-expect (textbox->image tb3) ...) ; => foo| 
(check-expect (textbox->image tb4) ...) ; => foo |
)
