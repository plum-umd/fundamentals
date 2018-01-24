#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")

@title[#:style 'unnumbered #:tag "lab7"]{Lab 7: Chatting in Action}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/beginner.html"]{Beginning Student
Language}. Require the HtDP2e
@link["https://docs.racket-lang.org/teachpack/2htdpimage.html"]{image} and
@link["https://docs.racket-lang.org/teachpack/2htdpuniverse.html"]{universe}
libraries at the top of your definitions: @racketblock[(require 2htdp/image)
(require 2htdp/universe)]

Make sure you follow
@link["https://cs.umd.edu/class/fall2017/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.

Open your solutions to
@link["https://cs.umd.edu/class/fall2017/cmsc131A/lab6.html"]{lab 6}. If you
haven't already completed lab 6, do so before starting this lab.

Make sure you save and submit your definitions, we will be extending this
program in future labs. 

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab7:review"]{Same Lab Time, Same Lab Channel!}

In the last lab we gave data definitions for @tt{Message}, @tt{History}, and
@tt{Textbox}:

@#reader scribble/comment-reader (racketblock
(define-struct msg (name content))
;; A Message is a (make-msg String String).
;; Interp: A Message `m' represents the chat message `(msg-content m)'
;; from the sender `(msg-name m)'.

;; message-template : Message -> ???
;; Given any Message, we can pull out the sender's name and message content.
(define (message-template msg)
  (... (msg-name msg) ... (msg-content msg) ...))

(define msg0 (make-msg "Sam" "BSL is the best language ever!"))
(define msg1 (make-msg "Austin" "Nah, Coq is where it's at"))
(define msg2 (make-msg "DVH" "*head explodes*"))

;; A History is an Image.
(define empty-history empty-image)

;; history-template : History -> ???
(define (history-template hist)
  (... hist ...))
(define hist1 (add-message msg0 empty-history))
(define hist2 (add-message msg1 hist1))
(define hist3 (add-message msg2 hist2))

(define-struct textbox (content cursor))

;; A Natural is a non-negative integer.
;; A Textbox is a (make-textbox String Natural)
;; Interp: a textbox (make-textbox str n) has its cursor 
;; immediately before the `n'th character of `str'. The
;; cursor's value must never exceed the length of the
;; string.

;; textbox-template : Textbox -> ???
;; Given any Textbox, we can pull out its content and
;; cursor location.
(define (textbox-template tb)
  (... (textbox-content tb) ... (textbox-cursor tb) ...))

(define tb0 (make-textbox "foo " 0)) ; => |foo 
(define tb1 (make-textbox "foo " 1)) ; => f|oo 
(define tb2 (make-textbox "foo " 2)) ; => fo|o 
(define tb3 (make-textbox "foo " 3)) ; => foo| 
(define tb4 (make-textbox "foo " 4)) ; => foo |
)

We implemented the following functions:
@itemlist[
  @item{@tt{message->image : Message -> Image}}
  @item{@tt{add-message : Message History -> History}}
  @item{@tt{before-cursor : Textbox -> String}}
  @item{@tt{after-cursor : Textbox -> String}}
  @item{@tt{textbox->image : Textbox -> Image}}
]

@section[#:style 'unnumbered #:tag "lab7:edits"]{Creating New @tt{Textbox}es}

We need to be able to insert and remove content from our @tt{Textbox}, as well
as move around. We do that by creating new @tt{Textbox}es with modified content
from given @tt{Textbox}es.

@larger{@bold{Ex 1}}: Design a function @tt{textbox-insert} that, given a
@tt{Textbox} and a string, returns a new @tt{Textbox} with the new content
inserted at the appropriate location with an updated cursor.

@#reader scribble/comment-reader (racketblock
(check-expect (textbox-insert tb2 "") (make-textbox "foo " 2))
(check-expect (textbox-insert tb2 "f") (make-textbox "fofo " 3))
(check-expect (textbox-insert tb2 "o") (make-textbox "fooo " 3))
(check-expect (textbox-insert tb4 "bar") (make-textbox "foo bar" 7))
)

@larger{@bold{Ex 2}}: A @tt{Direction} is one of the strings @tt{"left"} or
@tt{"right"}. Design a function @tt{textbox-move} that, given a @tt{Textbox} and
a @tt{Direction} returns a new @tt{Textbox} with the cursor moved one character
to the left or right.

@larger{@bold{Ex 2.1}}: What does your @tt{textbox-move} do when the
@tt{Textbox} cursor is at 0? What about when it is equal to the length of its
content? Are you sure you're satisfying the @tt{Textbox} data definition?

@larger{@bold{Ex 3}}: Design a function @tt{textbox-backspace} that, given a
@tt{Textbox}, returns a new @tt{Textbox} with the last character before the
cursor removed and the cursor properly updated. If the cursor is at the
beginning of the @tt{Textbox}, @tt{textbox-backspace} should have no effect.

@larger{@bold{Ex 4}}: Design a function @tt{textbox-delete} that works similarly
to @tt{textbox-backspace}, but for the character directly after the cursor.


@section[#:style 'unnumbered #:tag "lab7:big-bang"]{A Working Textbox}

Swap @bold{Head} and @bold{Hands}!

We will use @racket[big-bang] to get our @tt{Textbox} up and running.

@larger{@bold{Ex 5}}: Using only @racket[big-bang] and the @racket[to-draw]
clause, display one of your example @tt{Textbox}es in a window.

Without a key-handler, our @tt{Textbox} is just about useless. The
@racket[on-key] clause to @racket[big-bang] will let us react to
@link["https://docs.racket-lang.org/teachpack/2htdpuniverse.html#%28tech._world._keyevent%29"]{@tt{KeyEvent}}s.

@larger{@bold{Ex 6}}: What is a
@link["https://docs.racket-lang.org/teachpack/2htdpuniverse.html#%28tech._world._keyevent%29"]{@tt{KeyEvent}}?
Is it atomic or composite data? Is it an enumeration? Write your answers in a
comment in the definitions window.

@larger{@bold{Ex 7}}: What keyboard key does the
@link["https://docs.racket-lang.org/teachpack/2htdpuniverse.html#%28tech._world._keyevent%29"]{@tt{KeyEvent}}
@tt{"a"} represent? What about @tt{"left"}, @tt{"\b"}, and @tt{"\u007F"}?

@larger{@bold{Ex 8}}: Design a function @tt{textbox-handle-key} that given a
@tt{Textbox} and a @tt{KeyEvent}, returns a new @tt{Textbox} modified in the
proper way.

Your key-handler should support inserting any lowercase character, number, or
space, as well as proper movement with left/right arrow keys, delete, and
backspace. The enter/return key should have no effect.
