#lang scribble/base
@(require scribble/core 
          scribble/html-properties
          (for-label (only-in typed/racket define-type : U Listof require/typed)
                     (only-in racket define if > * define-struct module+)
                     rackunit
                     (only-in lang/htdp-intermediate-lambda check-expect)))
@(require scribble/manual)

@title[#:style 'unnumbered #:tag "assign11"]{Assignment 11: Yo++}

@bold{Due:} Monday, December 11, 11:59:59 PM EST.

@(define-syntax-rule (bslblock . body)
  (codeblock #:keep-lang-line? #f "#lang htdp/bsl" "\n" . body))

The following should be completed in cooperation with your latest
assigned partner, @bold{which were newly assigned on Oct 23}. (Partner
assignments are listed on
@link["https://piazza.com/class/j474gwnsd3619n?cid=294"]{Piazza}.)
You may not share code for this assignment with anyone but your
partner.

You must use the design recipe and @secref{style} guidelines to
receive full credit.

@section[#:tag "assign11:yo"]{Extended Yo}

This assignment deals with revisiting the Yo application developed in
class @secref{yo-lec}.

You may start from the code provided after the lecture, including a
complete implementation of the client, which was only partially
developed in lecture.

Here is a demo of the server and three clients running:

@elem[#:style 
      (style #f (list (alt-tag "video") 
		      (attributes 
		       '((width . "520")
                         (height . "540")
                         (controls . "")))))      
      @elem[#:style
	    (style #f
	      (list (alt-tag "source")
		    (attributes 
		     '((src . "yo.mp4")
		       (type . "video/mp4")))))]]

Your task is to extend the @bold{client} program to work more like a
traditional chat client, allowing the user to type any message, not
just "yo".

Add a text box to the bottom of the window that allows the user to
type a message, which is sent to the currently selected user when they
press the Enter key.  (You may want to revist some of the early
@secref{Labs} that dealt with text boxes.)

The client should have different modes for selecting a user and
selecting the text box to type (otherwise you won't be able to write
messages containing "1" for example).  It's up to you to design and
represent these modes.

Your program @bold{must} define two functions with exactly these names
and signatures:

@#reader scribble/comment-reader (racketblock
;; handle-receive : World SExpr -> HandlerResult
;; Receive a message from the server

;; handle-key : World KeyEvent -> HandlerResult
;; Receiver keyboard input from the user
)

We will use these functions to test your program.  They should work
for arbitrary interactions with the server and user.

You @emph{should} only need to modify the client; the server is not
actually dependent on the message being "yo".  When we test your
client, we will not use your server so if your client is dependent on
changes you make to the server, tests will likely fail.

@section{Project submission}

Submit a single file named @tt{chat-client.rkt} that implements the
revised client program.
