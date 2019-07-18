#lang scribble/manual
@(require scribble/core 
          scribble/examples
          racket/sandbox
          (for-label lang/htdp-beginner) 
          (for-label (except-in 2htdp/image image?))
          ;"helper.rkt" 
	  "../utils.rkt"
          "../defns.rkt")

@title[#:style 'unnumbered #:tag "ex6"]{Exercise 6}

@bold{Due}: Thursday, July 18, 11:59:59 PM EST. 

@(define ex (make-exerciser "Problem"))

Implement these exercises with the
@link["https://docs.racket-lang.org/htdp-langs/beginner.html"]{Beginning
Student Language}.

@section[#:tag "ex6:submit"]{Directions for submitting}

Please read and follow these intructions carefully.  You should submit
a @bold{single} file named @tt{ex4.rkt} on @link[elms-url]{ELMS}.  You
may submit many times, but each submission should be one file called
@tt{ex4.rkt}.  You may lose points for not following these
instructions.

Make sure the top of the file contains the following, with your name filled in:
@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; Exercise 6
;; Name: ...your name here...
}|

@section[#:tag "ex6:overview"]{Oveview}

The goal of this exercise is to practice using the ``design recipe'' for
systematic problem solving with self-referential data (that are not lists).

@ex["Templates for self-referential data"]{
Make templates for each of the following data definitions:

@#reader scribble/comment-reader (racketblock
;; A Messages is one of:
;; - (make-no-msgs)
;; - (make-some-msgs String Messages)
(define-struct no-msgs ())
(define-struct some-msgs (m more))

;; A Mixages is one of:
;; - (make-last-msgs String)
;; - (make-some-msgs String Mixages)
(define-struct last-msgs (m))

;; A MsgTree is one of:
;; - (make-leaf)
;; - (make-node String MsgTree MsgTree)
(define-struct leaf ())
(define-struct node (v left right))

;; A MessageList is one of:
;; - empty
;; - (cons String MessageList)

;; A NonEmptyMessageList is one of:
;; - (cons String empty)
;; - (cons String NonEmptyMessageList)
)
}

@ex["Functions for self-referential data"]{

Design the following functions:

@#reader scribble/comment-reader (racketblock
;; count-messages : Messages -> Natural
;; Count the number of strings in the messages

;; count-mixages : Mixages -> Natural
;; Count the number of strings in the mixages

;; count-message-tree : MsgTree -> Natural
;; Count the number of strings in the message tree

;; count-message-list : MessageList -> Natural
;; Count the number of strings in the message list

;; count-non-empty-message-list : NonEmptyMessageList -> Natural
;; Count the number of strings in the non-empty message list

;; length-messages : Messages -> Natural
;; Count the total length of all the strings in the messages

;; length-message-tree : MsgTree -> Natural
;; Count the total length of all the strings in the message tree

;; messages->message-list : Messages -> MessageList
;; Convert the given messages into a message list

;; message-tree->message-list : MsgTree -> MessageList
;; Convert the given message into a message list
;; List should contain strings in the tree
;; (you can decide in what order)
)

}
