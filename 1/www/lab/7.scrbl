#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")

@title[#:style 'unnumbered #:tag "lab7"]{Lab 7: Practicing with Self-Reference}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/beginner.html"]{Beginning Student
Language}. 

Make sure you follow
@link["https://cs.umd.edu/class/fall2017/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.


Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab7:self-reference"]{Self-referencing templates}

@larger{@bold{Ex 1}}:
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

@larger{@bold{Ex 2}}:
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
;; List should contain strings in the tree (you can decide in what order)
)

@larger{@bold{Ex 3}}:
Make templates for each of the following data definitions:

@#reader scribble/comment-reader (racketblock
;; A NestingDoll is one of
;; - empty
;; - (make-doll NestingDoll)
(define-struct doll (inside))

;; A RedDoll is one of
;; - empty
;; - (make-red-doll BlueDoll)

;; A BlueDoll is one of:
;; - (make-blue-doll RedDoll)
)

@larger{@bold{Ex 4}}:
Design the following functions:

@#reader scribble/comment-reader (racketblock
;; doll-depth : NestingDoll -> Natural
;; Count the nesting depth of the given doll

;; blue-depth : RedDoll -> Natural
;; Count the number of blue dolls within given red doll
)
