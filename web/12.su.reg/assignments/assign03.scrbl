#lang scribble/manual
@(require "../utils.rkt")

@title[#:tag "assign03"]{5/22: XML}

Due: 5/22, midnight by svn.

Language: Java.

You must complete this assignment with your partner and should not
discuss solutions with anyone but your partner and the course staff.

This assignment is due Tuesday at midnight to give you some more time
to complete it.

@section{Follow-up from Lab}

Complete exercises 5-15 from @seclink["lab03"]{Lab 3}.

You may use the following (ISL) data definition that we worked out in
class to guide your solution if its helpful, but any adequate data
definition is acceptable.

@verbatim{
;; An XML is one of:
;; - empty
;; - (cons XMLFrag XML)
;; An XMLFrag is one of:
;; - Plaintext (String)
;; - (make-tagged Tag XML)
;; A Tag is a (make-tag Name Atts)
;; An Atts is one of:
;; - empty
;; - (cons Att Atts)
;; An Att is a (make-att String String)
(define-struct tagged (tag content))
(define-struct tag (name atts))
(define-struct att (name value))
}

@section{Book problems}

Complete problems 12.1, 12.3, 12.4, 12.5, 15.1 (no need to turn in
class diagrams), 15.2, 15.3, 15.4.

