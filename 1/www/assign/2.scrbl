#lang scribble/manual
@(require scribble/core)
@(define (colorize c . content)
  (elem #:style (style #f (list (color-property c)))
        content))

@title[#:style 'unnumbered #:tag "assign2"]{Assignment 2: Simple functions}


@bold{Due}: Friday, September 8, 11:59:59 PM EST.

@(define @Piazza @link["http://piazza.com/umd/fall2017/cmsc131a"]{Piazza})

The following should be completed in cooperation with your assigned
partner from lab 1.  (Partner assignments are listed on @|Piazza|.)

@section[#:tag "assign2:htdp"]{HtDP exercises}

Complete HtDP exercises
@link["http://www.ccs.neu.edu/home/matthias/HtDP2e/part_one.html#%28counter._%28exercise._fun2%29%29"]{13}, 14, and 18--20.

@section[#:tag "assign2:qwerty"]{Render string}

Design the function @tt{render-string}, which consumes a
number @tt{t} and produces a text image of the first @tt{t}
letters from the string @racket["qwerty"].

Place the text on a white 200 x 100 rectangle. Use black
text of font size 22.

@section[#:tag "assign2:message"]{Add message}


Write the function @tt{add-message}. Its task is to create
an image that represents a sequence of chat messages.

The function consumes three items:

@itemlist[
 @item{a string, which represents the name(s) of the sender of the latest message;}
 @item{another string, which represents the latest message; and}
 @item{an image, which represents what has been said so far and by whom.}
 ]

The function produces an image that represents the complete
history, that is, the history combined with the latest
message.

There are no constraints on how you wish to represent
messages in an image as long as it clearly expresses who
said what.

Here is one possible usage scenario:

@racketblock[
(define history-1 empty-image)
(define history-2 (add-message "Matthias and Becca" "hello world" history-1))
(define history-3 (add-message "Ben and Alan" "hello, you guys" history-2))
(define history-4 (add-message "Leena and Nada" "good bye" history-3))
             ]

That is, the image argument to @tt{add-message} is likely to
be something that the function produced before, but it
doesn't have to be so.
