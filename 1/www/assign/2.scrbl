#lang scribble/manual
@(require scribble/core)
@(define (colorize c . content)
  (elem #:style (style #f (list (color-property c)))
        content))

@title[#:style 'unnumbered #:tag "assign2"]{Assignment 2: Simple functions}


@bold{Due}: Wednesday, September 12, 11:59:59 PM EST.

@(define @Piazza @link["http://piazza.com/umd/fall2018/cmsc131a"]{Piazza})

The following should be completed in cooperation with your assigned
partner from lab 1.  (Partner assignments are listed on @|Piazza|.)

@section[#:tag "assign2:htdp"]{HtDP exercises}

Complete HtDP exercises @link["https://htdp.org/2018-01-06/Book/part_one.html"]{11--20}.

@section[#:tag "assign2:niceday"]{Have a nice day!}

Design a function @tt{nice-day}, which consumes a number @tt{size} and
produces an yellow circle representing a face with two oval eyes and a
smile.  The radius of the circle should be @tt{size} pixels.

The result should look something like this:
@image[#:scale .8 "img/face.png"]

@section[#:tag "assign2:convert"]{Converting to Inches}

Write a function, @racket[convert-to-inches], that takes as arguments a number
of yards, feet, and inches (in that order) and converts to a total
number of inches. For example, @racket[(convert-to-inches 4 1 3)] would produce
@racket[159] inches.

@;{section[#:tag "assign2:message"]{Add message}


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
}

@section[#:tag "assign2:submit"]{Submission}

Add a comment to top of your file that lists both partner names, separated by a comma.

Submit a single file named @tt{assign2.rkt} to the submit server.

Only one partner needs to submit and should select their partner from
the drop-down menu when submitting.


