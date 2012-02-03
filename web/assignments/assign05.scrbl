#lang scribble/manual
@(require "../utils.rkt"
	  "../unnumbered.rkt"
          (for-label (except-in class/1 append)))

@title[#:tag "assign05"]{2/7: Fundamentals}

Due: 2/7.

Language: @racketmodname[class/1].

In this assignment, you will practice designing solutions to several
small problems.

@itemlist[#:style 'ordered 

@item{Here is an example of a
@link["http://en.wikipedia.org/wiki/Mobile_%28sculpture%29"]{mobile}:
@verbatim{
                     |
          - - - - - - - - - - - -
         |                       |
        60                       |
       blue           - - - - - - - - - -
                     |                   |
                     |                  40
                  - - - - - -           red
                 |           |
                10           |
               green         5
                            red

}

The number of dashes in the struts and lines represents their length,
while the numbers and colors represent the weight and color of hanging
objects.

Design data definitions for representing hanging mobiles.  Make an
example of mobile data that represents the example given above.

Design the method @racket[total-weight] that computes the total
weight of a mobile. The weight of the lines and struts is given by
their lengths (a strut of length @racket[n] has weight @emph{n}).

Design the method @racket[height] that computes the height of the
mobile.} 

@item{ Here is a @link["Employees.pdf"]{class diagram} for employees.
Develop data and class definitions for employees.

Design the method @racket[count-subs] that computes the total number
of subordinates of this employee.

Design the method @racket[full-unit] that collects all of the
subordinates of this employee.  @emph{Hint:} you may find it useful to
add an @racket[append] method to the @racket[ILoE] interface (and
implement it).

Design the method @racket[has-peon?] that determines if this employee
has a peon with the given name (a string).
}

@item{Here is a data definition for a time of day:

@#reader scribble/comment-reader
(racketblock
;; A Time is a (new time% Hours Mins)
;; where Hours is an Integer in [0,24)
;;   and  Mins is an Integer in [0,60)
)

Develop a class definition for @racket[time%]. Design the method
@racket[->minutes] which converts this time to the number of minutes
since midnight. 

Here is a data definition for an event:

@#reader scribble/comment-reader
(racketblock
;; An Event is a (new event% String Time Time)
;; Interp: an event with a name and a start and end time.
)

Develop a class definition for @racket[event%].  Design the method
@racket[duration] which computes the duration of this event in
minutes.  Design the method @racket[ends-before?] which determines if
this events ends before a given event starts.  Design the method
@racket[overlap?] which determines whether this event overlaps with a given
event.

Here is a data definition for a schedule:

@#reader scribble/comment-reader
(racketblock
;; A Schedule is one of:
;; - (new no-event%)
;; - (new cons-event% Event Schedule)
)

Design the method @racket[good?], which determines if the events in
this schedule are in order and do not overlap.  Design the method
@racket[scheduled-time], which computes the total time in minutes
scheduled in this schedule.  You may assume this schedule is good for
@racket[scheduled-time].  Design the method @racket[free-time], which
computes a schedule of events named @racket["Free time"] that are all
times @emph{not} scheduled in this schedule.  You may assume this
schedule is good for @racket[free-time].}  ]