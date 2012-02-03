#lang scribble/manual
@(require "../utils.rkt"
	  "../unnumbered.rkt"
          (for-label (except-in class/1 append reverse)))

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

@;{
@item{
Design a data representation for river systems (see the related
problem on p. 46 of How to Design Classes).  A @racket[River] ends in a
@racket[Mouth]. Above the @racket[Mouth], a @racket[River] will be formed by
the @racket[Confluence] of two other @racket[River]s.  A
@racket[River] starts at a @racket[Source].  

Design the method @racket[count-sources].  Then extend your data
definition to record how far it is along each river segment.  Design
the method @racket[river-length], which is the @emph{longest} distance
from any source of the river to the mouth of the river.
}
}

@item{
A football game consists of a series of Posessions.  Each Posession
starts at a location on the field (a yard from 0 to 100), ends at a
specific location on the field, either with a score (of either 3 or 7
points), and a kick to the other team, causing them to start at a new
location on the field.  

Design a data defintion for football games.  Then design the method
@racket[total-offense], which records the number of yards gained by
the team that @emph{started} with the ball first.  Design the method
@racket[total-score], which records the total number of points scored
by @emph{both} teams.
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
schedule is good for @racket[free-time].}  

@item{Develop data and class definitions for representing lists of
numbers.  Design the @racket[reverse] method that consumes no
arguments and produces the list of this list's elements in reverse
order.  You should design the method using a helper method with an
accumulator.  Once you have the helper method, make sure the
definition of @racket[reverse] is identical in all the classes that
represent lists, then define a new super class of these classes and
lift this duplicated, identical definition to it.  Thus you should now
have a single definition of @racket[reverse].

@bold{Note:} you may @emph{not} use lists from last semester, i.e. those
constructed with @racket[cons] and @racket[empty].}

@item{Revisit your solution to @racket[free-time].  Did you use an
accumulator desgin?  (You probably should have.)  If you didn't,
redesign the program with an accumulator.  Lift any identical method
definitions to super classes.}

@item{
Here is a data defintion for a @racket[Shape]:

@#reader scribble/comment-reader
(racketblock
;; A Shape is one of:
;; - (new circle% radius)
;; - (new rectangle% width height)
)

Define the appropriate classes, and design the methods @racket[size],
which computes the area of a shape, and @racket[draw], which takes a
@racket[Color] and produces and image of the shape in that color.

Extend your data defintion to support overlayed shapes, and add an
@racket[overlay] method to the interface that all @racket[Shape]s
support.  

Now extend your classes to support the @racket[underlay] method.  Can
you write this method just once?
}

@item{
Design a representation for a @tt{Population}.  This consists of
children, adults, and retired people.  In this society, everyone
becomes an adult at 25, retires at 65, and dies at 90.  Then, design
the following methods:
@itemlist[
@item{@tt{age-all}, which makes everyone a year older,}
@item{@tt{max-age}, which computes the age of the oldest member,}
@item{@tt{half-working}, which ages the population until there are
half as many adults as there were to start with.}
]
}

@item{
Design a representation for simple @emph{organic molecules}.  An
organic molecule is either a carbon atom, or a carbon atom connected
to additional carbon atoms via bonds.  A connection between two carbon
atoms can be either a single, double, or triple bond.  The total
number of bonds attached to a single carbon atom is at most 4.  

Define @tt{butane} as an example, which is just 4 carbons in a line,
with all single bonds.  You can see a diagram of butane
@link["http://en.wikipedia.org/wiki/Butane"]{here}. 

Design the method @tt{count-carbons}, which starting from a carbon
atom, counts all the carbon atoms connected to it (including the
starting atom).

Design the method @tt{valid?}, which determines if every attached
carbon atom has at most 4 bonds.

It turns out that all of the gaps, i.e. the possible bonds that aren't
connected to another carbon atom, are actually connected to hydrogen
atoms.  Design the method @tt{count-hydrogens} which counts the number
of hydrogen atoms reachable starting from a carbon atom (i.e., counts
all of the gaps).

}

]