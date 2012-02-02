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

]