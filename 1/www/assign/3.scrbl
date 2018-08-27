#lang scribble/manual

@title[#:style 'unnumbered #:tag "assign3"]{Assignment 3: Designing and composing functions}

@bold{Due:} Friday, September 15, 11:59:59 PM EST.

The following should be completed in cooperation with your assigned
partner from lab 1. (Partner assignments are listed on Piazza.)

This is the first assignment where you must use the design recipe to
receive full credit.  You must also follow @secref{style}
guidelines.

@section{Preparation}

In your @link["https://htdp.org/2018-01-06/Book/"]{textbook} turn to 
@link["https://htdp.org/2018-01-06/Book/part_one.html"]{Fixed-Sized Data} chapter.

In Functions and Programs read first @link["https://htdp.org/2018-01-06/Book/part_one.html#%28part._sec~3afuncs%29"]{Functions} then
@link["https://htdp.org/2018-01-06/Book/part_one.html#%28part._sec~3acomposing%29"]{Composing Functions}.

You should also be familiar with using the design recipe
with two kinds of data definition, Enumerations and
Intervals.

@section{@tt{Year}, @tt{Month} and @tt{Day}}

You will write functions that make use of dates as
conventionally written in English. The following data
definitions will be used throughout the assignment, and more
will be introduced as they are needed.

@codeblock{
 ; A Year is a non-negative integer

 ; A Month is one of:
 ; - "January"
 ; - "February"
 ; - "March"
 ; - "April"
 ; - "May"
 ; - "June"
 ; - "July"
 ; - "August"
 ; - "September"
 ; - "October"
 ; - "November"
 ; - "December"

 ; A Day is an integer in the range 1 to 31 inclusive
}

Note that in general, it will be your job to create the data
definitions. However, for this assignment we will provide
the data definitions so that you may focus on the subsequent
steps in the design recipe.

@section{@tt{calendar}}

The goal of this section is to write a program which allows
a user armed with relevant information about a date to
generate an image displaying that date.

Analyzing this goal, we realize that written dates have a
format and a choice of format needs to be made along the
way. This decision gave rise to two additional data
definitions:

@codeblock{
; A MonthFormat is one of:
; - "long"
; - "short"

; A DateOrder is one of:
; - "MDY"
; - "DMY"
}

We will provide additionally the next 1-2 steps of the
design recipe. In this assignment, most of your work will be
in carrying out steps 4 and 5. (And what are these?)

@codeblock{
; calendar : Year Month Day -> Image
; returns an image of a date on a background
}

Now, notice that @tt{calendar} would need carry out a number
of operations to transform its input into an image. And more
importantly, some of these operations have yet to be
defined. Defining them all at once in the body of @tt{
 calendar} would result at best in a complicated function
definition; at worst, in a broken or buggy definition which
is hard to improve.

So instead we write helper functions which will be called in
the body of @tt{calendar}.

@codeblock{
; format-month : Month MonthFormat -> String
; abbreviates Month to three letters or not
(define (format-month m f) ...)

(check-expect (format-month "November" "long") "November")
(check-expect (format-month "November" "short") "Nov")
}

Use @tt{format-month} to define the following:

@codeblock{
; year-month-day->date : Year Month Day DateOrder MonthFormat -> String
; produces a date as a string
; given: 1936 "November" 12 "MDY" "long"   expect: "November 12, 1936"
; given: 1936 "November" 12 "MDY" "short"  expect: "Nov 12, 1936"
; given: 1936 "November" 12 "DMY" "long"   expect: "12 November 1936"
; given: 1936 "November" 12 "DMY" "short"  expect: "12 Nov 1936"
(define (year-month-day->date y m d o f) ...)
}

For @tt{year-month-day->date} and the function stubs below,
be sure to write unit tests with @tt{check-expect} using the
examples given (as well as any other examples you see fit to
devise).

Once you have defined @tt{year-month-day->date}, make a choice of date format
and define @tt{calendar}.

(If you are looking for some visual inspiration, do an image
search online for the Modernist painter On Kawara.)

@section{@tt{days-between}}

The goal of this section is to write a program which enables
its user to calculate the number of days between two dates.
As above, it would be prudent to decompose this task into
subtasks to be carried out by helper functions.

To simplify, we assume that a year has 365 days.

Analyzing this problem, we settled on the following
approach. Given a date, we calculate how many days have
elapsed since the earliest possible date (according to our
definitions above): 1 Jan, 0. Let's call this function
@tt{year-month-day->days}.
@codeblock{
; year-month-day->days : Year Month Day -> Number
; returns the number of days elapsed since January 1, 0
; given: 0 "January" 1     expect: 0
; given: 2017 "August" 28  expect: 736444
}
Notice that @tt{year-month-day->days} itself is not
completely straightforward, and this is due to the fact that
there is no standard month length. Let us then tackle the
number of days corresponding to a given month first.
@codeblock{
; month->days-in-year : Month -> Number
; returns the days elapsed in the year
; given: "January"    expect: 0
; given: "September"  expect: 243
}
Hint: your function template should make significant use of the
Month data definition.

With @tt{
 year-month-day->days} in hand, it's a matter of basic
arithmetic to compute the difference in days between two
dates. Since @tt{days-between} should return a non-negative integer,
consider using the @tt{abs} function.
@codeblock{
; days-between : Year Month Day Year Month Day -> Number
}

Be sure to complete the purpose statement and devise
examples/tests for @tt{days-between}.

@section{@tt{time-passing}}

In this section, the goal is to write a program which
produces a simple animation of @tt{calendar}. With each tick
of @tt{animate}, a day will pass. Lastly, the animation
will be initialized to a date of your choosing.

Take a moment to reflect on this problem and where the
difficulty lies. In a sense, we would like to feed @tt{
 calendar} to @tt{animate}, relax and watch the show.
However, @tt{calendar} takes three arguments and none of
these is the number of days elapsed since 1 Jan, 0000.

So we cannot simply compose @tt{animate} and @tt{calendar}.
But we can write functions which allow us to connect the
two. To start with, we will write a function @tt{
 days->year}:

@codeblock{
; days->year : Number -> Year
; takes days since 1 Jan 0 and returns the year
; given: 364                                       expect: 0
; given: 365                                       expect: 1
; given: 736305                                    expect: 2017
; given: (year-month-day->days 1999 "December" 31) expect: 1999
}

Notice that the last example is a witness to an important
property of @tt{days->year}: taken together with analogous
functions into Month and into Day, we would have constructed
a (left) @emph{inverse} to @tt{year-month-day->days}.

Also notice that @tt{days->year} can be used to
supply the Year argument to calendar.

Although it is now clear that we want @tt{days->month} and
@tt{days->day}, these are slightly more challenging. Again we
need to contend with the variability in month length. To
this end, we provide the following Intervals:
@codeblock{
; DaysInYear is an integer falling into one of 12 intervals:
; - [0,31)
; - [31,59)
; - [59,90)
; - [90,120)
; - [120,151)
; - [151,181)
; - [181,212)
; - [212,243)
; - [243,273)
; - [273,304)
; - [304,334)
; - [334,365)
; interpretation: the number of elapsed days
;                 since the first day of the year

; DaysInMonth is an integer in [0,31)
; interpretation: the number of elapsed days
;                 since the first of the month
}

Both @tt{days->month} and @tt{days->day} should be defined
using helpers.

@codeblock{
; days-in-year->month : DaysInYear -> Month
; takes days since the first of the year and returns the month
; given: 0    expect: "January"
; given: 31   expect: "February"
; given: 242  expect: "August"

; days->month : Number -> Month
; takes days since 1 Jan 0 and returns the month
; given: 59                                        expect: "March"
; given: 364                                       expect: "December"
; given: 736445                                    expect: "August"
; given: (year-month-day->days 1999 "December" 31) expect: "December"
}
Hint: Be sure that your template for @tt{days-in-year->month}
makes use of the DaysInYear data definition.

@codeblock{
; days-in-year->days-in-month : DaysInYear -> DaysInMonth
; takes days since the first of the year
; and returns days since the first of the month
; given: 0       expect: 0
; given: 59      expect: 0
; given: 364     expect: 30

; days->day : Number -> Day
; takes days since 1 Jan 0 and returns the day of the month
; given: 0                                         expect: 1
; given: 59                                        expect: 1
; given: 736324                                    expect: 30
; given: (year-month-day->days 1999 "December" 31) expect: 31
}

Hint: Although the input is an Enumeration, it is not
necessary to use @tt{cond} to write @tt{
 days-in-year->days-in-month}. Instead, define a composite
function with @tt{days-in-year->month} and @tt{month->days-in-year}.

Finally, we are ready to build our animation.

@codeblock{
(define init-year ...)
(define init-month ...)
(define init-day ...)

; init-time : Number
; days since init-month init-day, init-year
; given:  init-month := "December"
;         init-year := "1999"
;         init-day := 31
; expect: 729999
(define init-time ...)

; time-passing : Number -> Image
; takes days t since 1 Jan 0, advances t by init-time
; and returns a calendar image of the corresponding date
(define (time-passing t) ...)
}

Your final program should produce a running animation.
