#lang scribble/manual
@(require scribble/core 
          scribble/examples
          racket/sandbox
          (for-label lang/htdp-beginner) 
          (for-label (except-in 2htdp/image image?))
          ;"helper.rkt" 
	  "../utils.rkt"
          "../defns.rkt")

@title[#:style 'unnumbered #:tag "ex4"]{Exercise 4}

@bold{Due}: Monday, July 14, 11:59:59 PM EST. 

@(define ex (make-exerciser "Problem"))

Implement these exercises with the
@link["https://docs.racket-lang.org/htdp-langs/beginner.html"]{Beginning
Student Language}.

@section[#:tag "ex4:submit"]{Directions for submitting}

Please read and follow these intructions carefully.  You should submit
a @bold{single} file named @tt{ex4.rkt} on @link[elms-url]{ELMS}.  You
may submit many times, but each submission should be one file called
@tt{ex4.rkt}.  You may lose points for not following these
instructions.

Make sure the top of the file contains the following, with your name filled in:
@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; Exercise 4
;; Name: ...your name here...
}|

@section[#:tag "ex4:overview"]{Oveview}

The goal of this exercise is to practice using the ``design recipe'' for
systematic problem solving with itemizations and design by composition.

In this exercise, you will develop functions for operating on payroll
records for nascent video game company you have founded.  There are
three kinds of employees in your company:

@itemlist[
@item{salaried workers: these employees make a fixed amount annually,}
@item{hourly workers: these employees make a fixed hourly rate for some number of
hours they work each week,}
@item{contract workers: these employees a fixed total amount for jobs
that take some number of months to complete.}
]

Each employee has a name and the ability to have a portion of each pay
check deducted pre-tax and placed in a retirement savings account.

Your company cuts paychecks on a biweekly basis (every 2 weeks).

We will make the following assumptions:

@itemlist[
@item{there are exactly 52 weeks in a year,}
@item{everybody pays a tax rate of 17%,}
@item{retirement savings are deducted before taxes are computed,}
@item{hourly workers get "time and a half" for any hours worked in excess of 40 (meaning they get
1.5 times their usual wage for any hours over 40 they work).}
]

@section{Death and Taxes}

After much thought, you and your HR department have come up with the
following data definition to represent information need to do payroll
every 2 weeks:

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; An Employee is a:
;; (make-employee String PayRate Number)
;; Interpretation: an employee has a name, a pay-rate (see below), and
;; a percentage (between 0 and 1) to be deducted for retirement savings
(define-struct employee (name pay retire))

;; A PayRate is one of:
;; - (make-salary Number)
;; - (make-wage Number Number)
;; - (make-contract Number Natural)
(define-struct salary (amt))
(define-struct wage (rate hours))
(define-struct contract (total months))
;; Interpretation:
;; - annual salary in dollars
;; - hourly wage in dollars and weekly number of hours
;; - contract worker making fixed amount over some period of (whole) months


;; Examples:

(make-employee "Wile" (make-salary 100000) .07)
;; Wile is a salaried employee, making $100K/year, saving 7% for retirement

(make-employee "Bugs" (make-wage 15 40) .01)
;; Bugs is an hourly employee, making $15/hour, saving 1% for retirement

(make-employee "Goofy" (make-contract 16000 3) .05)
;; Goofy is a contractor for a $16K job over 3 months, saving 5% for retirement
}|

With these definitions in place, design the following functions.  For
each design, be sure to complete each step of the design process.


@ex[@racket[biweekly-gross]]{

Design the function @racket[biweekly-gross] which computes a given
employee's gross pay for a two week period.  (Gross pay means before
taxes or retirement savings are deducted.)

}

@ex[@racket[biweekly-savings]]{

Design the function @racket[biweekly-savings] which computes a given
employee's retirement savings for a two week period.

}

@ex[@racket[biweekly-tax]]{

Design the function @racket[biweekly-tax] which computes a given
employee's amount of taxes for a two week period.

}

@ex[@racket[biweekly-net]]{

Design the function @racket[biweekly-net] which computes a given
employee's net pay for a two week period. (Net pay means after taxes
and retirement savings are deducted; this is the amount an employee
"takes home.")

}

