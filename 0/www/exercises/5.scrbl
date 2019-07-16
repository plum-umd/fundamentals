#lang scribble/manual
@(require scribble/core 
          scribble/examples
          racket/sandbox
          (for-label lang/htdp-beginner) 
          (for-label (except-in 2htdp/image image?))
          ;"helper.rkt" 
	  "../utils.rkt"
          "../defns.rkt")

@title[#:style 'unnumbered #:tag "ex5"]{Exercise 5}

@bold{Due}: Wednesday, July 16, 11:59:59 PM EST. 

@(define ex (make-exerciser "Problem"))

Implement these exercises with the
@link["https://docs.racket-lang.org/htdp-langs/beginner.html"]{Beginning
Student Language}.

@section[#:tag "ex5:submit"]{Directions for submitting}

Please read and follow these intructions carefully.  You should submit
a @bold{single} file named @tt{ex4.rkt} on @link[elms-url]{ELMS}.  You
may submit many times, but each submission should be one file called
@tt{ex4.rkt}.  You may lose points for not following these
instructions.

Make sure the top of the file contains the following, with your name filled in:
@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; Exercise 5
;; Name: ...your name here...
}|

@section[#:tag "ex5:overview"]{Oveview}

The goal of this exercise is to practice using the ``design recipe'' for
systematic problem solving with lists.

In this exercise, you will develop functions for operating on
arbitrarily large collections of data.  There are many problems, but
practicing the design process will help make them go quickly.

@section[#:tag "ex:lon"]{Lists of numbers}

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; A LoN (list of numbers) is one of:
;; - '()
;; - (cons Number LoN)
}|

@ex[@racket[lon-count : LoN -> Natural]]{

Count the number of elements in a given list of numbers.

}

@ex[@racket[lon-product : LoN -> Number]]{

Compute the product of the given list of numbers (multiply all numbers
together).

}

@ex[@racket[lon-add1-all : LoN -> LoN]]{

Add one to every element of a given list and collect the results as a
list.

}

@ex[@racket[lon-abs-all : LoN -> LoN]]{

Compute the absolute value of every element of a given list and
collect the results as a list.

}

@ex[@racket[lon-filter-even : LoN -> LoN]]{

Produce a list that contains only the even elements of a given list.

}

@ex[@racket[lon-all-odd? : LoN -> Boolean]]{

Determine if every element of a given list is odd.

}


@section[#:tag "ex5:los"]{List of strings}


@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; A LoS (list of strings) is one of:
;; - '()
;; - (cons String LoS)
}|

@ex[@racket[los-count : LoS -> Natural]]{

Count the number of elements in a given list of strings.

}

@ex[@racket[los-string-append : LoS -> String]]{

Append together all the strings in a given list of strings.

}

@ex[@racket[los-upcase-all : LoS -> LoS]]{

Capitalize every string in a given list and collect the results as a
list. (Hint: @racket[string-upcase].)

}

@ex[@racket[los-length-all : LoS -> LoN]]{

Compute the length of every string in a given list and
collect the results as a list (of numbers).

}

@ex[@racket[los-filter-contains-e : LoS -> LoS]]{

Produce a list that contains only the elements of a given list that
contain the letter @racket["e"]. (Hint: @racket[string-contains?].)

}

@ex[@racket[los-any-whitespace? : LoS -> Boolean]]{

Determine if any element of a given list is whitespace. (Hint:
@racket[string-whitespace?].)

}

@section[#:tag "ex:lop"]{Lists of positions}

@codeblock[#:keep-lang-line? #false]|{
#lang racket
;; A LoP (list of positions) is one of:
;; - '()
;; - (cons Posn LoP)

;; A Posn is a (make-posn Number Number)
}|

@ex[@racket[lop-count : LoP -> Natural]]{

Count the number of elements in a given list of positions.

}

@ex[@racket[lop-dist-sum : LoP -> Number]]{

Compute the sum of each position's distance to the origin in a given list.

}

@ex[@racket[lop-dist-all : LoP -> LoN]]{

Compute a list of distances to the origin for a given list of positions.

}

@ex[@racket[lop-change-x : LoP Number -> LoP]]{

Change each position's x-coordinate by given amount.

}

@ex[@racket[lop-remove-y-bound : LoP Number -> LoP]]{

Produce a list that contains only the elements whose y-coordinate is
between 0 (inclusive) and the given number (inclusive).

}

@ex[@racket[lop-any-equal? : LoP Posn -> Boolean]]{

Determine if any element of a given list is the same as given position.

}

