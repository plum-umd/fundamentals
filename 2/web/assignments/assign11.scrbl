#lang scribble/manual
@(require "../utils.rkt"
	  "../unnumbered.rkt"
          (for-label (except-in class/2 empty cons first rest list-ref length e check-expect))
          (for-label (only-in lang/htdp-intermediate-lambda check-expect))
	  (for-label class/universe))

@title[#:tag "assign11"]{3/20: Quick Lists in Java}

Due: 3/20.  Language: Java.

Re-develop solutions for the quick list portion of assignment 5 and
all of assignment 8 in Java.