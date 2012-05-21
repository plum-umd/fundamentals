#lang scribble/manual
@(require (only-in racket first rest make-list)
	  "../unnumbered.rkt"
	  "../utils.rkt")

@title*{Syllabus}

This syllabus will evolve throughout the semester so check it often.

@local-table-of-contents[]

@(define (number->string2 n)
   (let ((r (number->string n)))
     (cond [(= (string-length r) 1)
            (string-append "0" r)]
           [else r])))

@(define WEEK-REVEALED (if-internal 15 10))

@(define assign-dates
   (list "1/12"
	 "1/19"
	 "1/26"
	 "2/2"
	 "2/9"
	 "2/18 (Non-standard day)"
	 "2/25 (Non-standard day)"
	 "3/2 Spring break"
	 "3/9"
	 "3/16"
	 "3/23"
	 "3/30"
	 "4/6"
	 "4/13"
	 "4/20"))

@(define lab-dates
   (list "1/10"
	 "1/17 MLK"
	 "1/24"
	 "1/31"
	 "2/7"
	 "2/14"
	 "2/21 Presidents"
	 "2/28 Spring break"
	 "3/7"
	 "3/14"
	 "3/21"
	 "3/28"
	 "4/4"
	 "4/11"
	 "4/18 Patriots"))
	 

@(define (syllabus-row i reading)
   (list (number->string i)
         (itemlist (item (secref (string-append "lec" (number->string2 (- (* 2 i) 2)))))
                   (item (secref (string-append "lec" (number->string2 (- (* 2 i) 1))))))
         reading
         (if (<= i WEEK-REVEALED)
	     (secref (string-append "lab" (number->string2 i)))
	     (list-ref lab-dates (sub1 i)))
	 (if (<= i (add1 WEEK-REVEALED))
	     (secref (string-append "assign" (number->string2 i)))
	     (list-ref assign-dates (sub1 i)))))

@(define reading-list
   (append (list "HtDC: Ch. 1, 2, 10."
		 "HtDC: Ch. 3, 4, 5."
		 "HtDC: Ch. 6, 11, 12."
		 "Universe docs."
		 "HtDC: Sec. 20."
		 "HtDP: Part VII.")
	   (make-list 9 "??")))

@section*{Week 1: 5/7-10}

Read Part I of @emph{HtDC} and the New York Times article about
Jacques Pépin on the @secref{Blog}.

@subsection*[#:tag "w1m"]{Monday}

@itemlist[
@item{Administration, Overview, Review}
@item{@seclink["assign01"]{Assignment 1} due}]

@subsection*[#:tag "w1t"]{Tuesday}

@itemlist[
@item{Data Definitions and Methods}
@item{@seclink["lab01"]{Lab 1}}]

@subsection*{Wednesday}

@itemlist[
@item{Recursive Unions and their Methods}]

@subsection*[#:tag "w1r"]{Thursday}

@itemlist[
@item{@link["exam1.pdf"]{Exam 1}}
@item{@seclink["lab02"]{Lab 2}}]

@section*{Week 2: 5/14-17}

Read Part II of @emph{HtDC}.

@subsection*[#:tag "w2m"]{Monday}

@itemlist[
@item{More Methods for Unions: Binary trees}
@item{Abstracting Identical Data & Methods}
]

@subsection*[#:tag "w2t"]{Tuesday}

@itemlist[
@item{Lists and Sorting}
@item{@seclink["assign02"]{Assignment 2} due}
@item{@seclink["lab03"]{Lab 3}}]

@subsection*[#:tag "w2w"]{Wednesday}

@itemlist[
@item{Abstraction with Function Objects}
@item{Parameterized data definitions}]

@subsection*[#:tag "w2r"]{Thursday}

@itemlist[
@item{@link["exam2.pdf"]{Exam 2}}
@item{@seclink["lab04"]{Lab 4}}]

@section*{Week 3: 5/21-24}

@subsection*[#:tag "w3m"]{Monday}

@itemlist[
@item{Sorting with comparisons}
@item{Type-specific computations over parametric data}]

@subsection*{Tuesday}

@itemlist[
@item{Visitors}
@item{Lab 5}
@item{@seclink["assign03"]{Assignment 3} due}]

@subsection*{Wednesday}

@subsection*{Thursday}

@itemlist[
@item{Exam 3}
@item{Lab 6}]

@;{

@section*{Week 4: 5/28-31}

@subsection*{Monday}

@itemlist[
@item{Assignment 4 due}]

@subsection*{Tuesday}

@itemlist[
@item{Lab 7}]

@subsection*{Wednesday}
@subsection*{Thursday}

@itemlist[
@item{Exam 4}
@item{Lab 8}]

@section*{Week 5: 6/4-7}

@subsection*{Monday}

@itemlist[
@item{Assignment 5 due}]

@subsection*{Tuesday}

@itemlist[
@item{Lab 9}]

@subsection*{Wednesday}
@subsection*{Thursday}

@itemlist[
@item{Exam 5}
@item{Lab 10}]

@section*{Week 6: 6/11-14}

@subsection*{Monday}

@itemlist[
@item{Assignment 6 due}]

@subsection*{Tuesday}

@itemlist[
@item{Lab 11}]

@subsection*{Wednesday}

@subsection*{Thursday}

@itemlist[
@item{Exam 6}
@item{Lab 12}]

@section*{Week 7: 6/18-21}

@subsection*{Monday}

@itemlist[
@item{Assignment 7 due}]

@subsection*{Tuesday}

@itemlist[
@item{Lab 13}]

@subsection*{Wednesday}

@subsection*{Thursday}

@itemlist[
@item{Exam 7}
@item{Lab 14}]

@section*{Week 8: 5/18-21}

Final project due.

}