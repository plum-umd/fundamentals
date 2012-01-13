#lang scribble/manual
@(require (only-in racket first rest make-list)
	  "unnumbered.rkt"
	  "utils.rkt")

@title*{Syllabus}

This syllabus will evolve throughout the semester so check it often.

@(define (number->string2 n)
   (let ((r (number->string n)))
     (cond [(= (string-length r) 1)
            (string-append "0" r)]
           [else r])))

@(define WEEK-REVEALED (if-internal 15 1))

@(define assign-dates
   (list "1/11"
	 "1/18"
	 "1/25"
	 "2/1"
	 "2/8"
	 "2/15"
	 "2/22"
	 "Spring break"
	 "3/7"
	 "3/14"
	 "3/21"
	 "3/28"
	 "4/4"
	 "4/11"
	 "4/28"))

@(define lab-dates
   (list "1/9"
	 "1/16 MLK"
	 "1/23"
	 "1/30"
	 "2/6"
	 "2/13"
	 "2/20 Presidents"
	 "2/27 Spring break"
	 "3/5"
	 "3/12"
	 "3/19"
	 "3/26"
	 "4/2"
	 "4/9"
	 "4/16 Patriots"))
	 

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
   (append (list "DPC: Ch. 1; HtDC: Ch. 1, 2, 10."
		 "DPC: Ch. 2; HtDC: Ch. 3, 4, 5."
		 "DPC: Ch. 3, 4; HtDC: Ch. 6, 11, 12."
		 "DPC: Ch. 5, 6; Universe docs."
		 "DPC: Ch: 7, 8; HtDC: Sec. 20."
		 "DPC: Ch: 9, 10; HtDP: Part VII.")
	   (make-list 9 "??")))	 

@(tabular (apply list 
                 (list "" "Lectures " "Readings " "Lab " "Assignment")
                 (list "1"    
		       (itemlist (item (secref "lec01")))
                       (first reading-list)
		       (secref "lab01")
		       (secref "assign01"))
                 (for/list ([i (in-range 2 16)]
                            [r (in-list (rest reading-list))])                   
                   (syllabus-row i r))))
