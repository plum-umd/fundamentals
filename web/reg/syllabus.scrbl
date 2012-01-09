#lang scribble/manual
@(require (only-in racket first rest make-list)
	  "../unnumbered.rkt"
	  "../utils.rkt")

@title*{Syllabus}

This syllabus will evolve throughout the semester so check it often.

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

@verbatim{
	1.	1/9	1/10	Review; inexact; accumulators (DrRacket all)
	2.	1/11, 17	1/18	Data, classses, unions; Eclipse intro
	3.	1/19, 23, 24	1/25	Complex data, methods conditionals, dispatch; inexact tests
	4.	1/25, 26, 30	1/31	Methods for complex data, World, Type checking errors
	5.	2/1, 2, 6	2/7	Abstract classes, constructors, exceptions, privacy; File read helper
	6.	2/8, 9, 13	2/14	Equality, Function objects, Singleton pattern?
	7.	2/15, 16	2/21	Circular data, methods, State change; fromStringData
	8.	2/22, 23, 27	2/28	More imperative methods, generics, designing to interfaces, getters, setters, predicates; (Lab: stacks, lists w. generics)
	9.	2/29, 3/1, 12	3/13	Traversals, Collections, Traversals with effects; ArrayList
	10.	3/14, 15, 19	3/20	Loops, ADT's, Visitors; (Lab: ArrayList, sorting in place)
	11.	3/21, 22, 37	3/28	Map, HashMap, Equality; Array
	12.	3/29, 30, 4/2	4/3	Big Oh and Algorithms (introduce some standard ones); (Lab: Time trials, more algorithms)
	13.	4/4, 5, 9	4/10	More algorithms (e.g.graph traversals, union/find, …); I/O, GUIs (Lab: GUI interactions, maybe more…)
	14.	4/11, 12	4/17, 18	Q&A: what does it all mean; Lab: project presentations
}


