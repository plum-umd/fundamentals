#lang scribble/manual
@(require racket/list scribble/decode scribble/core scribble/html-properties)
@(require "utils.rkt")

@title[#:style 'unnumbered]{Schedule}

With the exception of midterms and exams, this schedule is subject to
change as the semester progresses.

@(define (augment-week week last-numbered-lec)
  (foldl
   (λ(entry acc)
     (cond
       [(string? (first entry))
        (cons entry acc)]
       [(and (= (length entry) 2) (number? (first entry)))
        (let* ((numbers-only
                (dropf acc (λ(acc-entry) (not (number? (last acc-entry))))))
               (last-numbered-lec (first numbers-only))
               (lecnum (+ 1 (last last-numbered-lec))))
          (cons (append entry (list lecnum)) acc))]
       [(= (length entry) 3)
        (cons entry acc)]
       [(eq? (first entry) 'labs)
        (cons entry acc)]
       [(eq? (first entry) 'exam)
        (cons entry acc)]
       [else
        (error 'huh entry)]))
   `((dummy dummy ,last-numbered-lec))
   week))
@(define (augment-calendar cal)
  (define augmented
    (foldl
     (λ(week acc)
       (let* ((numbers-only
               (dropf (apply append acc)
                      (λ(acc-entry) (not (number? (last acc-entry))))))
              (last-numbered-lec (first numbers-only))
              (lecnum (last last-numbered-lec)))
         (cons (augment-week week lecnum) acc)))
     '(((dummy dummy 0)))
     cal))
  (rest (reverse (map (λ(w) (rest (reverse w))) augmented))))

@(define (render-day entry syllabus labs)
   (cond
     [(number? (first entry))
      (let* ((month (first entry))
             (day (second entry))
             (lec (third entry)))
        (if (string? lec)
            (para #:style "boxed" 
                  (bold (number->string month) "/" (number->string day) ": " lec)
                  (indented-span (hspace 1)))
            (para #:style "boxed"
                  (bold (number->string month) "/" (number->string day) ": " (lecref (number->string lec)))
                  (indented-span (rest (assoc lec syllabus))))))]
     [(string? (first entry))
      (para #:style "boxed" (first entry))]
     [(eq? (first entry) 'labs)
      (if (> (length entry) 1)
          (para #:style "boxed"
                (add-between 
                 (map (λ(lab)
                        (let ((labcontent (assoc lab labs)))
                          (if labcontent
                              (list (labref lab) 
                                    (indented-span (rest (assoc lab labs))))
                              lab)))
                      (rest entry))
                 (linebreak)))
          (list))]
     [(eq? (first entry) 'exam)
      (para #:style "boxed" (add-between (rest entry) (linebreak)))]
     [else
      (error 'what entry)]
       ))
@(define (render-week syllabus labs)
   (lambda (entries n)
     (list
      (section #:style 'unnumbered "Week " (number->string n))
      (indented (map (lambda(d) (render-day d syllabus labs)) entries)))))

@(define (render-cal cal syllabus labs)
   (let ((aug-calendar (augment-calendar cal)))
     (map (render-week syllabus labs) aug-calendar (range 1 (+ 1 (length aug-calendar))))))



@(define calendar
   '(((1 28) (labs "1") (1 30) (labs "2") (2 1))
     ((2 4) (labs "3") (2 6) (labs "4") (2 8))
     ((2 11) (labs "5") (2 13) (labs "6") (2 15))
     ((2 18) (labs "7") (2 20) (labs "8") (2 22))
     ((2 25) (labs "9") (2 27) (labs "10") (3 1))
     ((3 4) (labs "11") (3 6) (labs "12") (3 8 "Midterm 1"))
     ((3 11) (labs "13") (3 13) (labs "14") (3 15))
     ((3 18  "No Class: Spring break")
      (3 20  "No Class: Spring break")
      (3 22  "No Class: Spring break"))
     ((3 25) (labs "15") (3 27) (labs "16") (3 29))
     ((4 1) (labs "17") (4 3) (labs "18") (4 5))
     ((4 8) (labs "19") (4 10) (labs "20") (4 12))
     ((4 15) (labs "21") (4 17) (labs "22") (4 19 "Midterm 2"))
     ((4 22) (labs "23") (4 24) (4 26))
     ((4 29) (5 1) (5 3))
     ((5 6) (5 8) (5 10))
     ((5 13))    
))

@(define syllabus
   `(;(1  "Designing Data: Simple classes, classes with containment")
     ;(2  "Designing unions of classes; self-reference")
     ;(3  "")
     ,@(build-list 43 (λ (i) (list i "")))
     #|
     (4  "Methods for unions of classes and classes with self-reference")
     (5  "Designing methods for complex class hierarchies")
     (6  "More complicated methods for self-referential data")
     (7  "(continued) Methods with accumulators")
     (8  "Recap: designing methods and wish lists for a larger problem")
     (9  "Abstract classes and inheritance")
     (10 "Customizing constructors for correctness and convenience")
     (11 "Sameness of data values")
     (12 "More of the same")
     (13 "Overview of world programming; Function objects")
     (14 "More function objects")
     (15 "Generics")
     (16 "Visitors")
     (17 "Creating cyclic data, mutation")
     (18 "Testing mutation methods, indirect cycles")
     (19 "Aliasing and equality, revisited")
     (20 "Removing items from lists, mutable lists")
     (21 "Direct-access data structures: " ,(tt "ArrayList") "; swapping two items, mapping over ArrayLists, for-each loops")
     (22 "ArrayLists and binary search; for-each loops; working with indices")
     (23 "For-each loops and counted-for loops")
     (24 "While-loops")
     (25 "Iterators and Iterables")
     (26 "HashMap, equals")
     (27 "Big Oh: searching and sorting; insertion sort and selection sort")
     (28 "Big Oh: searching and sorting; quicksort and merge sort")
     (29 "Big Oh: searching and sorting; HeapSort -- Priority Queue")
     (30 "Graph algorithms: breadth-first search, depth-first search")
     (31 "Graph algorithms: Dijkstra's algorithm, and comparison with BFS/DFS")
     (32 "Minimum spanning trees: Prim's and Kruskal's algorithms")   
     (33 "Design choices in object-oriented languages: JavaScript")
     (34 "Implementing Objects")
     (35 "Other languages and wrapup")
     (36 "Slack")
     (37 "Slack")
     (38 "Slack")
     (39 "Slack")
     (40 "Slack")
     (41 "Conclusions and perspectives")|#
     ))

@(define labs
   `(("1"   "Simple Data definitions and methods in Class")
     ("2"   "Designing methods in Java: classes, containment, unions, self-reference")    
     (dummy "Cancelled because of snow")
     ("3"   "Designing methods for self-referential data")
     ("4"   "Practice with abstract classes and constructors")
     ("5"   "Functional world games")
     ("6"   "Working with function objects")
     ("7"   "Generics and Visitors")
     ("8"   "Working with mutable and cyclic data")
     ("9"   "Loyd's 15 puzzle: working with " ,(tt "ArrayList") "s, loops and imperative worlds")
     (dummy "No lab this week")
     ("10"  "Practice implementing iterators")
     ("11"  "Heapsort, stress tests")
     ("12"  "TBD")
     ("13"  "TBD")
     (dummy "No lab this week")
     ))

@(render-cal calendar syllabus labs)
