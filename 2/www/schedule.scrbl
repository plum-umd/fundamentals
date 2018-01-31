#lang scribble/manual
@(require racket/list scribble/decode scribble/core scribble/html-properties)
@(require "utils.rkt")

@title[#:style 'unnumbered]{Schedule}

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
   '(((1 24) (1 26) (1 29) (labs))
     ((1 31) (2 2) (2 5) (labs "1"))
     ((2 7) (2 9) (2 12) (labs))
     ((2 14) (2 16) (2 19) (labs))
     ((2 21) (2 23) (2 26) (labs))
     ((2 28) (3 2) (3 5) (labs))
     ((3 7 "Midterm 1") (3 9) (3 12) (labs))
     ((3 14) (3 16) (3 19 "No Class: Spring break") (labs))
     ((3 21 "No Class: Spring break") (3 23 "No Class: Spring break") (3 26) (labs))
     ((3 28) (3 30) (4 2) (labs))
     ((4 4) (4 6) (4 9) (labs))
     ((4 11) (4 13) (4 16) (labs))
     ((4 18) (4 20) (4 23) (labs))
     ((4 25 "Midterm 2") (4 27) (4 30) (labs))
     ((5 2) (5 4) (5 7) (labs))
     ((5 9) (labs))

))

@(define syllabus
   `((1  "Designing Data: Simple classes, classes with containment")
     (2  "Designing unions of classes; self-reference")
     (3  "")
     ,@(build-list 40 (λ (i) (list (+ i 4) "")))
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
