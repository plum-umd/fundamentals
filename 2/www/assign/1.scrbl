#lang scribble/manual
@(require scribble/core)
@(require "../utils.rkt")
@(define (colorize c . content)
  (elem #:style (style #f (list (color-property c)))
        content))

@assn-title[1]{ISL with a touch of Class}

This is assignment is to be completed and submitted individually.  You
may not work with anyone else.

@bold{Due}: Tuesday, February 5, 11:59:59 PM EST.

@(define @Piazza @link["http://piazza.com/umd/spring2019/cmsc132a"]{Piazza})

@section[#:style 'unnumbered #:tag "assign1:piazza"]{Sign up for Piazza}

Our primary means of communication and answering questions will be the
course @Piazza message board. Sign up.

@section[#:style 'unnumbered #:tag "assign1:class"]{Install the Class programming language}

You will need DrRacket and the Class programming language to complete
this assignment.  If you do not have both installed, follow the
instructions included in @labref{1}.


@section[#:style 'unnumbered #:tag "assign1:read"]{Read the website}

Read all the pages on this website and familiarize yourself with the
course policies.

@section{Data Definitions and Methods in Class}

For this assignment, create a single file name @tt{assign1.rkt} that
contains solutions for each of the following problems.  (The submit
server will reject your submission if it is not named appropriately.)

You may organize the file using the same order as the problems given
here.

Each of these problems gives a small program designed using the design
recipe of last semester.  For each program, redevelop it using the
@tt{class/0} language.  You should use classes and methods instead in
place of atomic data, structures, and functions.

@subsection{Problem 1}

Here is a program for modelling the state of rocket ship that launches
from the bottom of the screen.

@#reader scribble/comment-reader (racketblock
;; A Rocket is a non-negative real number
;; Interp: units of time that have passed since launch

;; Uniform acceleration of Rocket in pixels per unit^2 of time
(define A 10)

;; tick : Rocket -> Rocket
;; Advance the given rocket by 1 unit of time
(check-expect (tick 5) 6)
(define (tick r)
  (+ r 1))

;; displacement : Rocket -> Real
;; The displacement from the launch site of the given rocket
(check-expect (displacement 0) 0)
(check-expect (displacement 1) (* 1/2 A))
(check-expect (displacement 5) (* 1/2 A 25))
(define (displacement r)
  (* 1/2 A (sqr r)))
)

@subsection{Problem 2}

Here is a program that deals with parts of a university fund raising
system for sending letters to wealthy alumni.

@#reader scribble/comment-reader (racketblock
; A Name is a (make-name String String)
; Interp: person's first and last name
(define-struct name (first last))
 
; greeting : Name -> String
; Create letter opening: "Dear <first> <last>,"
(check-expect (greeting (make-name "David" "Van Horn")) "Dear David Van Horn,")
(define (greeting n)
  (string-append "Dear " (name-first n) " " (name-last n) ","))
 
; same-last? : Name Name -> Boolean
; Do the given names have the same last name?
(check-expect (same-last? (make-name "A" "B") (make-name "C" "B")) #true)
(check-expect (same-last? (make-name "A" "B") (make-name "A" "C")) #false)
(define (same-last? n1 n2)
  (string=? (name-last n1) (name-last n2)))
)

@subsection{Problem 3}

Here is a program that deals with points in 3D space.

@#reader scribble/comment-reader (racketblock

;; A 3D is a (make-3d Real Real Real)
;; Interp: a coordinate in 3D space
(define-struct 3d (x y z))

;; dist3d : 3D 3D -> Real
;; Compute the distance between two 3D points
(check-within (dist3d (make-3d 2 3 1) (make-3d 8 -5 0)) 10.05 .01)
(define (dist3d p1 p2)
  (sqrt (+ (sqr (- (3d-x p1) (3d-x p2)))
           (sqr (- (3d-y p1) (3d-y p2)))
           (sqr (- (3d-z p1) (3d-z p2))))))       
)

@subsection{Problem 4}

Here is a program that deals with spheres, which relies on the program
in problem 3.

@#reader scribble/comment-reader (racketblock

;; A Sphere is a (make-sphere 3D Real)
;; Interp: a sphere with center and radius
(define-struct sphere (center radius))

;; sphere-intersect? : Sphere Sphere -> Boolean
;; Do the given spheres intersect?
(check-expect (sphere-intersect? (make-sphere (make-3d 1 1 1) 2)
                                 (make-sphere (make-3d 2 2 1) 1))
              #true)
(check-expect (sphere-intersect? (make-sphere (make-3d 1 1 1) 2)
                                 (make-sphere (make-3d 4 4 1) 1))
              #false)
(define (sphere-intersect? s1 s2)
  (<= (dist3d (sphere-center s1) (sphere-center s2))
      (+ (sphere-radius s1) (sphere-radius s2))))

;; sphere-volume : Sphere -> Real
;; Compute the volume of given sphere
(check-within (sphere-volume (make-sphere (make-3d 1 1 1) 5)) 523.6 0.1)
(define (sphere-volume s)
  (* 4/3 pi (expt (sphere-radius s) 3)))
)

@subsection{Problem 5}

Here is a program that deals with shapes, which relies on the program
in problem 4.

@#reader scribble/comment-reader (racketblock
;; A Shape is one of:
;; - a Sphere
;; - a Cube

;; A Cube is a (make-cube 3D Real)
;; Interp: a cube with center and side length
(define-struct cube (center side))

;; cube-volume : Cube -> Real
;; Compute the volume of the given cube
(check-expect (cube-volume (make-cube (make-3d 1 1 1) 5)) 125)
(define (cube-volume c)
  (expt (cube-side c) 3))

;; shape-volume : Shape -> Real
;; Compute the volume of the given shape
(check-expect (shape-volume (make-cube (make-3d 1 1 1) 5)) 125)
(check-within (shape-volume (make-sphere (make-3d 1 1 1) 5)) 523.6 0.1)
(define (shape-volume s)
  (cond [(sphere? s) (sphere-volume s)]
        [(cube? s) (cube-volume s)]))
)

@subsection{Problem 6}

Here is a program that deals with arbitrarily long sequences of
shapes, which relies on the program in problem 5.  (It defines it own
structures instead of using @racket[cons] and @racket['()] to make the
translation to a @tt{class/0} program more straightforward.)

@#reader scribble/comment-reader (racketblock

;; LoS (List of Shapes) is one of:
;; - (make-empty-los)
;; - (make-cons-los Shape LoS)
;; Interp: a sequence of shapes
(define-struct empty-los ())
(define-struct cons-los (first rest))

;; shapes-volume : LoS -> Real
;; Compute total volume of all shapes in given list
(check-expect (shapes-volume (make-empty-los)) 0)
(check-within
  (shapes-volume (make-cons-los (make-cube (make-3d 1 1 1) 5)
                                (make-cons-los (make-sphere (make-3d 1 1 1) 5)
                                               (make-empty-los))))
  (+ 125 523.6)
  .1)
(define (shapes-volume los)
  (cond [(empty-los? los) 0]
        [(cons-los? los)
         (+ (shape-volume (cons-los-first los))
            (shapes-volume (cons-los-rest los)))]))
)

@section[#:style 'unnumbered #:tag "assign1:submit"]{Submission}

Use @tt{submit.cs.umd.edu} to submit your solution to the problems in
part 1 as a single file called @tt{assign1.rkt}.
