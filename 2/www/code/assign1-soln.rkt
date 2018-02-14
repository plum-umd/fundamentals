#lang class/0

;;------------------------------------------------------------------------------
;; Problem 1

; A Rocket is a (new rocket% Number) where Number is a positive number
; Interp: units of time that have passed since launch
 
; Uniform acceleration of Rocket in pixels per unit^2 of time
(define A 10)

(define-class rocket%
  (fields n)             

  ; tick : -> Rocket 
  ; Advance this rocket by 1 unit of time
  (check-expect (send (new rocket% 5) tick) (new rocket% 6))
  (define (tick)
    (new rocket% (+ (send this n) 1)))

  ; displacement : -> Real
  ; The displacement from the launch site of this rocket
  (check-expect (send (new rocket% 0) displacement) 0)
  (check-expect (send (new rocket% 1) displacement) (* 1/2 A))
  (check-expect (send (new rocket% 5) displacement) (* 1/2 A 25))
  (define (displacement)
    (* 1/2 A (sqr (send this n)))))


;;------------------------------------------------------------------------------
;; Problem 2

; A Name is a (new name% String String)
; Interp: person's first and last name

(define-class name%
  (fields first last)

  ; greeting : -> String
  ; Create letter opening: "Dear <first> <last>,"
  (check-expect (send (new name% "David" "VH") greeting) "Dear David VH,")
  (define (greeting)
    (string-append "Dear " (send this first) " " (send this last) ","))
 
  ; same-last? : Name -> Boolean
  ; Does this name and given name have the same last name?
  (check-expect (send (new name% "A" "B") same-last? (new name% "C" "B")) #t)
  (check-expect (send (new name% "A" "B") same-last? (new name% "A" "C")) #f)
  (define (same-last? n2)
    (string=? (send this last) (send n2 last))))


;;------------------------------------------------------------------------------
;; Problem 3

; A 3D is a (new 3d% Real Real Real)
; Interp: a coordinate in 3D space

(define-class 3d%
  (fields x y z)
  
  ; dist3d : 3D -> Real
  ; Compute the distance between this and given 3D points
  (check-within (send (new 3d% 2 3 1) dist3d (new 3d% 8 -5 0)) 10.05 0.01)
  (define (dist3d p2)
    (sqrt (+ (sqr (- (send this x) (send p2 x)))
             (sqr (- (send this y) (send p2 y)))
             (sqr (- (send this z) (send p2 z)))))))


;;------------------------------------------------------------------------------
;; Problem 4

; A Sphere is a (new sphere% 3D Real)
; Interp: a sphere with center and radius

(define-class sphere%
  (fields center radius)

  ; sphere-intersect? : Sphere -> Boolean
  ; Do the given spheres intersect?
  (check-expect (send (new sphere% (new 3d% 1 1 1) 2)
                      sphere-intersect? 
                      (new sphere% (new 3d% 2 2 1) 1))
                #true)
  (check-expect (send (new sphere% (new 3d% 1 1 1) 2)
                      sphere-intersect?
                      (new sphere% (new 3d% 4 4 1) 1))
                #false)
  (define (sphere-intersect? s2)
    (<= (send (send this center) dist3d (send s2 center))
        (+ (send this radius) (send s2 radius))))

  ; sphere-volume : -> Real
  ; Compute the volume of this sphere
  (check-within (send (new sphere% (new 3d% 1 1 1) 5) sphere-volume) 523.6 0.1)
  (define (sphere-volume)
    (* 4/3 pi (expt (send this radius) 3)))
  
  ;; shape-volume : -> Real
  ; Compute the volume of this sphere
  (check-within (send (new sphere% (new 3d% 1 1 1) 5) shape-volume) 523.6 0.1)
  (define (shape-volume)
    (send this sphere-volume)))


;;------------------------------------------------------------------------------
;; Problem 5

; A Shape is one of:
; - a Sphere
; - a Cube

; A Cube is a (new cube% 3D Real)
; Interp: a cube with center and side length

(define-class cube%
  (fields center side) 

  ; cube-volume : -> Real
  ; Compute the volume of the given cube
  (check-expect (send (new cube% (new 3d% 1 1 1) 5) cube-volume ) 125)
  (define (cube-volume)
    (expt (send this side) 3))

  ; shape-volume : Shape -> Real
  ; Compute the volume of the given shape
  (check-expect (send (new cube% (new 3d% 1 1 1) 5) shape-volume) 125)
  (define (shape-volume)
    (send this cube-volume)))


;;------------------------------------------------------------------------------
;; Problem 6: 26pts

; LoS (List of Shapes) is one of:
; - (new empty-los%)
; - (new cons-los% Shape LoS)
; Interp: a sequence of shapes

(define-class empty-los%

  ; shapes-volume : -> Real
  ; Compute total volume of all shapes in given empty list
  (check-expect (send (new empty-los%) shapes-volume) 0)
  (define (shapes-volume) 0))

(define-class cons-los%
  (fields first rest)  
  
  ; shapes-volume : -> Real
  ; Compute total volume of all shapes in given non-empty list  
  (check-within
   (send (new cons-los% (new cube% (new 3d% 1 1 1) 5)
              (new cons-los% (new sphere% (new 3d% 1 1 1) 5)
                   (new empty-los%)))
         shapes-volume)
   (+ 125 523.6)
   0.1)
  (define (shapes-volume)
    (+ (send (send this first) shape-volume)
       (send (send this rest) shapes-volume))))
