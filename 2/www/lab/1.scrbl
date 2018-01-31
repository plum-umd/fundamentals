#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")
@(require "../utils.rkt")

@lab-title[1]{Simple Data Definitions with Class}

@section[#:style 'unnumbered #:tag "lab1:intro"]{Introduction(s)}

You'll work in labs and on problem sets in pairs, and we've randomly
assigned your partner for this first lab. Find your partner and
introduce yourself. If your partner is @bold{not} present, let one of
your TAs know.

The two of you will work as a team to solve problems. At any time, one of you
will be the @bold{Head} and the other will be the @bold{Hands}. The @bold{Head}
does the thinking and the @bold{Hands} does the typing. @bold{Hands} type only
what the @bold{Head} tells them to, but you're free to discuss any issues that
pop up. We'll have you switch off during the lab to make sure each of you get
practice problem solving, dealing with syntax, and getting finger exercises on
the keyboard.

You both should install DrRacket, but only one instance should be use during the
lab. At the end you'll submit the lab as a pair via the
@link["https://submit.cs.umd.edu"]{UMD CS Submit Server} so we can keep an eye
on lab attendance.


@section[#:tag "lab1:class"]{Meet the Class language}

Make sure you still have DrRacket installed.  If not,
@link["https://download.racket-lang.org"]{download}, install, and run
DrRacket to get started.

Next you will need to install the Class programming language.  To do
this, open DrRacket.  Choose the @tt{File > Install Package...} menu.
In the text area for the "Package Source:" copy and paste the
following URL:

@centered{
@tt{https://github.com/dvanhorn/dpc.git#racket-v6.10}}

Press "Install".  You should see the output of the installation
process.  When it's done, press "Close".

Next you will need to select the @tt{class/0} language.  To do this,
select the drop-down menu in the lower-left-hand corner of DrRacket.
Select "Determine language from source".  You should see @tt{#lang
racket} in the definitions window.  Replace @tt{racket} with
@tt{class/0}.  Press "Run".  If you see @tt{class/0} as the langauge
in the interactions window, then you have successfully installed the
Class programming language.

You are now ready to start exploring the @tt{class/0} language.

You can start by looking at the Class language documentation by
selecting the "Help > Racket Documentation" menu and searching for
"class/0".  This will describe the basic syntax and semantics of
@tt{class/0} programs.

You can also start by writing simple @tt{class/0} programs like those
we've written in class.  Take a moment to review the lecture notes
from class and try running a few examples.

@section[#:tag "lab1:ex"]{Finger exercises}

In the following exercises you'll find simple programs that follow the
design recipe from last semester. Redesign these programs using the
@tt{class/0} language. You should use classes and methods instead in
place of atomic data, structures, and functions.

Each exercise builds on the last, so solve the exercises in-order and
in the same @emph{definitions window}.

@larger{@bold{Ex 1}}: This program calculates how long it takes to
travel some distance in feet at some speed in mile per hour. 

@class-block{
;; A Distance is a real number.
;; Interp: the distance (in feet) from one place to another.

;; A MPH is a real number.
;; Interp: some speed (in miles per hour).

;; how-fast : Distance PositiveReal -> MPH
;; Calculates how quickly (in miles per hour) one must travel to move
;; the given Distance in the given number of hours.
(check-within (how-fast 2640 1/4) 2 0.01)
(check-within (how-fast 138435 5.244) 5 0.01)
(define (how-fast dist hrs)
  (* (/ dist 5280) (/ 1 hrs)))
}

@larger{@bold{Ex 2}}: This program can calculate the time it takes for
some person to walk certain distances, as well as create encouraging,
personalized messages for the walker.

@class-block{
;; A Person is a (make-person String).
;; Interp: A person with some name.
(define-struct person (name))

;; Example People:
(define alice (make-person "Alice"))
(define bob (make-person "Bob"))

;; WALKER-SPEED : MPH
;; How quickly a Person walks.
(define WALKER-SPEED 3)

;; how-long-walking : Distance -> Real
;; Calculates how much time (in hours) will it take to move the given
;; distance at a walker's pace.
(check-expect (how-long-walking 1760) 1/9)
(check-within (how-long-walking 138435) 8.74 0.01)
(define (how-long-walking dist)
  (* dist (/ 1 (* 5280 WALKER-SPEED))))

;; message-walking : Person Distance -> String
;; Create an encouraging message to the person traveling some distance.
(check-expect (message-walking alice 15840)
              "Only 1 hour(s) to go, Alice!")
(check-expect (message-walking bob 7920) 
              "Only 1/2 hour(s) to go, Bob!")
(define (message-walking walk dist)
  (format "Only ~a hour(s) to go, ~a!"
          (how-long-walking dist)
          (person-name walk)))
}

@larger{@bold{Ex 3}}: This program can calculate the time it takes for
a car traveling at some average-rate to travel certain distances, as
well as increase and decrease the speed of the car.

@class-block{
;; A Car is a (make-car Person MPH).
;; Interp: A car driven by some person at some average speed.
(define-struct car (driver speed))

;; Example cars:
(define zippy (make-car alice 40))
(define clunker (make-car bob 10))

;; how-long-driving : Car Distance -> Real
;; Calculates how much time (in hours) will it take for the given
;; car to travel the given distance at the car's current pace.
(check-expect (how-long-driving zippy 21120) 1/10)
(check-expect (how-long-driving clunker 10560) 1/5)
(define (how-long-driving car dist)
  (* dist (/ 1 (* 5280 (car-speed car)))))

;; modify-speed : Car MPH -> Car
;; Create a new car with the average speed of the given car
;; accelerated or decelerated by the given delta.
(check-expect (modify-speed zippy 10) (make-car alice 50))
(check-expect (modify-speed clunker -5) (make-car bob 5))
(define (modify-speed car delta)
  (make-car (car-driver car) (+ delta (car-speed car))))
}

@larger{@bold{Ex 4}}: This program can calculate the time it takes for
a metro train to travel certain distances, as well as create messages to
relate that time for the riders of the train.

@class-block{
;; A Route is one of:
;; - "Green"
;; - "Red"

;; A Train is a (make-train Route).
;; Interp: A train traveling some route.
(define-struct train (route))

;; Example Trains:
(define green (make-train "Green"))
(define red (make-train "Red"))

;; GREEN-SPEED : MPH
;; How quickly a green-line train moves.
(define GREEN-SPEED 12)

;; RED-SPEED : MPH
;; How quickly a red-line train moves.
(define RED-SPEED 9)

;; how-long-riding : Train Distance -> Real
;; Calculates how much time (in hours) will it take to move the given
;; distance at the given speed (in miles per hour).
(check-expect (how-long-riding red 1760) 1/27)
(check-expect (how-long-riding green 63360) 1)
(define (how-long-riding train dist)
  (* dist (/ 1 (* 5280 (cond [(string=? "Green" (train-route train))
                              GREEN-SPEED]
                             [(string=? "Red" (train-route train))
                              RED-SPEED])))))

;; message-passengers : Train Distance -> String
;; Create a message to the train passengers traveling some distance.
(check-expect (message-passengers green 63360)
              "Green-line passengers: 1 hour(s) to destination.")
(check-expect (message-passengers red 1760) 
              "Red-line passengers: 1/27 hour(s) to destination.")
(define (message-passengers train dist)
  (format "~a-line passengers: ~a hour(s) to destination."
          (train-route train)
          (how-long-riding train dist)))
}

@larger{@bold{Ex 5}}: This program joins the three modes of
transportation in a single data definition.

@class-block{
;; A Transport is one of:
;; - Person
;; - Train
;; - Car

;; how-long : Transport Distance -> Real
(check-expect (how-long green 63360) 1)
(check-expect (how-long alice 1760) 1/9)
(check-expect (how-long clunker 21120) 2/5)
(define (how-long tspt dist)
  (cond [(person? tspt) (how-long-walking dist)]
        [(train?  tspt) (how-long-riding  tspt dist)]
        [(car?    tspt) (how-long-driving tspt dist)]))

;; message-eta : Transport Distance -> String
;; Create an ETA message giving the time remaining to travel the
;; given distance by some mode of transport.
(check-expect (message-eta bob 1760) "Only 1/9 hour(s) to go, Bob!")
(check-expect (message-eta green 63360)
              "Green-line passengers: 1 hour(s) to destination.")
(check-expect (message-eta zippy 21120)
              "ETA: 1/10 hour(s) at the current rate: 40 MPH")
(define (message-eta tspt dist)
  (cond [(person? tspt) (message-walking    tspt dist)]
        [(train?  tspt) (message-passengers tspt dist)]
        [(car?    tspt)
         (format "ETA: ~a hour(s) at the current rate: ~a MPH"
                 (how-long-driving tspt dist)
                 (car-speed tspt))]))
}

@larger{@bold{Ex 6}}: This allows a trip to be described as a list of
distances, then totaled into a single distance.

@class-block{
;; An Itinerary (List of Distances) is one of:
;; - (make-empty-lod)
;; - (make-cons-lod Distance LoD)
;; Interp: a sequence of distances to travel.
(define-struct empty-lod ())
(define-struct cons-lod (first rest))
 
;; total-distance : LoD -> Distance
;; Compute total distance to travel in the given list of distances.
(check-expect (total-distance (make-empty-lod)) 0)
(check-expect 
  (total-distance (make-cons-lod 1760 (make-cons-lod 5280 (make-empty-lod))))
  7040)
(define (total-distance lod)
  (cond [(empty-lod? lod) 0]
        [(cons-lod? lod)
         (+ (cons-lod-first lod) (total-distance (cons-lod-rest lod)))]))
}
