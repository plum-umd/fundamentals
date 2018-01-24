#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")

@title[#:style 'unnumbered #:tag "lab3"]{Lab 3: Conditional Eval}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/beginner.html"]{Beginning Student
Language}. Require the HtDP2e image and universe libraries at the top of your
definitions: @racketblock[(require 2htdp/image)
(require 2htdp/universe)]

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab3:temp"]{Temperature and Composition}

@larger{@bold{Ex 1}}: Define a function @tt{what-temp} that consumes a number
(in degrees Fahrenheit) and produces one of three strings: "cold" for cold
temperatures (say, less than 45), "hot" for hot temperatures (at or greater than
75), and "comfy" for anything in between.

@larger{@bold{Ex 2}}: Some international students (or just anyone tired of the
Imperial system) may prefer to use Celsius for their input temperatures. Define
a function @tt{celsius->fahrenheit} that converts a number from degrees Celsius
to degrees Fahrenheit. Search for the conversion online if you don't know it
off-hand.

@larger{@bold{Ex 3}}: Define a function
@link["https://xkcd.com/526/"]{@tt{what-temp/celsius}} that takes in a number (in
degrees Celsius) and returns the same three strings as in Ex 1 for the
appropriate temperature ranges.


@section[#:style 'unnumbered #:tag "lab3:chip"]{Chip the Cheap Sheep}

Swap @bold{Head} and @bold{Hands}.

Meet Chip! Chip likes to run.

@centered{@image["img/chip0.png"]{(Chip's 0th image is missing)}0
          @image["img/chip1.png"]{(Chip's 1st image is missing)}1
          @image["img/chip2.png"]{(Chip's 2nd image is missing)}2
          @image["img/chip3.png"]{(Chip's 3rd image is missing)}3}

@larger{@bold{Ex 4}}: Define a function @tt{which-chip} that, given a number
between 0 and 3, returns the properly indexed image above.

@larger{@bold{Ex 5}}: Define a function @tt{time->chip} that, given any
non-negative number, returns an appropriate frame of Chip's stride. For any N,
N+1 should return the next indexed image (looping back to the first after the
fourth image is returned). Animate Chip's graceful stride by passing
@tt{time->chip} to @racket[animate].

@colorize["red"]{Hint}: There may be a helpful, built-in
@link["https://docs.racket-lang.org/htdp-langs/beginner.html#%28def._htdp-beginner._%28%28lib._lang%2Fhtdp-beginner..rkt%29._modulo%29%29"]{numeric
function} to make @tt{time->chip} easy to implement with @tt{which-chip}.

@larger{@bold{Ex 6}}: Define a function @tt{run-chip-run} that, (like
@tt{time->chip}) given a non-negative number, returns the appropriate frame of
Chip's stride above, but also moves Chip right-to-left across a 1000px-wide
scene. Show one of your TAs this animation and you get to leave early!
