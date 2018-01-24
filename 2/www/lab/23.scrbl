#lang scribble/manual
@(require scribble/core scribble/examples "helper.rkt"
          (for-label lang/htdp-intermediate-lambda))

@title[#:style 'unnumbered #:tag "lab23"]{Lab 23: Accumulating Numbers}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/intermediate-lam.html"]{Intermediate
Student Language with Lambda}.

Make sure you follow
@link["https://cs.umd.edu/class/fall2017/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab23:tedious"]{Busy Work}

By the age of 8 Johann Carl Friedrich Gauss was a troublesome student. To stay
his mewling, his teacher came up with a tedious task for him to perform: adding
together the numbers 1 through 100. Much to the teacher's chagrin, Gauss
responded with 5050 after only a moment's thought.

Of course, we can easily perform the same task.

@bold{Ex 1}: Define a function @tt{sum-all} that returns the sum of the natural
numbers from 1 to the given natural number @tt{n}.  Write it using the standard
structural template over the natural numbers.

@; ;; sum-all : Natural -> Natural
@; (define (sum-all n)
@;   (cond [(zero? n) 0]
@;         [else (+ n (sum-all (sub1 n)))]))

Gauss came up with his sum in only a moment. Per the International System of
Units, a moment is just over 3 seconds. I'm pretty sure we've got him
beat. Let's test it by timing the execution of @tt{sum-all} on 100.

@examples[#:label #f
          (eval:alts (time (sum-all 100))
                     (eval:result @racketresult[5050]
                                  "cpu time: 0 real time: 1 gc time: 0"))]

Pssh, it only took us a millisecond. Take that Gauss! I bet we can even sum the
first million numbers in less time than Gauss took for the first hundred.

@examples[#:label #f
          (eval:alts (time (sum-all 1000000))
                     (eval:result @racketresult[500000500000]
                                  "cpu time: 649 real time: 671 gc time: 162"))]

Not bad. But can we do better? Let's look at what @tt{sum-all} is doing when it
executes.

@bold{Ex 2}: Step through @racket[(sum-all 5)] and in a comment write down the
full addition expression that executes after all the sum-all terms are
gone. (There should only be numbers and @racket[+].)

@bold{Ex 3}: Which addition is performed first? Which addition is performed
last? Write your answer down in a comment.


@section[#:style 'unnumbered #:tag "lab23:acc"]{In What Direction?}

If you've defined @tt{sum-all} according to the template for the natural
numbers, the first addition performed is @racket[(+ 1 0)] and the last is
@racket[(+ 5 10)]. The function @tt{sum-all} adds the naturals up from 1 to 5
after building the expression: @racketblock[(+ 5 (+ 4 (+ 3 (+ 2 (+ 1 0)))))]

Is it possible to add the numbers from 5 down to 1, you ask? Yes indeed! But we
need an extra argument to @emph{accumulate} the result as we calculate, so we'll
call it @tt{sum-all/acc}.

@#reader scribble/comment-reader (racketblock
;; sum-all/acc : Natural Natural -> Natural
;; Sum the naturals from n to 1.
(define (sum-all/acc n a)
  (cond [(zero? n) a]
        [else (sum-all/acc (sub1 n) (+ n a))]))
)

What's the difference here? Rather than building a big expression that is then
added together like @tt{sum-all}, @tt{sum-all/acc} performs one addition each
time it recurs and simply returns the result once it reaches zero. Of course, we
must give an initial value for the accumulator to @tt{sum-all/acc}.

@bold{Ex 4}: What should be the initial value for the accumulator in
@tt{sum-all/acc}? Define a function @tt{sum-all-down} with the same signature as
@tt{sum-all}, which gives the given number and correct initial accumulator to
@tt{sum-all/acc}.

@bold{Ex 5}: Step through @racket[(sum-all-down 5)]. Confirm that it does not
build a large addition expression while it executes. What additions does it
perform? Write them down in a comment.

Now what's the point? Let's look at the cost of summing the first million
numbers using @tt{sum-all-down}.

@examples[#:label #f
          (eval:alts (time (sum-all-down 1000000))
                     (eval:result @racketresult[500000500000]
                                  "cpu time: 80 real time: 80 gc time: 0"))]

@image{img/lab23-sum-all-down-results.jpg}


@section[#:style 'unnumbered #:tag "lab23:whoa"]{Whoa}

Swap @bold{Head} and @bold{Hands}!

That beat our old time by nearly an order of magnitude.

And we can sum much larger numbers than the original @tt{sum-all}.

@bold{Ex 6}: Run a bunch of tests to see how large an input causes @tt{sum-all}
to run out of memory. Try to get @tt{sum-all-down} to run out of memory too.

@bold{Ex 7}: Why do you think @tt{sum-all-down} executes faster than
@tt{sum-all}? How is it related to the fact that it can sum larger numbers?
Discuss this with your partner, and feel free to ask other groups as well.

That's just the beginning. Recall the function @tt{fib} from the last lab.

@#reader scribble/comment-reader (racketblock
;; fib : Natural -> Natural
;; Return the nth term in the Fibonacci sequence.
(define (fib n)
  (cond [(= 0 n) 0]
        [(= 1 n) 1]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))
)

Note that, like our original @tt{sum-all}, the addition is performed on the
outside of the recursive calls to @tt{fib}.  This forces ISL+ to remember the
rest of the work that needs to be performed (add the right recursive call to
@tt{fib}) while it recurs down the left recursive call. But if we define
@tt{fib} with an accumulator, there's no need to remember each recursive
addition.

@bold{Ex 8}: Define @tt{fib/acc} with the signature @tt{Natural Natural ->
Natural}. Consider: what is (1) the proper initial accumulator value and (2)
what is the proper result in the base cases?

@colorize["red"]{@bold{Hint}}: Use one of the recursive calls to @tt{fib/acc} as
the new accumulator value to the other recursive call.

@; (define (fib/acc n a)
@;   (cond [(= 0 n) a]
@;         [(= 1 n) (+ a 1)]
@;         [else (fib/acc (- n 1) (fib/acc (- n 2) a))]))

@examples[#:label #f 
          (eval:alts (check-expect (time (fib/acc 40 0)) (time (fib 40)))
                     (eval:result @racketresult[]
                                  "cpu time: 52417 real time: 52413 gc time: 26
cpu time: 69273 real time: 69435 gc time: 70
The test passed!"))]

Not as impressive a difference as for @tt{sum-all}, but still a noticeable
improvement!


@section[#:style 'unnumbered #:tag "lab23:folds"]{Generalizing}

Like any inductively defined data, natural numbers have a fold that follows the
structural template. The given operation @tt{f} is applied first on the base
case result @tt{b} and @racket[1], then that result and @racket[2], and so on,
so we'll call it @tt{foldu} for "fold-up".

@#reader scribble/comment-reader (racketblock
;; foldu : [Natural X -> X] X Natural -> X
(define (foldu f b n)
  (cond [(zero? n) b]
        [else (f n (foldu f b (sub1 n)))]))
)

@bold{Ex 9}: Implement @tt{sum-all/up} in terms of @tt{foldu}. Test its
performance against the original @tt{sum-all}. Does it exhibit the same
behavior?

@bold{Ex 10}: Design the function @tt{foldd} with the same signature as
@tt{foldu}, which instead folds the natural numbers down using an accumulator.

@; (define (foldd f a n)
@;   (cond [(zero? n) a]
@;         [else (foldd f (f n a) (sub1 n))]))

@bold{Ex 11}: Implement @tt{sum-all/down} in terms of @tt{foldd}. Test its
performance against @tt{sum-all-down}. Does it exhibit the same behavior?
