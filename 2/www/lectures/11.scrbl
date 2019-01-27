#lang scribble/manual
@(require scribble/eval
          racket/sandbox
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ... check-expect))
          (for-label (except-in class/0 define-struct ... check-expect))
          (for-label class/universe)
          "../utils.rkt")

@lecture-title[11]{Computations on Many Structural Arguments: Double Dispatch}

@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=1f9c02ac-58a3-4904-8196-a88c0161db27"]{Video}.

Double dispatch is the object-oriented approach to writing a
computation that must do a case analysis on multiple (more than one)
values that belong to union data definitions.

Consider the following example, an ISL program:

@class-block{
;; A [Pairof A B] is a (make-pair A B)
(define-struct pair (left right))

;; Zip together two lists into a list of pairs
;; Runs out whenever the shorter list runs out
;; zip : [Listof A] [Listof B] -> [Listof [Pairof A B]]
(check-expect (zip '() '()) '())
(check-expect (zip '() (list 1 2 3)) '())
(check-expect (zip (list 1 2 3) '()) '())
(check-expect (zip (list 1 2 3) (list "a" "b" "c"))
              (list (make-pair 1 "a")
                    (make-pair 2 "b")
                    (make-pair 3 "c")))
(define (zip xs1 xs2)
  (cond [(empty? xs1) '()]        
        [(cons? xs1)
         (cond [(empty? xs2) '()]
               [(cons? xs2)
                (cons (make-pair (first xs1) (first xs2))
                      (zip (rest xs1) (rest xs2)))])]))
}

Notice how this follows the template for @racket[xs1] but then also
does a case analysis on @racket[xs2].

In moving to an object-oriented setting, you should be totally
comfortable with using dynamic dispatch to perform the case analysis
on @racket[xs1]: it becomes @racket[this] and the code for the RHS of
the first cond clause goes in the @tt{Empty} class and the RHS of the
second clause goes in the @tt{Cons} class.

BUT: how do you do the second cond that looks at @racket[xs2]?

The answer is: add a helper method that is invoked from @racket[xs2].
Invoking the method will cause a second (hence DOUBLE) dynamic
dispatch to occur.

We can see this even in ISL.  Let's rewrite the above program to use a
helper function that follows the template for @racket[xs2]:

@class-block{
;; Zip together two lists into a list of pairs
;; Runs out whenever the shorter list runs out
;; zip : [Listof A] [Listof B] -> [Listof [Pairof A B]]
(check-expect (zip '() '()) '())
(check-expect (zip '() (list 1 2 3)) '())
(check-expect (zip (list 1 2 3) '()) '())
(check-expect (zip (list 1 2 3) (list "a" "b" "c"))
              (list (make-pair 1 "a")
                    (make-pair 2 "b")
                    (make-pair 3 "c")))
(define (zip xs1 xs2)
  (cond [(empty? xs1) '()]        
        [(cons? xs1)
         (zip-cons xs2 xs1)]))

;; zip-cons : [Listof B] (cons A [Listof A]) -> [Listof [Pairof A B]]
;; Zip xs2 (on the right) of the given non-empty list
(check-expect (zip-cons (list "a" "b" "c") (list 1 2 3))
              (list (make-pair 1 "a")
                    (make-pair 2 "b")
                    (make-pair 3 "c")))
(define (zip-cons xs2 xs1)
  (cond [(empty? xs2) '()]
        [(cons? xs2)
         (cons (make-pair (first xs1) (first xs2))
               (zip (rest xs1) (rest xs2)))]))
}

Now this program can easily be translated into an object oriented design:

@verbatim|{
interface Listof<A> {

    // Zip together this list and xs2 into a list of pairs
    // Runs out whenever the shorter list runs out
    <B> Listof<Pairof<A, B>> zip(Listof<B> xs2);

    // Zip this (on the right) of the given non-empty list
    <B> Listof<Pairof<B, A>> zipCons(Cons<B> xs1);
}

class Empty<A> implements Listof<A> {
    
    public <B> Listof<Pairof<A, B>> zip(Listof<B> xs2) {
        return new Empty<>();
    }

    public <B> Listof<Pairof<B, A>> zipCons(Cons<B> xs1) {
        return new Empty<>();
    }
}

class Cons<A> implements Listof<A> {
    A first;
    Listof<A> rest;

    Cons(A first, Listof<A> rest) {
        this.first = first;
        this.rest = rest;
    }

    public <B> Listof<Pairof<A, B>> zip(Listof<B> xs2) {
        return xs2.zipCons(this);
    }

    public <B> Listof<Pairof<B, A>> zipCons(Cons<B> xs1) {
        return new Cons<>(new Pairof<>(xs1.first, this.first),
                xs1.rest.zip(this.rest));
    }
}
}|

That's the basic idea.  Try some variants of this problem.  For
example, make a variant of this program that signals an error if the
lists are different lists.  Do it first in ISL if it's not clear how
to proceed in Java.