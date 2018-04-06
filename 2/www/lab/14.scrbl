#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")
@(require "../utils.rkt")

@lab-title[14]{Counting on State}

@section[#:style 'unnumbered #:tag "lab14:intro"]{Intro}

You'll work in this lab with your
@link["https://piazza.com/class/jcspfhewmdn41y?cid=108"]{lab partners}.

The two of you will work as a team to solve problems. At any time, one
of you will be the @bold{Head} and the other will be the
@bold{Hands}. The @bold{Head} does the thinking and the @bold{Hands}
does the typing. @bold{Hands} type only what the @bold{Head} tells
them to, but you're free to discuss any issues that pop up. You should
switch off during the lab to make sure each of you get practice
problem solving, dealing with syntax, and getting finger exercises on
the keyboard.

You should start this lab with @link["Lab14.zip"]{this project
skeleton}. Unzip the file into your IdeaProjects directory and open it
with IntelliJ to get started.

@section[#:style 'unnumbered #:tag "lab14:recall"]{Recall}

We created a data definition for a stateful counter in the
@link["https://www.cs.umd.edu/class/fall2017/cmsc131A/lab28.html"]{
last lab} of last semester. This is what it looked like:

@isl-block{
;; A Command is one of:
;; - "next"
;; - "reset"
;; Interp: Commands that a Counter can accept.

;; cmd-template : Command -> ???
(define (cmd-template cmd)
  (cond [(string=? cmd "next")  ...]
        [(string=? cmd "reset") ...]))

;; A Counter is a (list Natural [Command -> Counter])
;; Interp: the first of the list is the current

;; counter-template : Counter -> ???
(define (counter-template c)
  (... (first c) ...
       (second c) ...))

;; make-counter : Natural -> Counter
;; Create a Counter starting at the given number.
(define (make-counter n)
  (list n (lambda (cmd)
            (cond [(string=? cmd "next")  (make-counter (+ n 1))]
                  [(string=? cmd "reset") (make-counter 0)]))))

;; next-counter : Counter -> Counter
;; Return the next counter.
(define (next-counter c) ((second c) "next"))

;; reset-counter : Counter -> Counter
;; Reset the given counter to 0.
(define (reset-counter c) ((second c) "reset"))

;; get-count : Counter -> Natural
;; Return the counter's value.
(define (get-count c) (first c))

(define c0 (make-counter 0))
(define c1 (next-counter c0))
(define c2 (next-counter c1))
(define c0* (reset-counter c2))

(check-expect (get-count c0) 0)
(check-expect (get-count c1) 1)
(check-expect (get-count c2) 2)
(check-expect (get-count c0*) 0)
}

A @emph{Counter} was a two element list with the state (the current
count) as the first element and a function that created a new counter
as the second element. We sent the @emph{Counter} either the "next" or
"reset" messages to create a new incremented or zeroed @emph{Counter},
resp.

In Java-speak, the @emph{Counter} is a class that implements this
interface:

@verbatim|{
interface Counter {
  Integer getCount();
  Counter next();
  Counter reset();
}
}|

We can implement the very same @emph{Counter} in Java as well. What
follows is an almost-direct translation with the two-element list
replaced by a two-field class.

@verbatim|{
class SillyCounter implements Counter {
  Integer count;
  Function<String, Counter> messageToCounter;

  SillyCounter() { this(0); }
  SillyCounter(Integer count) {
    this.count = count;
    this.messageToCounter =
            (cmd) -> {
              if ("next".equals(cmd)) {
                return new SillyCounter(count+1);
              } else {
                return new SillyCounter();
              }
            };
  }

  public Integer getCount() {
    return count;
  }

  public Counter next() {
    return this.messageToCounter.apply("next");
  }

  public Counter reset() {
    return this.messageToCounter.apply("reset");
  }
}
}|

This is far from idiomatic Java: the @tt{messageToCounter} function is
unnecessary when we have the methods @tt{next} and
@tt{resetConter}.

In the rest of this lab, we'll look at how Java classes encapsulate
state and how we can @bold{mutate} that state without creating new
objects.

@section[#:style 'unnumbered #:tag "lab14:butreally"]{A
@emph{FunCounter}}

Note how the function @tt{SillyCounter.messageToCounter} models the
message-passing of objects. Since Java objects already know how to
receive and act on messages (as method calls), we can remove
@tt{messageToCounter} entirely.

@bold{Ex 1}: Define the class @tt{FunCounter} that implements
@tt{Counter} with a single field @tt{count}. Make sure the methods
@tt{next} and @tt{reset} return the same new
@tt{Counter}s that @tt{messageToCounter} returns.

The following is the small test suite for the @tt{FunCounter}. Before
running these tests on your code, let's predict the results.

@verbatim|{
boolean testFun(Tester t) {
  Counter fun0 = new FunCounter();
  Integer count0 = fun0.getCount();
  Counter fun1 = fun0.next();
  Integer count1 = fun1.getCount();
  Counter fun1b = fun0.next();
  Integer count1b = fun1b.getCount();
  Counter fun0b = fun1b.reset();
  Integer count0b = fun0b.getCount();
  return t.checkExpect(count0, fun0.getCount())
      && t.checkExpect(count1, fun1.getCount())
      && t.checkExpect(count1, count1b)
      && t.checkExpect(count1b, fun1b.getCount())
      && t.checkExpect(count0b, fun0b.getCount());
}
}|

@bold{Ex 2}: Should the first test pass? Is @tt{count0} the same as
@tt{fun0.getCount()}?

@bold{Ex 3}: Should the second test pass? Is @tt{count1} the same as
@tt{fun1.getCount()}?

@bold{Ex 4}: Should the third test pass? Is @tt{count1} the same as
@tt{count1b}?

@bold{Ex 5}: Should the fourth test pass? Is @tt{count1} the same as
@tt{fun1b.getCount}?

@bold{Ex 6}: Should the fifth test pass? Is @tt{count0b} the same as
@tt{fun0b.getCount}?

@section[#:style 'unnumbered #:tag "lab14:imp"]{Removing another step}

Great! Our tests all worked as expected, and we've simplified the
@emph{Counter} from last semester into a simple Java object. This may
be the only time that the Java code has been more terse than the
student languages.

Let's take another look at the method @tt{next}:

@verbatim|{
// Inside FunCounter:
public Counter next() {
  return new FunCounter(count+1);
}
}|

This creates a new instance of the @tt{FunCounter} class by calling
the constructor with the incremented count.

@verbatim|{
// Inside FunCounter:
FunCounter(Integer count) {
  this.count = count;
}
}|

The constructor assigns the new count to the new object's @tt{count}
field. This seems like a lot of work just to increment a number. Let's
see if we can do any better.

What if instead we simply incremented the count in the @emph{same
object}?

@verbatim|{
public Counter next() {
  this.count = this.count + 1;
  return this;
}
}|

There's no need to create a new object, so we just return @tt{this}
same @emph{Counter} with the @bold{mutated} field value.

@bold{Ex 7}: Implement the class @tt{ImpCounter} as a @tt{Counter}
similar to @tt{FunCounter}, except it mutates the field @tt{count} in
the @tt{next} and @tt{reset} methods.


@section[#:style 'unnumbered #:tag "lab14:ohno"]{So what changed?}

Alright, let's make sure everything in our @tt{ImpCounter} works as
expected.

The following is the same small test suite for the
@tt{ImpCounter}. Before running these tests on your code, let's
predict the results.

@verbatim|{
boolean testImp(Tester t) {
  Counter imp0 = new ImpCounter();
  Integer count0 = imp0.getCount();
  Counter imp1 = imp0.next();
  Integer count1 = imp1.getCount();
  Counter imp1b = imp0.next();
  Integer count1b = imp1b.getCount();
  Counter imp0b = imp1b.reset();
  Integer count0b = imp0b.getCount();
  return t.checkExpect(count0, imp0.getCount())
      && t.checkExpect(count1, imp1.getCount())
      && t.checkExpect(count1, count1b)
      && t.checkExpect(count1b, imp1b.getCount())
      && t.checkExpect(count0b, imp0b.getCount());
}
}|

@bold{Ex 8}: Should the first test pass? Is @tt{count0} the same as
@tt{imp0.getCount()}?

@bold{Ex 9}: Should the second test pass? Is @tt{count1} the same as
@tt{imp1.getCount()}?

@bold{Ex 10}: Should the third test pass? Is @tt{count1} the same as
@tt{count1b}?

@bold{Ex 11}: Should the fourth test pass? Is @tt{count1} the same as
@tt{imp1b.getCount}?

@bold{Ex 12}: Should the fifth test pass? Is @tt{count0b} the same as
@tt{imp0b.getCount}?

Now, run the tests if you have yet to do so.


@section[#:style 'unnumbered #:tag "lab14:void"]{A different signature}

Yikes! The first and last tests pass, but none of the others do! The
problem can be seen here:

@verbatim|{
...
Counter imp0 = new ImpCounter();
Integer count0 = imp0.getCount();
Counter imp1 = imp0.next();
...
}|

We create @tt{imp0} and get its count, @tt{count0} with the value
0. But when we call @tt{imp0.next()} on the next line, the object that
the variable @tt{imp1} refers to is the @emph{same object} as the
object that the variable @tt{imp0} refers to. When the count was
incremented, the variable @tt{imp0} no longer points to an object with
a count of 0.

Mutation can cause behavior that is non-local and
non-obvious. Contributing to this issue for @emph{Counter}s in
particular are the signatures for @tt{next} and @tt{reset}. Though
they return objects of type @tt{Counter}, that doesn't mean the return
@bold{new} objects.

A better practice for methods that intend to mutate data (rather than
create new data) is to explicitly omit any return value from the
method.

For example:
@verbatim|{
interface MCounter {
  Integer getCount();
  void next();
  void reset();
}
}|

The methods @tt{next} and @tt{reset} return no value: also known as
@tt{void}. We can implement this by removing the @tt{return this;}
from @tt{ImpCounter.{next, reset}}.

@bold{Ex 13}: Modify your @tt{ImpCounter} class so it implements the
@tt{MCounter} interface. Update the test suite to reflect those
changes. Do the modified signatures make the code and its behavior
more obvious to you and your partner?
