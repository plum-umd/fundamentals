#lang scribble/manual

@(require "../lab.rkt"
          "../unnumbered.rkt"
          "../utils.rkt"
          scribble/eval)

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require lang/htdp-intermediate))
    the-eval))


@(define exercise (exercise-counter))

@title[#:tag "lab11"]{6/12: Iterators, Iterables}

The goal of this lab is to practice implementing and using iterators
and iterables.

@lab:section{Home on the Half-open Range}

In class today, there came a point when we wanted to iterate through a
range of numbers.  The code we would have liked to write was something
like:

@verbatim|{
for (Integer i : new Integer(30)) {
  table.add(new Empty<Pair<K,V>>());
}
}|

This was just wishful thinking, but the idea was that if @tt{Integer}
had implemented the @tt{Iterable<Integer>} interface, then its
iterator could have produced the integers from 0 (inclusive) through 30
(exclusive).

Since the lackeys at Oracle didn't do the leg work for us, let's
design our own class that makes this possible.  What we'd like to
write to carry out our original task is:

@verbatim|{
for (Integer i : new HalfOpen(0,30)) {
  table.add(new Empty<Pair<K,V>>());
}
}|

This should run through the integers in @tt{[0,30)}.  The class is
called @tt{HalfOpen} because a @tt{[x,y)} is known as a
@emph{half-open interval}.

Half-open intervals are useful for all kinds of programming tasks.
For example, here's how to compute the factorial function using an
interval:

@verbatim|{
Integer fact(Integer n) {
  Integer acc = 1;
  for (Integer i : new HalfOpen(1, n+1)) {
    acc = acc * i;
  }
  return acc;
}
}|

@exercise{Design the @tt{HalfOpen} class so that it can be used as
shown above.}

@exercise{Add a constructor to @tt{HalfOpen} that consumes a single
integer and whose iterator goes from @tt{0} to that number.}

@exercise{Add an integrity check to your constructors so that
malformed intervals such as @tt{[3,-2)} are rejected by raising an
exception.}

@exercise{Add a third constructor to @tt{HalfOpen} that consumes three
integers and whose iterator starts from the first integer and goes to
the second integer by increments of the third integer.  Eg. @tt{new
HalfOpen(4, 12, 2)} would iterate through @tt{4, 6, 8, 10}.}

@exercise{Design the @tt{Open} and @tt{Closed} classes that represent
@tt{[x,y]} and @tt{(x,y)} intervals, respectively.  Include all of the
constructor features above.}

Now let's do something with a slight twist: let's construct an
infinite iterator.  An infinite iterator is just an iterator that will
iterate through an infinite number of elements.  As a first exercise,
design an iterator for the natural numbers.

@exercise{Design a class @tt{Nat} that implements
@tt{Iterable<Integer>} and whose iterator will produce the elements
@tt{0, 1, 2, 3, ...} and so on without end.}

At first glance it may not seem like an infinite iterator is useful
since it will go on forever, but in fact they can be quite useful when
combined with @tt{return}.  For example, if we want to find the first
integer that makes a given function produce 0, we could do so as:

@verbatim|{
Integer findZero(Fun<Integer,Integer> f) {
  for (Integer i : new Nat()) {
    if (f.apply(i).equals(0)) {
       return i;
    }
  }
  throw new RuntimeException("Inconceivable");
}
}|

@exercise{Add a constructor to @tt{Nat} that consumes a single integer
@tt{n} so that its iterator will produce the elements @tt{n, n+1, n+2,
n+3, ...}  and so on without end.}

@exercise{Add a constructor to @tt{Nat} that consumes two integers,
@tt{n} and @tt{step}, so that its iterator will produce the elements
@tt{n, n+step, n+2*step, n+3*step, ...}  and so on without end.}


@lab:section{Iterable Hash Tables}

Starting with the code we wrote in class
@link["06.12.12.java"]{today}, add the following features to hash
tables.

@exercise{Rewrite the hash table initialization to use an interval as
we had orginally wanted to.}

@exercise{Design the method @tt{void clean()} that ``cleans up'' a
hash table by removing elements in each list that are not reachable
with @tt{lookup}, i.e. any elements that have the same key as an
earlier key in the list.}

@exercise{Design the methods @tt{Iterator<K> iteratorKeys()},
@tt{Iterator<V> iteratorVals()}, and @tt{Iterator<Pair<K,V>>
iterator()} that produce iterators over the keys, the values, and
pairs of keys and values in the table, respectively.  Once you've done
this, make @tt{HashTable<K,V>} implement @tt{Iterable<Pair<K,V>>}.
(Hint: using @tt{clean} as the first step should make this problem
easier.)}


