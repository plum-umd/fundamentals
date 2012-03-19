#lang scribble/manual

@(require "../lab.rkt"
          "../unnumbered.rkt"
          "../utils.rkt")

@(define exercise (exercise-counter))

@title[#:tag "lab11"]{3/19: Iterators}

@lab:section{Java & Collections}

Out of the box, Java comes with an extensive library of data structures. Java's
standard library takes advantage of its object-oriented nature and thus uses
interfaces extensively. For example, most of the important data structures you
will use in Java implement the
@hyperlink["http://docs.oracle.com/javase/1.5.0/docs/api/java/util/Collection.html"
           @racket[Collection]]
interface.

As you can see, there are quite a few methods that you have to implement to
have a real, working @racket[Collection]. That's a lot of work for the
library designer, but it's great that you can use any of these methods with
most of the data structures in Java.

In this lab, we don't actually want to make you implement all of these
methods by hand. Instead, we will take the easy way out by using classes
that Java provides to make our life easier. Before that, we will review
iterators.

@lab:section{Iterators}

In lecture today, we saw the @racket[Traversal] interface, which is
a functional way of doing a generic traversal over a data structure.
We saw that you could write programs in the style of Fundies 1.

Now we'll look at an alternative style of interfaces that iterate in a less
functional style.  First of all, let's just note that all @racket[Collection]s
implement the @racket[Iterable] interface that is defined like this:

@indented{@verbatim|{
  // An Iterable<T> implements
  //
  // iterator : -> Iterator<T>
  // Produces an iterator
}|}

This interface lets us extract an iterator in order to iterate over any
collection. The iterator itself implements an interface:

@indented{@verbatim|{
  // An Iterator<T> implements
  //
  // hasNext : -> Boolean
  // Check if there are more elements
  //
  // next : -> T
  // Produces the next element
  // Effect: modifies the iterator's position
  //
  // remove : -> Void
  // Removes the last element produced (optional)
  // Effect: modifies the collection
}|}

Looks simple enough right? Okay, try implementing it on your own.

@exercise{
  You have implemented the @racket[Dict<V>] interface many times before. Now
  improve any of your dictionary classes to implement the @racket[Iterable<V>]
  interface.

  You will need to define an @racket[Iterator] class to do this. Note that
  the @racket[V] parameter implies that this iterator only iterates over the
  values stored in the dictionary, not the keys.
}

@exercise{
  In an @racket[Examples] class, write a @racket[map] method that iterates
  over your dictionary and produces an @racket[Iterable<Y>] of the new values.

  Your @racket[map] method will need to take as input something implementing
  the @racket[Function<X, Y>] interface.
}

@exercise{
  Now write a @racket[reverse] method that returns an @racket[Iterable<T>]
  that will iterate in reverse order.

  Test both @racket[map] and @racket[reverse] with a dictionary.
}

Your iterator for dictionaries iterates only over the values. What if we want
to have both the keys and values? You can represent key-value pairs using an
interface like this:

@indented{@verbatim|{
  // A Pair<X, Y> implements
  //
  // getFirst : -> X
  // Produce the first item in the pair
  //
  // getSecond : -> Y
  // Produce the second item in the pair
}|}

@exercise{
  Now write iterators for your dictionary producing iterators that
  implement the @racket[Iterator<Pair<Key, V>>] interface. Recall that
  we defined @racket[Key] to be @racket[String] in Lab 8.
}

It turns out there are more specific iterators that let you do more
interesting things. For example, there are iterators that specifically work
over lists:

@indented{@verbatim|{
  // A ListIterator<T> extends Iterator<T> and implements
  //
  // add : T -> Void
  // Add a new element (optional)
  // Effect: updates the collection
  //
  // hasPrevious : -> Boolean
  // Check if we can produce the last element
  //
  // nextIndex : -> Integer
  // Produces the index of the next item
  //
  // previous : -> T
  // Produces the last element
  // Effect: updates the iterator
  //
  // previousIndex : -> Integer
  // Produces the index of the last item
  //
  // set : T -> Void
  // Set the last element produced to the new element (optional)
  // Effect: updates the collection
}|}

Note that since @racket[ListIterator] implements @racket[Iterator], it has all
of the methods that @racket[Iterator] implements (e.g., @racket[next]) even
though they are not listed here.

Let's think of a class that we can use with list iterators. Think back to Lab 5
where you used sorted lists to represent dictionaries. Now let's just implement
a general sorted list class that follows this interface:

@indented{@verbatim|{
  // A SortedList<V> implements Iterable<V>
  //
  // where V implements Comparable<V>
  //
  // and SortedList<V> also implements:
  //
  // add : V -> Void
  // Add the element to the list
  // Effect: modifies the list
  //
  // remove : V -> Void
  // Remove the given element from the list
  // Effect: modifies the list
  //
  // size : -> Integer
  // Produces the size of the list
}|}

@exercise{
  Implement a @racket[SortedList<V>] and its iterator. Make sure that the
  iterator implements @racket[Iterator<V>].
}

@exercise{
  Modify your list so that it produces a @racket[ListIterator<V>] instead.
}

@lab:section{Making collections out of iterators}

Now you have some experience building iterators and iterable things.  Java
provides a convenient way to turn anything with an iterator into a full-blown
collection using its built-in @emph[abstract] classes.

The way to do it is to extend the @racket[AbstractCollection] class and
define the following two methods: @racket[size] and @racket[iterator].

@exercise{
  Add a @racket[size] method to your sorted list class.
}

@exercise{
  Modify your sorted list to extend @racket[AbstractCollection]. Try to test all of
  the methods provided by the @racket[Collection] interface for your list
  (you can ignore @racket[toArray]).

  Does this work? If you read carefully, you might notice that some of the
  optional operations don't work. Change your tests (using @racket[checkException])
  to accommodate the operations that aren't implemented and make sure they pass.
}

@exercise{
  Follow the instructions in the @racket[Collection] interface documentation
  to implement the optional operations as well. Now test these operations
  too.
}

@exercise{
  Now try to make your dictionary class from earlier into a @racket[Collection] using
  the same technique.
}
