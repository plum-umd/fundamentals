#lang scribble/manual
@(require "../utils.rkt")

@title[#:tag "assign07"]{6/26: Quick Lists, Part II}

Due: 6/26, midnight by svn.

Language: Java.

You must complete this assignment with your partner and should not
discuss solutions with anyone but your partner and the course staff.

This assignment is due Tuesday at midnight.

@section[#:tag "assign07lab"]{Follow-up from Lab}

Complete exercises 1-6 from @seclink["lab13"]{Lab 12}.  Note there was
a typo on the write-up before exercise 3 which has been corrected.

@section{Quick Visitors}

It would be a real shame if we had to throw out all of our old list
computations written as visitors just because we changed the
representation of lists.  In fact, it would be too bad if we couldn't
use a @tt{QList} everywhere a @tt{List} was expected.

In order to make this work, we need to express that @tt{QList}s are a
kind of @tt{List}, so let's revise our interface definitions slightly
to the following:

@verbatim|{
interface List<X> {
   // Cons given element on to this list.
   List<X> cons(X x);

   // Get the first element of this list (only defined on non-empty lists).
   X first();

   // Get the rest of this list (only defined on non-empty lists).
   List<X> rest();

   // Get the ith element of this list
   // (only defined for lists of i+1 or more elements).
   X get(Integer i);

   // Compute the number of elements in this list.
   Integer size();
}

interface QList<X> extends List<X> {
   // Cons given element on to this list.
   QList<X> cons(X x);

   // Get the rest of this list (only defined on non-empty lists).
   QList<X> rest();
}
}|

Now we've made it clear that a @tt{QList} is a variant of a @tt{List}.

Now let's look back at the interface for list visitors:

@verbatim|{
interface ListVisitor<X,R> {
  R visitEmpty();
  R visitCons(X first, List<X> rest);
}
}|

We can add the visitor pattern to the list interface in the usual way:

@verbatim|{
interface List<X> {
   ...
   <R> R accept(ListVisitor<X,R> v);
}
}|

But notice that this obligates you to implement @tt{accept} in the
classes that implement @tt{QList}.  Implement @tt{accept} so that
visitors can work seamlessly with quick lists.  In other words, your
@tt{accept} method must do the work of making the quick list
@emph{appear} as though it were just a regular list.

@section{Quick Iterators}

Add the following to the list interface and implement it:

@verbatim|{
interface List<X> extends Iterable<X> {
   ...
}
}|



