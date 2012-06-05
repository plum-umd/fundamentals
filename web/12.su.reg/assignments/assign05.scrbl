#lang scribble/manual
@(require "../utils.rkt")

@title[#:tag "assign05"]{6/5: Indexing}

Due: 6/5, midnight by svn.

Language: Java.

You must complete this assignment with your partner and should not
discuss solutions with anyone but your partner and the course staff.

This assignment is due Tuesday at midnight.

@section[#:tag "assign05lab"]{Follow-up from Lab}

Complete exercises 1-3 from @seclink["lab07"]{Lab 6} @bold{and}
exercises 1-4 from @seclink["lab08"]{Lab 7}.

@section{Indexing into data}

A fundamental idea in CS is that data can be enumerated and indexed.
So for example, we can refer the ``second'' element of a list, or 58th
element of a list.  With that in mind, develop an implementation of
the following interface:

@verbatim|{
// Represents a list of Xs.
interface List<X> {
  // Get the ith element of this list (counting from zero).
  X ref(Integer i);
}
}|

This method should throw an exception if the index exceeds the bounds
of the list.

We can expand on this interface.  For example, we can add a method
that updates a position in a list to hold a new value:

@verbatim|{
   // Update ith element to be given value.
   List<X> set(Integer i, X val);
}|

(Again, this method should throw an exception if the index exceeds the
bounds of the list.)

This works well for updating a list to produce another list, but we
could also expect the update to communicate the change everywhere that
reference the list.  To do that, we must use mutation:

@verbatim|{
   // EFFECT: Update ith element to be given value.
   void setBang(Integer i, X val);
}|

Develop both of these methods.


