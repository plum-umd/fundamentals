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

@title[#:tag "lab09"]{6/5: Array Lists}

The goal of this lab is to practice designing methods for indexed,
mutable data such as @tt{ArrayList}s.

@lab:section{Simple functional methods on ArrayLists}

There are basically two ways to design a method that operates on
indexed data: either count up or count down:

@verbatim|{
Result methodA(ArrayList<T> as) {
  return methodAi(as, as.size()-1);
}

Result methodB(ArrayList<T> as) {
  return methodBi(as, 0);
}

Result methodAi(ArrayList<T> as, Integer i) {
  if (i.equals(-1)) {
    ...
  } else {
    ... as.get(i) ... methodAi(as, i-1) ...
  }
}

Result methodBi(ArrayList<T> as, Integer i) {
  if (i.equals(as.size())) {
    ...
  } else {
    ... as.get(i) ... methodBi(as, i+1) ...
  }
}
}|


@exercise{Design a method @tt{Integer sum(ArrayList<Integer> is)} that
sums an array list of integers.  Implement the method counting up and
counting down.}

@exercise{Design a method @tt{Integer prod(ArrayList<Integer> is)}
that computes the product of an array list of integers.  Implement the
method counting up and counting down.}

Recall our interface for ``combiner'' functions:

@verbatim|{
interface Combiner<A,B> {
  // Combine an A and B into a B
  B combine(A a, B b);
}
}|

@exercise{Design a method @tt{<A,B> B foldr(ArrayList<A> is, B base,
Combiner<A,B> f)} that computes the right fold of the given function
and base over the array list, that is, this method should compute a
result analogous to @tt{foldr} from ISL.  Should the method count up
or down?}

@exercise{Design a method @tt{<A,B> B foldl(ArrayList<A> is, B base,
Combiner<A,B> f)} that computes the left fold of the given function
and base over the array list, that is, this method should compute a
result analogous to @tt{foldl} from ISL.  Should the method count up
or down?}

@exercise{Re-implement @tt{prod} and @tt{sum} using your fold
methods.}

Recall the @tt{Predicate<T>} interface we saw previously:

@verbatim|{
interface Predicate<T> {
  // Does this predicate hold for t?
  Boolean apply(T t);
}
}|

@exercise{Design a method @tt{<T> Boolean ormap(ArrayList<T> is,
Predicate<T> p)} that computes whether any element of the list
satisfies the given predicate.  Implement the method counting up and
counting down.}

@exercise{Design a method @tt{<T> Integer find(ArrayList<T> is,
Predicate<T> p)} that computes the index of the @emph{first} element
that satisfies the predicate (or @tt{-1} if there's no such element).
Should the method count up or count down?}

@exercise{Design a method @tt{<T> Boolean same(ArrayList<T> is,
ArrayList<T> js)} that determines whether the two given arrays are
``the same'', meaning they have the same elements, in the same order.
Implement the method counting up and counting down.}

@exercise{Design a method @tt{<T> ArrayList<T> reverse(ArrayList<T>
is)} that computes the reverse of the given list.  (This method should
have no effect on @tt{is}.)}

Here is a data definition for pairs:

@verbatim|{
class Pair<X,Y> {
  Pair(X left, Y right) {
    this.left = left;
    this.right = right;
  }
}
}|

@exercise{Design a method @tt{<T,V> ArrayList<Pair<T,V>>
zip(ArrayList<T> is, ArrayList<V> js)} that ``zips'' together an array
list of @tt{T}s and an array list of @tt{V}s into an array list of
pairs of @tt{T}s and @tt{V}s.  You may assume the given lists are the
same length.}

@lab:section{Some imperative methods on ArrayLists}

Now you'll design a few methods that mutate their input list.

@exercise{Design a method @tt{<T> void reverseBang(ArrayList<T>
is)} that has the effect of reversing the given list.}

@exercise{Design a method @tt{<T> void replaceBang(ArrayList<T> is,
Predicate<T> p, T t)} that has the effect of replacing every element
that satisfies the predicate with the given value.}

Here was our interface definition for functions:

@verbatim|{
// Represents a function [X -> Y]
interface Fun<X,Y> {
  // Apply this function to argument x.
  Y apply(X x);
}
}|

@exercise{Design a method @tt{<T> void updateBang(ArrayList<T> is,
Fun<T,T> f)} that has the effect of replacing every element with the
result of applying the function to that element.}
