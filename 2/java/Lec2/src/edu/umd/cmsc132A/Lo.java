package edu.umd.cmsc132A;

// For some object o and p, assume they obey the laws of equals and hashCode

// 1. If o.hashCode() == p.hashCode(),
// what does o.equals(p) produce?
// - true
// - false
// - cannot determine

// 2. If o.equals(p) is false,
// what does o.hashCode() == p.hashCode() produce?
// - true
// - false
// - cannot determine


































import java.util.Comparator;
import java.util.Iterator;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Predicate;

interface Lo<X> extends Iterable<X> {

    // Produce an iterator for the elements of this list
    Iterator<X> iterator();

    // Does this list contain that element?
    Boolean contains(X that);


    // Sort the elements of this list in ascending order according to c.
    Lo<X> sort(Comparator<X> c);

    // Insert given element into this (sorted) list to produce a sorted list
    // ASSUME: this list is sorted according to c
    Lo<X> insert(Comparator<X> c, X x);

    // Get the first element if there is one
    Optional<X> first();

    // Get the rest of the list if there is one
    Optional<Lo<X>> rest();

    // Accept a given visitor and compute with this list
    <R> R accept(LoVisitor<X, R> v);

    // Lo<Integer> specific methods:

    // Sum this list of integers
    // Integer sum();

    // Add 1 to each element of this list
    // Lo<Integer> addOne();

    // Keep even elements of this list
    // Lo<Integer> keepEven();

    // Does this list contain 5?
    // Boolean containsFive();

    Boolean ormap(Predicate<X> p);

    Boolean andmap(Predicate<X> p);

    // map : [Listof X] [X -> Y] -> [Listof Y]
    <Y> Lo<Y> map(Function<X, Y> f);

    // Run given command on each element in this list
    void foreach(Consumer<X> f);

    // Add given element to the front of this list
    Lo<X> cons(X x);

    // Count the number of elements in this list
    Integer length();

    // Reverse the elements of this list
    Lo<X> rev();

    // Reverse the elements of this list and append with a.
    // INVARIANT: this.revAcc(a) == this.rev().app(a)
    Lo<X> revAcc(Lo<X> a);

    // Append this list and the given list
    Lo<X> app(Lo<X> that);

    // Zip together this list and given list into a list of pairs
    // Ending with whichever list is short
    <Y> Lo<Pair<X,Y>> zip(Lo<Y> ys);

    // Zip together this list and given cons into a list of pairs
    <Y> Lo<Pair<Y,X>> zipCons(Cons<Y> c);

    <Y> Y foldr(BiFunction<X, Y, Y> f, Y y);

}
