package edu.umd.cmsc132A;

import java.util.function.*;
import java.util.Optional;

// Sets of elements of type X
interface Set<X> {
    // Add x to this set
    Set<X> add(X x);

    // Apply f to every element of this set and collect results as a set.
    <R> Set<R> map(Function<X, R> f);

    // Count the number of elements in this set
    Integer count();

    // Is this set the same as the given set?
    Boolean same(Set<X> s);

    // Does this set contain all of the elements of the given set?
    Boolean superset(Set<X> s);

    // Does the given set contain all of the elements of the given set?
    Boolean subset(Set<X> s);

    // Does this set contain the given element?
    Boolean contains(X x);

    // Does there exist an element that satisfies p in this set?
    Boolean exists(Predicate<X> p);

    // Do all elements satisfy p in this set?
    Boolean forAll(Predicate<X> p);

    // Convert this set to a list of unique elements (order is unspecified)
    Listof<X> toList();

    // Produce an element of this set (if one exists)
    Optional<X> elem();

    // Choose an element from this (if one exists) and
    // produce the element and rest of the set
    Optional<Pairof<X, Set<X>>> choose();

    // Convert this set to a multiset
    MultiSet<X> toMultiSet();
}

abstract class ASet<X> implements Set<X> {
    static <Y> Set<Y> empty() {
        return null; // REPLACE WITH CODE TO CONSTRUCT AN EMPTY SET
    }
}






