package edu.umd.cmsc132A;

import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;

// MultiSets of elements of type X
// A multiset is like a set, but allows multiple instances of elements
interface MultiSet<X> {
    // Add x to this multiset
    MultiSet<X> add(X x);

    // Apply f to every element of this multiset, collect results as a multiset
    <R> MultiSet<R> map(Function<X, R> f);

    // Count the number of elements in this multiset
    Integer count();

    // Is this multiset the same as the given multiset?
    Boolean same(MultiSet<X> s);

    // Does this multiset contain all of the elements of the given multiset?
    Boolean superset(MultiSet<X> s);

    // Does the given multiset contain all of the elements of given multiset?
    Boolean subset(MultiSet<X> s);

    // Does this mutliset contain the given element?
    Boolean contains(X x);

    // Does there exist an element that satisfies p in this multiset?
    Boolean exists(Predicate<X> p);

    // Do all elements satisfy p in this multiset?
    Boolean forAll(Predicate<X> p);

    // Convert this multiset to a list of elements (order is unspecified)
    Listof<X> toList();

    // Produce an element of this multiset (if one exists)
    Optional<X> elem();

    // Choose an element from this (if one exists) and
    // produce the element and rest of the multiset
    Optional<Pairof<X, MultiSet<X>>> choose();

    // Convert this multiset into a set
    Set<X> toSet();
}
