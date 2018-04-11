package edu.umd.cmsc132A;

import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;

interface BST<X extends Comparable<X>> {

    // Add the given element into this BST
    BST<X> add(X x);

    // Inserts the given element into this BST,
    // replacing the first element that compares equally to x with x
    // if such an element exists
    BST<X> insertComp(X x);

    // Apply f to every element of this BST and collect results as a BST
    // NOTE: cannot assume f is monotonic
    <R extends Comparable<R>> BST<R> map(Function<X, R> f);

    // Apply f to every element of this BST and collect results as a BST
    // ASSUME: f is monotonic
    <R extends Comparable<R>> BST<R> mapMono(Function<X, R> f);

    // Count the number of elements in this BST
    Integer count();

    // Does this BST contain the given element?
    Boolean contains(X x);

    // Does there exist an element that satisfies p in this BST?
    Boolean exists(Predicate<X> p);

    // Do all elements satisfy p in this set?
    Boolean forAll(Predicate<X> p);

    // Convert this BST to a list of elements sorted in ascending order
    Listof<X> toSortedList();
}

class Leaf<X extends Comparable<X>> implements BST<X> {
    Leaf() {
    }

    // Add the given element into this BST
    public BST<X> add(X x) {
        return new Node<>(x, this, this);
    }

    // Inserts the given element into this BST,
    // replacing the first element that compares equally to x with x
    // if such an element exists
    public BST<X> insertComp(X x) {
        return null;
    }

    // Apply f to every element of this BST and collect results as a BST
    // NOTE: cannot assume f is monotonic
    public <R extends Comparable<R>> BST<R> map(Function<X, R> f) {
        return null;
    }

    // Apply f to every element of this BST and collect results as a BST
    // ASSUME: f is monotonic
    public <R extends Comparable<R>> BST<R> mapMono(Function<X, R> f) {
        return null;
    }

    // Count the number of elements in this BST
    public Integer count() {
        return null;
    }

    // Does this BST contain the given element?
    public Boolean contains(X x) {
        return null;
    }

    // Does there exist an element that satisfies p in this BST?
    public Boolean exists(Predicate<X> p) {
        return null;
    }

    // Do all elements satisfy p in this set?
    public Boolean forAll(Predicate<X> p) {
        return null;
    }

    // Convert this BST to a list of elements sorted in ascending order
    public Listof<X> toSortedList() {
        return null;
    }
}

class Node<X extends Comparable<X>> implements BST<X> {
    X val;
    BST<X> left;
    BST<X> right;

    Node(X val, BST<X> left, BST<X> right) {
        this.val = val;
        this.left = left;
        this.right = right;
    }

    // Add the given element into this BST
    public BST<X> add(X x) {
        return (x.compareTo(this.val) <= 0) ?
                new Node<>(this.val, this.left.add(x), this.right) :
                new Node<>(this.val, this.left, this.right.add(x));
    }

    // Inserts the given element into this BST,
    // replacing the first element that compares equally to x with x
    // if such an element exists
    public BST<X> insertComp(X x) {
        return null;
    }

    // Apply f to every element of this BST and collect results as a BST
    // NOTE: cannot assume f is monotonic
    public <R extends Comparable<R>> BST<R> map(Function<X, R> f) {
        return null;
    }

    // Apply f to every element of this BST and collect results as a BST
    // ASSUME: f is monotonic
    public <R extends Comparable<R>> BST<R> mapMono(Function<X, R> f) {
        return null;
    }

    // Count the number of elements in this BST
    public Integer count() {
        return null;
    }

    // Does this BST contain the given element?
    public Boolean contains(X x) {
        return null;
    }

    // Does there exist an element that satisfies p in this BST?
    public Boolean exists(Predicate<X> p) {
        return null;
    }

    // Do all elements satisfy p in this set?
    public Boolean forAll(Predicate<X> p) {
        return null;
    }

    // Convert this BST to a list of elements sorted in ascending order
    public Listof<X> toSortedList() {
        return null;
    }
}