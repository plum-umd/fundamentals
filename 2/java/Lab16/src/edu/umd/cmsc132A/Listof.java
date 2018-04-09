package edu.umd.cmsc132A;

import java.util.Optional;
import java.util.function.BiFunction;

interface IListof<X> {
    // Return the last cons cell of this list, if it exists.
    Optional<Cons<X>> last();

    // Is this list a cyclic list?
    Boolean isCyclic();
    Boolean isCyclicHelper(Cons<X> head);

    // Does this list contain x?
    Boolean contains(X x);
    Boolean containsHelper(Cons<X> head, X x);

    // How long is this list?
    Integer length();
    Integer lengthHelper(Cons<X> head);

    // Fold the elements of this list with `f' using the base value `b'.
    <R> R foldr(BiFunction<X, R, R> f, R b);
    <R> R foldrHelper(Cons<X> head, BiFunction<X, R, R> f, R b);
}

class Mt<X> implements IListof<X> {
    public Boolean isCyclic() { return false; }
    public Boolean isCyclicHelper(Cons<X> head) { return false; }

    public Boolean contains(X x) { return false; }
    public Boolean containsHelper(Cons<X> head, X x) { return contains(x); }

    public Integer length() { return 0; }
    public Integer lengthHelper(Cons<X> head) { return length(); }

    public <R> R foldr(BiFunction<X, R, R> f, R b) { return b; }
    public <R> R foldrHelper(Cons<X> head, BiFunction<X, R, R> f, R b) { return foldr(f, b); }

    public Optional<Cons<X>> last() {
        return Optional.empty();
    }
}

class Cons<X> implements IListof<X> {
    X first;
    IListof<X> rest;

    Cons(X first, IListof<X> rest) {
        this.first = first;
        this.rest = rest;
    }

    public Optional<Cons<X>> last() {
        Optional<Cons<X>> oc = this.rest.last();
        if (oc.isPresent()) {
            return oc;
        } else {
            return Optional.of(this);
        }
    }

    public Boolean isCyclic() {
        return rest.isCyclicHelper(this);
    }

    public Boolean isCyclicHelper(Cons<X> head) {
        if (head == this) {
            return true;
        } else {
            return this.rest.isCyclicHelper(head);
        }
    }

    public Integer length() {
        return 1 + this.rest.length();
    }

    public Integer lengthHelper(Cons<X> head) {
        return null;
    }

    public Boolean contains(X x) {
        if (this.first.equals(x)) {
            return true;
        } else {
            return this.rest.contains(x);
        }
    }

    public Boolean containsHelper(Cons<X> head, X x) {
        return null;
    }

    public <R> R foldr(BiFunction<X, R, R> f, R b) {
        return f.apply(this.first, this.rest.foldr(f, b));
    }

    public <R> R foldrHelper(Cons<X> head, BiFunction<X, R, R> f, R b) {
        return null;
    }
}
