package edu.umd.cmsc132A;

import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;

// This file should not need to change.

interface Listof<X> {
    // The fundamental list operation
    <R> R foldr(BiFunction<X, R, R> f, R b);

    // Iterates `f' over this list
    void forEach(Consumer<X> f);

    // Returns the first element of this list, if it exists
    Optional<X> first();

    // Returns the `n'th element of this list, if it exists
    Optional<X> nth(Integer n);

    // Returns the tail of this list, if it exists
    Optional<Listof<X>> rest();
}

abstract class AListof<X> implements Listof<X> {

    // Make the `n'th element of the list
    static private <X> Listof<X> make(Integer n, Integer max, Function<Integer, X> f) {
        if (n.equals(max)) {
            return new Empty<>();
        } else {
            return new Cons(f.apply(n), make(n+1, max, f));
        }
    }

    // Make an `n'-element list: f(0), f(1), ..., f(n-1)
    public static <X> Listof<X> make(Integer n, Function<Integer, X> f) {
        return make(0, n, f);
    }

    // Make an empty list
    public static <X> Listof<X> empty() {
        return new Empty<>();
    }

    // Add `x' as the head of a new list extending this list
    public Listof<X> cons(X x) {
        return new Cons<>(x, this);
    }


}

class Empty<X> extends AListof<X> {
    public <R> R foldr(BiFunction<X, R, R> f, R b) { return b; }
    public void forEach(Consumer<X> f) { return; }
    public Optional<X> first() { return Optional.empty(); }
    public Optional<X> nth(Integer n) { return Optional.empty(); }
    public Optional<Listof<X>> rest() { return Optional.empty(); }
}

class Cons<X> extends AListof<X> {
    X first;
    Listof<X> rest;

    Cons(X first, Listof<X> rest) {
        this.first = first;
        this.rest = rest;
    }

    public <Y> Y foldr(BiFunction<X, Y, Y> f, Y b) {
        return f.apply(this.first, this.rest.foldr(f, b));
    }

    public void forEach(Consumer<X> f) {
        f.accept(this.first);
        this.rest.forEach(f);
    }

    public Optional<X> first() { return Optional.of(this.first); }

    public Optional<X> nth(Integer n) {
        if (n.equals(0)) {
            return Optional.of(this.first);
        } else {
            return this.rest.nth(n-1);
        }
    }

    public Optional<Listof<X>> rest() { return Optional.of(this.rest); }
}