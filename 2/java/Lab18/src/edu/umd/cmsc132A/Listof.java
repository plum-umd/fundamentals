package edu.umd.cmsc132A;

import java.util.function.BiFunction;

interface Listof<X> {
    // Cons given element on to this list
    Listof<X> cons(X x);

    // Compute the length of this list
    Integer length();

    // Fundamental list abstraction method
    <Y> Y foldr(BiFunction<X, Y, Y> f, Y b);

    // Does this list contain the given X (using ==)?
    Boolean contains(X x);
}

abstract class AListof<X> implements Listof<X> {

    static <Y> Listof<Y> empty() {
        return new Empty<Y>();
    }

    public Listof<X> cons(X x) {
        return new Cons<X>(x, this);
    }

    public Integer length() {
        return this.foldr((x, i) -> i + 1, 0);
    }

    public Boolean contains(X x) { return this.foldr((y, b) -> b || y == x, false); }
}

class Empty<X> extends AListof<X> {

    public <Y> Y foldr(BiFunction<X, Y, Y> f, Y b) {
        return b;
    }
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
}
