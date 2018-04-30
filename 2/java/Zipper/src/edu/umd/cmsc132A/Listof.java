package edu.umd.cmsc132A;

import java.util.Iterator;
import java.util.function.BiFunction;
import java.util.function.Function;

interface ListVisitor<X, R> {
    R visitEmpty(Empty<X> mt);
    R visitCons(Cons<X> cons);
}

interface Listof<X> extends Iterable<X> {
    // The fundamental list operation
    <R> R foldr(BiFunction<X, R, R> f, R b);

    // Accept a list visitor
    <R> R accept(ListVisitor<X, R> visitor);

    Listof<X> cons(X x);

    // Create a zipper of this list
    // ListZipper<X> zip();
}

abstract class AListof<X> implements Listof<X> {

    // Make the `n'th element of the list
    static private <X> Listof<X> make(Integer n, Integer max, Function<Integer, X> f) {
        if (n.equals(max)) {
            return new Empty<>();
        } else {
            return new Cons<>(f.apply(n), make(n+1, max, f));
        }
    }

    // Make an `n'-element list: f(0), f(1), ..., f(n-1)
    static <X> Listof<X> make(Integer n, Function<Integer, X> f) {
        return make(0, n, f);
    }

    // Make an empty list
    static <X> Listof<X> empty() {
        return new Empty<>();
    }

    // Add `x' as the head of a new list extending this list
    public Listof<X> cons(X x) {
        return new Cons<>(x, this);
    }

    // Create an iterator of this list
    public Iterator<X> iterator() {
        return new ListIterator<>(this);
    }

    // Create a zipper of this list
    /* public ListZipper<X> zip() {
        return new ListZipper<>(this);
    }*/

    // Return a string representation of this list
    public String toString() {
        String sep = ", ";
        String xs = this.foldr((x, s) -> x + sep + s, "");
        return xs.isEmpty() ? "[]" : "[" + xs.substring(0, xs.length() - sep.length()) + "]";
    }
}

class Empty<X> extends AListof<X> {
    public <R> R foldr(BiFunction<X, R, R> f, R b) {
        return b;
    }
    public <R> R accept(ListVisitor<X, R> visitor) {
        return visitor.visitEmpty(this);
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

    public <R> R accept(ListVisitor<X, R> visitor) {
        return visitor.visitCons(this);
    }
}

class ListIterator<X> implements Iterator<X>, Iterable<X> {
    Listof<X> elems;

    ListIterator(Listof<X> elems) {
        this.elems = elems;
    }

    public Iterator<X> iterator() {
        return this;
    }

    public boolean hasNext() {
        return this.elems.accept(new ListVisitor<X, Boolean>() {
            public Boolean visitEmpty(Empty<X> mt) {
                return false;
            }
            public Boolean visitCons(Cons<X> cons) {
                return true;
            }
        });
    }

    public X next() {
        return this.elems.accept(new ListVisitor<X, X>() {
            public X visitEmpty(Empty<X> mt) {
                throw new RuntimeException("Iterator has no next element!");
            }

            // EFFECT: Updates iterator to point to next element
            public X visitCons(Cons<X> cons) {
                ListIterator.this.elems = cons.rest;
                return cons.first;
            }
        });
    }

    public String toString() {
        return "⟨" + elems + "⟩";
    }
}