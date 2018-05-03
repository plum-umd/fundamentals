package edu.umd.cmsc132A;

import java.util.Iterator;

interface ListVisitor<X, R> {
    R visitEmpty(Empty<X> mt);
    R visitCons(Cons<X> cons);
}

interface Listof<X> extends Iterable<X> {
    // Accept a list visitor
    <R> R accept(ListVisitor<X, R> visitor);
 }

abstract class AListof<X> implements Listof<X> {
    // Create an iterator of this list
    public Iterator<X> iterator() {
        return new ListIterator<>(this);
    }

}

class Empty<X> extends AListof<X> {
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
}