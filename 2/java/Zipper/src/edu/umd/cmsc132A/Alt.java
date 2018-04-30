package edu.umd.cmsc132A;

import java.util.Iterator;

public class Alt<X> implements Iterator<X>, Iterable<X> {
    Iterator<X> first;
    Iterator<X> second;

    Alt(Iterator<X> first, Iterator<X> second) {
        this.first = first;
        this.second = second;
    }

    public X next() {
        X nxt = this.first.next();
        Iterator<X> tmp = this.first;
        this.first = this.second;
        this.second = tmp;
        return nxt;
    }

    public boolean hasNext() {
        return this.first.hasNext();
    }

    public Iterator<X> iterator() {
        return this;
    }
}
