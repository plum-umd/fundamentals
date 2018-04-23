package edu.umd.cmsc132A;

import java.util.Iterator;

public class LotsOfIterators {
}


// Iterator<X>
// X next()
// bool hasNext()

// Iterable<X>
// Iterator<X> iterator()

class Zeros implements Iterator<Integer>, Iterable<Integer> {
    public Integer next() {
        return 0;
    }

    public boolean hasNext() {
        return true;
    }

    public Iterator<Integer> iterator() {
        return this;
    }
}

class Nats implements Iterator<Integer> {
    Integer i;

    Nats() {
        this.i = 0;
    }

    public Integer next() {
        Integer j = i;
        i = i + 1;
        return j;
    }

    public boolean hasNext() {
        return true;
    }
}

class Range implements Iterator<Integer>, Iterable<Integer> {
    Integer lo;
    Integer hi;

    Range(Integer lo, Integer hi) {
        this.lo = lo;
        this.hi = hi;
    }

    public Integer next() {
        Integer j = this.lo;
        this.lo = this.lo + 1;
        return j;
    }

    public boolean hasNext() {
        return !this.lo.equals(this.hi);
    }

    public Iterator<Integer> iterator() {
        return this;
    }
}


class ListIterator<X> implements Iterator<X>, Iterable<X> {
    Listof<X> elems;

    ListIterator(Listof<X> elems) {
        this.elems = elems;
    }

    public X next() {
        return this.elems.accept(new ListVisitor<X, X>() {
            public X visitEmpty(Empty<X> e) {
                throw new RuntimeException("iterator has no next element");
            }

            public X visitCons(Cons<X> c) {
                elems = c.rest;
                return c.first;
            }
        });
    }

    public boolean hasNext() {
        // return !this.elems.length().equals(0);
        return this.elems.exists(x -> true);
        //return this.elems.foldr((b, xs) -> true, false);
        /*
        return this.elems.accept(new ListVisitor<Boolean, X>() {
            public Boolean visitEmpty(Empty<X> e) {
                return false;
            }

            public Boolean visitCons(Cons<X> c) {
                return true;
            }
        });
        */
    }


    public Iterator<X> iterator() { return this; }
}





// Zip together elements of two iterators
class Zip<X> implements Iterator<Pairof<X,X>> {
    Iterator<X> left;
    Iterator<X> right;
    Zip(Iterator<X> left, Iterator<X> right) {
        this.left = left;
        this.right = right;
    }

    public boolean hasNext() {
        return this.left.hasNext() && this.right.hasNext();
    }

    public Pairof<X,X> next() {
        return new Pairof<>(this.left.next(), this.right.next());
    }
}


// Alternate between elements between two iterators
class Alt<X> implements Iterator<X> {
    Iterator<X> first;
    Iterator<X> second;
    Alt(Iterator<X> first, Iterator<X> second) {
        this.first = first;
        this.second = second;
    }
    public boolean hasNext() { return false; }
    public X next() { return null; }
}