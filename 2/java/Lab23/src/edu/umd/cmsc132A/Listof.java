package edu.umd.cmsc132A;

import java.util.Iterator;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;

/* Interfaces:
 * - ListVisitor<X, R>
 * - Listof<X>
 *
 * Abstract classes:
 * - AListof<X>
 *
 * Classes:
 * - Empty<X>
 * - Cons<X>
 * - ListIterator<X>
 */


interface ListVisitor<X, R> {
    R visitEmpty(Empty<X> mt);
    R visitCons(Cons<X> cons);
}

interface Listof<X> extends Iterable<X> {
    // The fundamental list operation
    <R> R foldr(BiFunction<X, R, R> f, R b);

    // Does an element that satisfies the given predicate exist?
    Boolean exists(Predicate<X> p);

    // Reverse this list
    Listof<X> reverse();

    // Append that list to the end of this list
    Listof<X> append(Listof<X> that);

    // Return the nth element of this list, if it exists
    Optional<X> nth(Integer n);

    // Return the length of this list
    Integer length();

    // Accept a list visitor
    <R> R accept(ListVisitor<X, R> visitor);

    // Construct a new list with the given element at the head
    Listof<X> cons(X x);

    // Map over this list
    <R> Listof<R> map(Function<X, R> f);
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
    static public <X> Listof<X> make(Integer n, Function<Integer, X> f) {
        return make(0, n, f);
    }

    // Make an empty list
    static public <X> Listof<X> empty() {
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

    // Return a string representation of this list
    public String toString() {
        String sep = ", ";
        String xs = this.foldr((x, s) -> x + sep + s, "");
        return xs.isEmpty() ? "[]" : "[" + xs.substring(0, xs.length() - sep.length()) + "]";
    }

    // Map the operation `f' over this list
    public <R> Listof<R> map(Function<X, R> f) {
        return this.foldr((x, rs) -> rs.cons(f.apply(x)), empty());
    }

    // Return the length of this list
    public Integer length() {
        return this.foldr((x, n) -> n+1, 0);
    }

}

class Empty<X> extends AListof<X> {
    public Boolean exists(Predicate<X> p) {
        return false;
    }

    public <R> R foldr(BiFunction<X, R, R> f, R b) {
        return b;
    }

    public <R> R accept(ListVisitor<X, R> visitor) {
        return visitor.visitEmpty(this);
    }

    public Listof<X> reverse() {
        return this;
    }

    public Listof<X> append(Listof<X> that) {
        return that;
    }

    public Optional<X> nth(Integer n) {
        return Optional.empty();
    }
}

class Cons<X> extends AListof<X> {
    X first;
    Listof<X> rest;

    public Cons(X first, Listof<X> rest) {
        this.first = first;
        this.rest = rest;
    }

    public Boolean exists(Predicate<X> p) {
        return p.test(this.first) || this.rest.exists(p);
    }

    public <Y> Y foldr(BiFunction<X, Y, Y> f, Y b) {
        return f.apply(this.first, this.rest.foldr(f, b));
    }

    public <R> R accept(ListVisitor<X, R> visitor) {
        return visitor.visitCons(this);
    }

    public Listof<X> reverse() {
        return this.rest.reverse().append(new Cons<>(this.first, new Empty<>()));
    }

    public Listof<X> append(Listof<X> that) {
        return new Cons<>(this.first, this.rest.append(that));
    }

    public Optional<X> nth(Integer n) {
        return n <= 0 ? Optional.of(this.first) : this.rest.nth(n-1);
    }

}

class ListIterator<X> implements Iterator<X>, Iterable<X> {
    private Listof<X> elems;

    ListIterator(Listof<X> elems) {
        this.elems = elems;
    }

    // (for Iterable<X>)
    public Iterator<X> iterator() {
        return this;
    }

    // Does this iterator have any more elements?
    public boolean hasNext() {
        return this.elems.accept(new ListVisitor<>() {
            public Boolean visitEmpty(Empty<X> mt) {
                return false;
            }
            public Boolean visitCons(Cons<X> cons) {
                return true;
            }
        });
    }

    // EFFECT: Updates iterator to point to next element;
    // EFFECT: Throws runtime exception if `elems' is empty;
    // Returns the next element of this list.
    public X next() {
        return this.elems.accept(new ListVisitor<>() {
            public X visitEmpty(Empty<X> mt) {
                throw new RuntimeException("Iterator has no next element!");
            }
            public X visitCons(Cons<X> cons) {
                ListIterator.this.elems = cons.rest;
                return cons.first;
            }
        });
    }

    // Creates a string representation of this iterator
    public String toString() {
        return "⟨" + elems + "⟩";
    }
}
