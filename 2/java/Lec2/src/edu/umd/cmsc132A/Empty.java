package edu.umd.cmsc132A;

import java.util.Comparator;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;

class Empty<X> extends ALo<X> {

    public X get(Integer i) {
        throw new RuntimeException("Index to large for this list");
    }

    public Lo<X> sort(Comparator<X> c) {
        return this;
    }

    public Lo<X> insert(Comparator<X> c, X x) {
        return new Cons<>(x, this);
    }

    public Optional<X> first() { return Optional.empty(); }

    public Optional<Lo<X>> rest() { return Optional.empty(); }

    public <R> R accept(LoVisitor<X,R> v) {
        return v.visitEmpty();
    }

    public <Y> Lo<Y> map(Function<X,Y> f) {
        return new Empty<>();
    }

    public void foreach(Consumer<X> f) { }

    public Boolean ormap(Predicate<X> p) {
        return false;
    }

    public Boolean andmap(Predicate<X> p) {
        return true;
    }

    public Integer length() {
        return 0;
    }

    public Lo<X> rev() {
        return this;
    }

    // Reverse the elements of this empty list and append with a.
    // INVARIANT: this.revAcc(a) == this.rev().app(a)
    public Lo<X> revAcc(Lo<X> a) {
        return a;
    }

    // Append this empty list and the given list
    public Lo<X> app(Lo<X> that) {
        return that;
    }

    public <Y> Lo<Pair<X,Y>> zip(Lo<Y> ys) {
        return new Empty<>();
    }

    public <Y> Lo<Pair<Y,X>> zipCons(Cons<Y> c) {
        return new Empty<>();
    }

    public <Y> Y foldr(BiFunction<X, Y, Y> f, Y y) {
        return y;
    }
}
