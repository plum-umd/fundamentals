package edu.umd.cmsc132A;

import java.util.Comparator;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;

class Cons<X> extends ALo<X> {
    X first;
    Lo<X> rest;

    Cons(X first, Lo<X> rest) {
        this.first = first;
        this.rest = rest;
    }

    public X get(Integer i) {
        return i == 0 ? this.first : this.rest.get(i - 1);
    }

    public Lo<X> sort(Comparator<X> c) {
        return this.rest.sort(c).insert(c, this.first);
    }

    public Lo<X> insert(Comparator<X> c, X x) {

        if (c.compare(x, this.first) < 0) {
            return new Cons<>(x, this);
        } else {
            return new Cons<>(this.first, this.rest.insert(c, x));
        }
    }


    public Optional<X> first() { return Optional.of(this.first); }

    public Optional<Lo<X>> rest() { return Optional.of(this.rest); }

    public <R> R accept(LoVisitor<X,R> v) {
        return v.visitCons(this.first, this.rest);
    }

    public <Y> Lo<Y> map(Function<X,Y> f) {
        return new Cons<>(f.apply(this.first),
                this.rest.map(f));
    }

    public void foreach(Consumer<X> f) {
        f.accept(this.first);
        this.rest.foreach(f);
    }


    public Boolean ormap(Predicate<X> p) {
        return p.test(this.first) || this.rest.ormap(p);
    }

    public Boolean andmap(Predicate<X> p) {
        return p.test(this.first) && this.rest.ormap(p);
    }

    /*
    public Integer sum() {
        return this.first + this.rest.sum();
    }
    */

    // Compute the length of this non-empty list.
    public Integer length() {
        return 1 + this.rest.length();
    }

    // Reverse the elements of this non-empty list.
    public Lo<X> rev() {
        return this.revAcc(new Empty<X>());
    }

    // Reverse the elements of this non-empty list and append with a.
    // INVARIANT: this.revAcc(a) == this.rev().app(a)
    public Lo<X> revAcc(Lo<X> a) {
        return this.rest.revAcc(new Cons<>(this.first, a));
    }

    // Append this empty list and the given list
    public Lo<X> app(Lo<X> that) {
        return new Cons<>(this.first, this.rest.app(that));
    }


    public <Y> Lo<Pair<Y,X>> zipCons(Cons<Y> ys) {
        return new Cons<Pair<Y,X>>(new Pair<>(ys.first, this.first),
                ys.rest.zip(this.rest));
    }

    public <Y> Lo<Pair<X,Y>> zip(Lo<Y> ys) {
        return ys.zipCons(this);
    }

    public <Y> Y foldr(BiFunction<X, Y, Y> f, Y y) {
        return f.apply(this.first, this.rest.foldr(f, y));
    }
}
