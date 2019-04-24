package edu.umd.cmsc132A;

abstract class ALo<X> implements Lo<X> {
    public Lo<X> cons(X x) { return new Cons<X>(x, this); }

    public Boolean contains(X that) {
        return this.ormap(y -> y.equals(that));
    }


    public LoIterator<X> iterator() { return new LoIterator<>(this); }

}
