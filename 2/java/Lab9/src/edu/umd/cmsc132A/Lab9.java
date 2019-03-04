package edu.umd.cmsc132A;

import tester.Tester;

public class Lab9 {
    // Intentionally blank; leave blank
}

class Pair<X,Y> {
    X left;
    Y right;

    Pair(X left, Y right) {
        this.left = left;
        this.right = right;
    }

    // Swap the elements in this pair
    Pair<Y,X> swap() {
        return new Pair<>(this.right, this.left);
    }
}

interface LoInteger {
    // Sum this list of integers
    Integer sum();

    // Add 1 to each element of this list
    LoInteger addOne();

    // Add 5 to each element of this list
    LoInteger addFive();

    // Add n to each element of this list
    LoInteger addN(Integer n);

    // Keep even elements of this list
    LoInteger keepEven();

    // Does this list contain 5?
    Boolean containsFive();

    // Does this list contain something larger than 8?
    Boolean containsBiggerThanEight();

    // Does this list contain an element for which w.test(x) produces true?
    Boolean contains(Predicate<Integer> w);
}


abstract class ALoInteger implements LoInteger {
    public LoInteger addOne() {
        return this.addN(1);
    }

    public LoInteger addFive() {
        return this.addN(5);
    }

    public Boolean containsFive() {
        return this.contains(new EqualsFive());
    }

    public Boolean containsBiggerThanEight() {
        return this.contains(new BiggerThanEight());
    }

}

class EmptyLoInteger extends ALoInteger {
    public Integer sum() { return 0; }
    public LoInteger addN(Integer n) { return this; }
    public LoInteger keepEven() { return this; }
    public Boolean contains(Predicate<Integer> w) { return false; }
}

class ConsLoInteger extends ALoInteger {
    Integer first;
    LoInteger rest;
    ConsLoInteger(Integer first, LoInteger rest) {
        this.first = first;
        this.rest = rest;
    }

    public Integer sum() {
        return this.first + this.rest.sum();
    }

    public LoInteger addN(Integer n) {
        return new ConsLoInteger(n + this.first, this.rest.addN(n));
    }

    public LoInteger keepEven() {
        /*
        if ((this.first % 2) == 0) {
            return new ConsLoInteger(this.first, this.rest.keepEven());
        } else {
            return this.rest.keepEven();
        }
        */

        return ((this.first % 2) == 0) ?
            new ConsLoInteger(this.first, this.rest.keepEven()) :
            this.rest.keepEven();
    }

    public Boolean contains(Predicate<Integer> w) {
        return (w.test(this.first)) || this.rest.contains(w);
    }
}

// Interp: a predicate on values of type T
interface Predicate<T> {
    Boolean test(T t);
}

// Interp: a function from Xs to Ys
interface Function<Fred,Wilma> {
    Wilma apply(Fred x);
}

class AddOne implements Function<Integer,Integer> {
    public Integer apply(Integer i) {
        return i+1;
    }
}


class StringLength implements Function<String,Integer> {
    public Integer apply(String s) {
        return s.length();
    }
}

class MoveDiagonal implements Function<Pair<Integer,Integer>, Pair<Integer,Integer>> {
    public Pair<Integer,Integer> apply(Pair<Integer,Integer> p) {
        return new Pair<>(p.left + 1, p.right + 1);
    }
}

class Swap<X,Y> implements Function<Pair<X,Y>, Pair<Y,X>> {
    public Pair<Y,X> apply(Pair<X,Y> p) {
        return new Pair<>(p.right, p.left);
    }
}



// Interp: a predicate recognizing strings less than 100 characters
class ShortString implements Predicate<String> {
    public Boolean test(String s) {
        return s.length() < 100;
    }
}

// Interp: a predicate recognizing the integer 5
class EqualsFive implements Predicate<Integer> {
    public Boolean test(Integer i) {
        return i == 5;
    }
}

// Interp: a predicate recognizing integers greater than 8
class BiggerThanEight implements Predicate<Integer> {
    public Boolean test(Integer i) {
        return i > 8;
    }
}

interface Lo<X> {

    // Lo<Integer> specific methods:

    // Sum this list of integers
    // Integer sum();

    // Add 1 to each element of this list
    // Lo<Integer> addOne();

    // Keep even elements of this list
    // Lo<Integer> keepEven();

    // Does this list contain 5?
    // Boolean containsFive();

    Boolean ormap(Predicate<X> p);

    Boolean andmap(Predicate<X> p);

    // map : [Listof X] [X -> Y] -> [Listof Y]
    <Y> Lo<Y> map(Function<X,Y> f);

    // Add given element to the front of this list
    Lo<X> cons(X x);

    // Count the number of elements in this list
    Integer length();

    // Reverse the elements of this list
    Lo<X> rev();

    // Reverse the elements of this list and append with a.
    // INVARIANT: this.revAcc(a) == this.rev().app(a)
    Lo<X> revAcc(Lo<X> a);

    // Append this list and the given list
    Lo<X> app(Lo<X> that);

    // Zip together this list and given list into a list of pairs
    // Ending with whichever list is short
    <Y> Lo<Pair<X,Y>> zip(Lo<Y> ys);

    // Zip together this list and given cons into a list of pairs
    <Y> Lo<Pair<Y,X>> zipCons(Cons<Y> c);

}

abstract class ALo<X> implements Lo<X> {
    public Lo<X> cons(X x) { return new Cons<X>(x, this); }
}

class Empty<X> extends ALo<X> {

    public <Y> Lo<Y> map(Function<X,Y> f) {
        return new Empty<>();
    }

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
}

class Cons<X> extends ALo<X> {
    X first;
    Lo<X> rest;

    Cons(X first, Lo<X> rest) {
        this.first = first;
        this.rest = rest;
    }

    public <Y> Lo<Y> map(Function<X,Y> f) {
        return new Cons<>(f.apply(this.first),
                this.rest.map(f));
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
}

//-----------------------------------------------------------------------------
// Tests

class Tests {

    Lo<Integer> l1 = new Cons<>(1, new Cons<>(2, new Cons<>(3, new Empty<>())));
    Lo<String> l2 = new Cons<>("1", new Cons<>("2", new Cons<>("3", new Empty<>())));


    Boolean testLo(Tester t) {

        Lo<Integer> l0 = new Empty<>();
        Lo<Integer> l1 = l0.cons(3).cons(2).cons(1);
        Lo<Integer> l2 = l0.cons(3).cons(5).cons(1);

        /*
        class Sq implements Function<Integer,Integer> {
            public Integer apply(Integer i) {
                return i*i;
            }
        }
        */

        /*
        Function<Integer,Integer> sq =
                new Function<Integer, Integer>() {
                    public Integer apply(Integer i) {
                        return i*i;
                    }
                };
                */

        Function<Integer,Integer> sq = (i -> i*i); // (define sq (lambda (i) (* i i))

        t.checkExpect(l0.map(i -> i+1), l0);
        t.checkExpect(l1.map(i -> i+1), l0.cons(4).cons(3).cons(2));

        t.checkExpect(l0.map(i -> i*i), l0);
        t.checkExpect(l1.map(i -> i*i), l0.cons(9).cons(4).cons(1));





        /*
        t.checkExpect(l0.filter(new EqualsFive()), l0);
        t.checkExpect(l2.filter(new EqualsFive()), mt.cons(5));
         */

        return true;
    }


    Boolean testLoInteger(Tester t) {

        LoInteger mt = new EmptyLoInteger();
        LoInteger l1 = new ConsLoInteger(1, new ConsLoInteger(2, new ConsLoInteger(3, mt)));
        LoInteger l2 = new ConsLoInteger(1, new ConsLoInteger(5, new ConsLoInteger(3, mt)));
        LoInteger l3 = new ConsLoInteger(1, new ConsLoInteger(15, new ConsLoInteger(3, mt)));

        t.checkExpect(mt.sum(), 0);
        t.checkExpect(mt.addOne(), mt);
        t.checkExpect(mt.keepEven(), mt);
        t.checkExpect(mt.containsFive(), false);

        t.checkExpect(l1.sum(), 6);
        t.checkExpect(l1.addOne(),
                new ConsLoInteger(2, new ConsLoInteger(3, new ConsLoInteger(4, mt))));
        t.checkExpect(l1.addFive(),
                new ConsLoInteger(6, new ConsLoInteger(7, new ConsLoInteger(8, mt))));
        t.checkExpect(l1.keepEven(), new ConsLoInteger(2, mt));
        t.checkExpect(l1.containsFive(), false);

        t.checkExpect(l2.sum(), 9);
        t.checkExpect(l2.addOne(),
                new ConsLoInteger(2, new ConsLoInteger(6, new ConsLoInteger(4, mt))));
        t.checkExpect(l2.keepEven(), mt);
        t.checkExpect(l2.containsFive(), true);


        t.checkExpect(l2.contains(i -> i == 5), true);
        t.checkExpect(l1.contains(i -> i == 5), false);

        t.checkExpect(l2.contains(new BiggerThanEight()), false);
        t.checkExpect(l1.contains(new BiggerThanEight()), false);
        t.checkExpect(l3.contains(new BiggerThanEight()), true);


        return true;
    }

    Boolean testZip(Tester t) {
        return t.checkExpect(l1.zip(l1),
                new Cons<>(new Pair<>(1,1),
                        new Cons<>(new Pair<>(2,2),
                            new Cons<>(new Pair<>(3, 3),
                                    new Empty<>())))) &&
                t.checkExpect(l1.zip(l2),
                        new Cons<>(new Pair<>(1,"1"),
                                new Cons<>(new Pair<>(2,"2"),
                                        new Cons<>(new Pair<>(3, "3"),
                                                new Empty<>()))));
    }


    Boolean testApp(Tester t) {
        return t.checkExpect(l1.app(l1),
                new Cons<>(1, new Cons<>(2, new Cons<>(3, l1))));
    }

    Boolean testRev(Tester t) {
        return t.checkExpect(l1.rev(), new Cons<>(3, new Cons<>(2, new Cons<>(1, new Empty<>()))));
    }

    Boolean testLength(Tester t) {

        Lo<Integer> myloi = new Cons<>(5, new Cons<>(4, new Cons<>(2, new Empty<>())));

        return t.checkExpect(myloi.length(), 3);

    }

    Boolean testSwap(Tester t) {
        return t.checkExpect(new Pair<Integer,String>(3, "wilma").swap(),
                new Pair<String,Integer>("wilma", 3))
                && t.checkExpect(new Pair<String,String>("fred", "wilma").swap(),
                    new Pair<String,String>("wilma", "fred"));
    }
}