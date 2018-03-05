package edu.umd.cmsc132A;
import java.util.function.*;

public class Examples {

    public static void main(String[] args) {
        System.out.println("Hello World!");
    }
}

interface Operation<I, O> { // Function<I,O>
    O apply(I y);
}

interface BiOperation<I1, I2, O> {
    O apply(I1 i1, I2 i2);
}


class Addition implements BiOperation<Integer,Integer,Integer> {
    public Integer apply(Integer i, Integer j) {
        return i+j;
    }
}

// 5,"fred"   --->    "5fred"
class Weird implements BiOperation<Integer,String,String> {
    public String apply(Integer i, String s) {
        return i.toString().concat(s);
    }
}

// new Addition() "is the same as" (i,j) -> i+j

interface LoI {
    // Sum all the numbers in this list
    Integer sum();

    // Multiply all the numbers in this list
    Integer prod();

    // Count the elements in this list
    Integer length();

    // Zip together this list and given list into a list of pairs
    // Stop zipping at end of shortest list
    Listof<PairofInt> zip(LoI ls2);

    // Zip this list on the right of the given cons
    Listof<PairofInt> zipCons(Integer firstLs1, LoI restLs1);

    // Add 1 to every element in this list
    LoI addone();

    // Square each element in this list
    LoI square();

    // Apply f to each element in this list
    LoI map(Function<Integer, Integer> f);   // (....a method on Integers.... [Integer -> Integer])

    // Add n to each element of this list
    LoI addN(Integer n);
}

abstract class ALoI implements LoI {
    public LoI addone() {
        return this.map(new AddN(1));
    }
}

class EmptyLoI extends ALoI {
    EmptyLoI() {
    }

    public Integer sum() {
        return 0;
    }

    public Integer prod() {
        return 1;
    }

    public Integer length() {
        return 0;
    }

    public Listof<PairofInt> zip(LoI ls2) {
        return new Empty<PairofInt>();
    }

    public Listof<PairofInt> zipCons(Integer firstLs1, LoI restLs1) {
        return new Empty<PairofInt>();
    }

    public LoI addfive() {
        return this.map(new AddN(5));
    }

    public LoI square() {
        return this.map(new Square());
    }

    public LoI addN(Integer n) {
        return this.map(new AddN(n));
    }

    public LoI map(Function<Integer, Integer> f) {
        return this;
    }
}

class ConsLoI extends ALoI {
    Integer first;
    LoI rest;

    ConsLoI(Integer first, LoI rest) {
        this.first = first;
        this.rest = rest;
    }

    public Integer sum() {
        return this.first + this.rest.sum();
    }

    public Integer prod() {
        return this.first * this.rest.prod();
    }

    public Integer length() {
        return 1 + this.rest.length();
    }

    public Listof<PairofInt> zip(LoI ls2) {
        return ls2.zipCons(this.first, this.rest);
    }

    public Listof<PairofInt> zipCons(Integer firstLs1, LoI restLs1) {
        return new Cons<PairofInt>(new PairofInt(firstLs1, this.first),
                restLs1.zip(this.rest));
    }

    public LoI addfive() {
        return this.map(new AddN(5));
    }


    public LoI addN(Integer n) {
        return this.map(new AddN(n));
    }

    public LoI square() {
        return this.map(new Square());
    }

    public LoI map(Function<Integer, Integer> f) {
        return new ConsLoI(f.apply(this.first), this.rest.map(f));
    }
}

class AddN implements Function<Integer, Integer> {
    Integer n;

    AddN(Integer n) {
        this.n = n;
    }

    public Integer apply(Integer i) {
        return this.n + i;
    }
}

class Square implements Function<Integer, Integer> {
    public Integer apply(Integer i) {
        return i * i;
    }
}

class PairofInt {
    Integer left;
    Integer right;

    PairofInt(Integer left, Integer right) {
        this.left = left;
        this.right = right;
    }
}

class Pairof<X, Y> {
    X left;
    Y right;

    Pairof(X left, Y right) {
        this.left = left;
        this.right = right;
    }

    <Z> Z combine(BiFunction<X,Y,Z> f) {
        return f.apply(this.left, this.right);
    }
}

interface LoS {
    // Append all the strings in this list together
    String append();

    // Count the elements in this list
    Integer length();

    // Append given string to each element in this list
    LoS appendEach(String s);
}

class EmptyLoS implements LoS {
    public String append() {
        return "";
    }

    public Integer length() {
        return 0;
    }

    public LoS appendEach(String s) {
        return new EmptyLoS();
    }

}

class ConsLoS implements LoS {
    String first;
    LoS rest;

    ConsLoS(String first, LoS rest) {
        this.first = first;
        this.rest = rest;
    }

    public String append() {
        return this.first.concat(this.rest.append());
    }

    public Integer length() {
        return 1 + this.rest.length();
    }

    public LoS appendEach(String s) {
        return new ConsLoS(this.first.concat(s), this.rest.appendEach(s));
    }


}

class Fred {
    LoI eg1 = new ConsLoI(5, new ConsLoI(6, new ConsLoI(7, new EmptyLoI())));

    Boolean testLoILength(tester.Tester t) {
        return t.checkExpect(new EmptyLoI().length(), 0)
                && t.checkExpect(eg1.length(), 3);
    }

    Boolean testLoISum(tester.Tester t) {
        return t.checkExpect(new EmptyLoI().sum(), 0, "sum empty")
                && t.checkExpect(eg1.sum(), 18);
    }

    Boolean testListLength(tester.Tester t) {

        Listof<Integer> mtLoI = new Empty<Integer>();
        Listof<Integer> onetwothree = new Cons<Integer>(1,
                new Cons<Integer>(2,
                        new Cons<Integer>(3, new Empty<Integer>())));


        Listof<String> mtLoS = new Empty<String>();
        Listof<String> onetwothreeString = new Cons<String>("one",
                new Cons<String>("two",
                        new Cons<String>("three", new Empty<String>())));

        return t.checkExpect(mtLoI.length(), 0) &&
                t.checkExpect(mtLoS.length(), 0) &&
                t.checkExpect(onetwothree.length(), 3) &&
                t.checkExpect(onetwothreeString.length(), 3);

    }

    Boolean testCheckWithin(tester.Tester t) {
        // How to test with floating point numbers
        return t.checkInexact(1.0 + 2.5, 3.5, 0.0001);
    }

    Boolean testZip(tester.Tester t) {
        LoI ls1 = new ConsLoI(1, new ConsLoI(2, new ConsLoI(3, new EmptyLoI())));
        LoI ls2 = new ConsLoI(4, new ConsLoI(5, new ConsLoI(6, new EmptyLoI())));

        LoI ls3 = new ConsLoI(1, new ConsLoI(2, new EmptyLoI()));
        LoI ls4 = new ConsLoI(4, new ConsLoI(5, new EmptyLoI()));

        Listof<PairofInt> ls1ls2 = new Cons<PairofInt>(new PairofInt(1, 4),
                new Cons<PairofInt>(new PairofInt(2, 5),
                        new Cons<PairofInt>(new PairofInt(3, 6),
                                new Empty<PairofInt>())));

        Listof<PairofInt> ls3ls2 = new Cons<PairofInt>(new PairofInt(1, 4),
                new Cons<PairofInt>(new PairofInt(2, 5),
                        new Empty<PairofInt>()));

        Listof<PairofInt> ls1ls4 = ls3ls2; // Same result

        return t.checkExpect(ls1.zip(ls2), ls1ls2)
                && t.checkExpect(ls3.zip(ls2), ls3ls2)
                && t.checkExpect(ls1.zip(ls4), ls1ls4);
    }

    Boolean testAddone(tester.Tester t) {

        LoI empty = new EmptyLoI();
        LoI onetwothree = new ConsLoI(1, new ConsLoI(2, new ConsLoI(3, empty)));
        LoI twothreefour = new ConsLoI(2, new ConsLoI(3, new ConsLoI(4, empty)));

        return t.checkExpect(empty.addone(), empty)
                && t.checkExpect(onetwothree.addone(), twothreefour);
    }

    Boolean testSquare(tester.Tester t) {

        LoI empty = new EmptyLoI();
        LoI onetwothree = new ConsLoI(1, new ConsLoI(2, new ConsLoI(3, empty)));
        LoI onefournine = new ConsLoI(1, new ConsLoI(4, new ConsLoI(9, empty)));

        return t.checkExpect(empty.square(), empty)
                && t.checkExpect(onetwothree.square(), onefournine);
    }

    Boolean testMap(tester.Tester t) {
        LoI onetwothree = new ConsLoI(1, new ConsLoI(2, new ConsLoI(3, new EmptyLoI())));

        Listof<Integer> ott = new Cons<Integer>(1, new Cons<Integer>(2, new Cons<Integer>(3, new Empty<Integer>())));

        Listof<Integer> ottsq = ott.map(i -> i * i);

        return t.checkExpect(ott.map(new AddN(7)),
                new Cons<Integer>(8, new Cons<Integer>(9, new Cons<Integer>(10, new Empty<Integer>())))) &&

                t.checkExpect(ott.map(i -> i * i),
                        new Cons<Integer>(1, new Cons<Integer>(4, new Cons<Integer>(9, new Empty<Integer>())))) &&

                t.checkExpect(onetwothree.map(i -> i * i),
                        new ConsLoI(1, new ConsLoI(4, new ConsLoI(9, new EmptyLoI()))));


    }


    Boolean testFoldr(tester.Tester t) {
        Listof<String> ot = new Cons<String>("one", new Cons<String>("two", new Empty<String>()));

        return t.checkExpect(ot.foldr((s, r) -> s.concat(r), ""), "onetwo") &&
                t.checkExpect(ot.foldr((s, n) -> n + 1, 0), 2);

    }
}

// A [Listof X] is one:
// - '()
// - (cons X [Listof X])

interface Listof<X> {
    // Compute the length of this list
    Integer length();

    // Zip together this list and given list into a list of pairs
    // Stop zipping at end of shortest list
    <Y> Listof<Pairof<X, Y>> zip(Listof<Y> ls2);

    <Y> Listof<Pairof<Y, X>> zipCons(Y firstLs1, Listof<Y> restLs1);


    // map : [Listof X] [X -> Y] -> [Listof Y]
    <Y> Listof<Y> map(Function<X, Y> f);

    // foldr : [Listof X] [X Y -> Y] Y -> Y
    <Y> Y foldr(BiFunction<X, Y, Y> f, Y b);
}

abstract class AListof<X> implements Listof<X> {
    public <Y> Listof<Y> map(Function<X, Y> f) {
        return this.foldr(((x, ys) -> (new Cons<Y>(f.apply(x), ys))),
                (Listof<Y>)new Empty<Y>());
    }
}

class Empty<X> extends AListof<X> {
    public Integer length() {
        return 0;
    }

    public <Y> Listof<Pairof<X, Y>> zip(Listof<Y> ls2) {
        return new Empty<Pairof<X, Y>>();
    }

    public <Y> Listof<Pairof<Y, X>> zipCons(Y firstLs1, Listof<Y> restLs1) {
        return new Empty<Pairof<Y, X>>();
    }

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

    public Integer length() {
        return 1 + this.rest.length();
    }

    public <Y> Listof<Pairof<X, Y>> zip(Listof<Y> ls2) {
        return ls2.zipCons(this.first, this.rest);
    }

    public <Y> Listof<Pairof<Y, X>> zipCons(Y firstLs1, Listof<Y> restLs1) {
        return new Cons<Pairof<Y, X>>(new Pairof<Y, X>(firstLs1, this.first),
                restLs1.zip(this.rest));
    }

    public <Y> Y foldr(BiFunction<X, Y, Y> f, Y b) {
        return f.apply(this.first, this.rest.foldr(f, b));
    }
}
