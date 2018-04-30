package edu.umd.cmsc132A;

import java.util.*;
import java.util.function.*;


public class Examples {
    public static void main(String[] args) {

    }
}

interface Operation<I, O> { // Function<I,O>
    O apply(I y);
}

interface BiOperation<I1, I2, O> {
    O apply(I1 i1, I2 i2);
}


class Addition implements BiOperation<Integer, Integer, Integer> {
    public Integer apply(Integer i, Integer j) {
        return i + j;
    }
}

// 5,"fred"   --->    "5fred"
class Weird implements BiOperation<Integer, String, String> {
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

    <Z> Z combine(BiFunction<X, Y, Z> f) {
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

    Shape r1 = new Rect(0, 0, 10, 20);
    Shape r2 = new Rect(0, 0, 10, 20);
    Shape r3 = new Rect(0, 1, 10, 20);
    Shape r4 = new Rect(0, 0, 20, 30);
    Shape r5 = r1;
    Shape r6 = new Rect(0, 0, 10, 10);

    Shape s1 = new Sqr(0, 0, 10);
    Shape s2 = new Sqr(0, 0, 10);
    Shape s3 = new Sqr(10, 20, 30);

    Boolean testAllXs(tester.Tester t) {
        Listof<Shape> ls = new Cons(r1, new Cons(s3, new Empty()));

        return true;
    }

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

    Boolean testAppend(tester.Tester t) {
        Listof<Integer> l1 = new Cons<Integer>(1, new Cons<Integer>(2, new Cons<Integer>(3, new Empty<Integer>())));
        Listof<Integer> l2 = new Cons<Integer>(4, new Cons<Integer>(5, new Cons<Integer>(6, new Empty<Integer>())));
        Listof<Integer> l3 = new Cons<Integer>(1, new Cons<Integer>(2, new Cons<Integer>(3, l2)));

        return t.checkExpect(l1.append(l2), l3);
    }

    Boolean testSameShape(tester.Tester t) {
        return t.checkExpect(r1.sameShape(r1), true) &&
                t.checkExpect(r1.sameShape(r5), true) &&
                t.checkExpect(r1.sameShape(r2), true) &&
                t.checkExpect(s1.sameShape(s1), true) &&
                t.checkExpect(s1.sameShape(s2), true) &&
                t.checkExpect(s1.sameShape(s3), false) &&

                t.checkExpect(s1.sameShape(r1), false) &&
                t.checkExpect(r1.sameShape(s1), false) &&

                // Equality:
                // - reflexive  x = x
                // - symmetric  x = y => y = x
                // - transitive x = y, y = z => x = z
                // - total      gives an answer for all values of this type

                t.checkExpect(r6.sameShape(s1), false) &&
                t.checkExpect(s1.sameShape(r6), false);
    }

    Boolean testShapeAreaVisitor(tester.Tester t) {
        Shape s1 = new Rect(1, 2, 3, 4);
        Shape s2 = new Circ(0, 0, 5);
        Shape s3 = new Combo(s1, s2);

        ShapeVisitor<Double> area = new ShapeAreaVisitor();

        return t.checkExpect(s1.accept(area), 12.0) &&
                t.checkExpect(s2.accept(area), 25.0 * Math.PI) &&
                t.checkExpect(s3.accept(area), 12.0 + 25.0 * Math.PI);
    }

    void testACounter(tester.Tester t) {
        Counter c = new Counter(0);

        c.tick();
        t.checkExpect(c.i, 1);
    }

    void testSomeCounter(tester.Tester t) {
        Counter c = new Counter(0);

        t.checkExpect(c.i, 0);
    }

    void testBank(tester.Tester t) {
        Bank b = new Bank(0);

        Person p = new Person("DVH", b);
        Person m = new Person("M", b);


        t.checkExpect(m.b.amt, 0);
        t.checkExpect(p.b.amt, 0);

        p.payday();

        t.checkExpect(m.b.amt, 10000);
        t.checkExpect(p.b.amt, 10000);
        t.checkExpect(p.name, "DVH");
        t.checkExpect(m.name, "M");

    }

    void testBookN(tester.Tester t) {
        AuthorN dvh = new AuthorN("DVH");
        AuthorN mf = new AuthorN("MF");

        AuthorN mw = new AuthorN("MW");

        BookN ror = new BookN("RoR", "1234", new Cons<>(dvh, new Cons<>(mf, new Empty<>())));
        BookN ll = new BookN("LL", "8765", new Cons<>(mf, new Empty<>()));
        BookN eopl = new BookN("EoPL", "4321", new Cons<>(mw, new Empty<>()));

        t.checkExpect(ror.authors, new Cons<>(dvh, new Cons<>(mf, new Empty<>())));
        t.checkExpect(dvh.books, new Cons<>(ror, new Empty<>()));
        t.checkExpect(mf.books, new Cons<>(ll, new Cons<>(ror, new Empty<>())));

        t.checkExpect(dvh.isConnected(dvh), true);
        t.checkExpect(dvh.isConnected(mf), true);
        t.checkExpect(mf.isConnected(dvh), true);
        t.checkExpect(mw.isConnected(dvh), false);
        t.checkExpect(mf.isConnected(mw), false);

    }

    void testBook(tester.Tester t) {

        Author dvh = new Author("DVH");
        Book ror = new Book("RoR", "1234", dvh);

        t.checkExpect(ror.author.name, "DVH");
        t.checkExpect(ror.author.book.author.book.title, "RoR");
        t.checkExpect(dvh.book, ror);

    }

    Boolean testSomething(tester.Tester t) {
        Optional<Integer> o = Optional.of(4);
        return t.checkExpect(o.get(), 4);
        /*
        Map<Integer,String> m = ....


        return t.checkExpect(m.lookup(3), Optional.of("Fred")) &&
                t.checkExpect(m.lookup(7), Optional.empty());

        return t.checkExpect(m.lookup(3).get(), "Fred") &&
                t.checkExpect(m.lookup(7).isPresent(), false);
        */
    }

    void testListHelper(tester.Tester t) {
        Listof<Pairof<String, Integer>> ls =
                new Cons<>(new Pairof<>("a", 3),
                        new Cons<>(new Pairof<>("b", 4),
                                new Empty<>()));

        ListHelper lh = new ListHelper();

        t.checkExpect(lh.buildCopies(ls).foldr(String::concat, ""),
                "aaabbbb");
    }

    void testHTList(tester.Tester t) {
        HTBucketList<Integer, String> ht = new HTBucketList<Integer, String>();
        t.checkExpect(ht.get(0).isPresent(), false);
        ht.put(0, "zero");
        t.checkExpect(ht.get(0).isPresent(), true);
        t.checkExpect(ht.get(0).get(), "zero");
        t.checkExpect(ht.put(0, "nil").get(), "zero");
        t.checkExpect(ht.get(0).isPresent(), true);
        t.checkExpect(ht.get(0).get(), "nil");

        ht.put(2000, "fundred");
        t.checkExpect(ht.get(2000).get(), "fundred");
        t.checkExpect(ht.get(0).get(), "nil");

        ht.put(1000, "thousy");
        t.checkExpect(ht.get(1000).get(), "thousy");
        t.checkExpect(ht.get(2000).get(), "fundred");
        t.checkExpect(ht.get(0).get(), "nil");
        t.checkExpect(ht.size, 2000);

        for (HTList<Integer, String> b : ht.buckets) {
            t.checkExpect(b.length() <= ht.maxBucketSize, true);
        }


        ArrayList<String> al = new ArrayList<>();
        al.add("four");
        t.checkExpect(al.get(0), "four");
        al.add("fred");
        t.checkExpect(al.get(1), "fred");
        al.add(0, "wilma");
        t.checkExpect(al.get(2), "fred");
        al.set(0, "barney");
        t.checkExpect(al.get(2), "fred");

        Listof<String> strs = new Cons<>("fred", new Cons<>("wilma", new Empty<>()));

        for (String s : strs) {
            System.out.println(s);
        }

    }

    void testIsSorted(tester.Tester t) {
        Listof<Integer> ls0 = new Empty<>();
        Listof<Integer> ls1 = new Cons<>(1, new Cons<>(2, new Cons<>(3, new Empty<>())));
        Listof<Integer> ls2 = new Cons<>(3, new Cons<>(2, new Cons<>(1, new Empty<>())));

        Comparator<Integer> lte = (i, j) -> i - j;
        Comparator<Integer> gte = (i, j) -> j - i;

        t.checkExpect(ls0.isSorted(lte), true);
        t.checkExpect(ls1.isSorted(lte), true);
        t.checkExpect(ls2.isSorted(lte), false);

        t.checkExpect(ls0.isSorted(gte), true);
        t.checkExpect(ls1.isSorted(gte), false);
        t.checkExpect(ls2.isSorted(gte), true);
    }

}


// A [Listof X] is one:
// - '()
// - (cons X [Listof X])

interface Listof<X> extends Iterable<X> {
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

    // Append this list and xs
    Listof<X> append(Listof<X> xs);

    // Apply f to each element (for effect)
    // void forEach(Consumer<X> f);

    <R> R accept(ListVisitor<R, X> v);

    Boolean exists(Predicate<X> p);

    // Is this list sorted in ascending order according to c?
    Boolean isSorted(Comparator<X> c);

    // Is Cons(prior, this) sorted in ascending order according to c
    // ACCUM: element occurring immediately before this list
    Boolean isSortedAcc(Comparator<X> c, X prior);
}

class ListofIterator<X> implements Iterator<X> {
    Listof<X> xs;

    ListofIterator(Listof<X> xs) {
        this.xs = xs;
    }

    public X next() {
        return this.xs.accept(new ListVisitor<X, X>() {
            public X visitEmpty(Empty<X> empty) {
                throw new RuntimeException("no next element");
            }

            public X visitCons(Cons<X> cons) {
                xs = cons.rest;
                return cons.first;
            }
        });
    }

    public boolean hasNext() {
        return xs.accept(new ListVisitor<Boolean, X>() {
            public Boolean visitEmpty(Empty<X> e) {
                return false;
            }

            public Boolean visitCons(Cons<X> c) {
                return true;
            }
        });
    }
}

interface ListVisitor<R, X> {
    R visitEmpty(Empty<X> e);

    R visitCons(Cons<X> c);
}

abstract class AListof<X> implements Listof<X> {
    public <Y> Listof<Y> map(Function<X, Y> f) {
        return this.foldr(((X x, Listof<Y> ys) -> (new Cons<Y>(f.apply(x), ys))),
                new Empty<Y>());
    }

    // Append this list and xs
    public Listof<X> append(Listof<X> xs) {
        return this.foldr(Cons<X>::new, xs);
    }

    public Iterator<X> iterator() {
        return new ListIterator<X>(this);
    }
}

class Empty<X> extends AListof<X> {

    public <R> R accept(ListVisitor<R, X> v) {
        return v.visitEmpty(this);
    }

    public Integer length() {
        return 0;
    }

    public <Y> Listof<Pairof<X, Y>> zip(Listof<Y> ls2) {
        //
        return new Empty<Pairof<X, Y>>();
    }

    public <Y> Listof<Pairof<Y, X>> zipCons(Y firstLs1, Listof<Y> restLs1) {
        //
        return new Empty<Pairof<Y, X>>();
    }

    public <Y> Y foldr(BiFunction<X, Y, Y> f, Y b) {
        return b;
    }

    public Boolean exists(Predicate<X> p) {
        return false;
    }

    // Is this empty list sorted in ascending order according to c?
    public Boolean isSorted(Comparator<X> c) {
        return true;
    }

    // Is Cons(prior, Empty) sorted in ascending order according to c?
    // ACCUM: element occurring immediately before this list
    public Boolean isSortedAcc(Comparator<X> c, X prior) {
        return true;
    }
}

class Cons<X> extends AListof<X> {
    X first;
    Listof<X> rest;

    Cons(X first, Listof<X> rest) {
        this.first = first;
        this.rest = rest;
    }

    public <R> R accept(ListVisitor<R, X> v) {
        //
        return v.visitCons(this);
    }

    public Integer length() {
        //
        return 1 + this.rest.length();
    }

    public <Y> Listof<Pairof<X, Y>> zip(Listof<Y> ls2) {
        //
        return ls2.zipCons(this.first, this.rest);
    }

    public <Y> Listof<Pairof<Y, X>> zipCons(Y firstLs1, Listof<Y> restLs1) {
        return new Cons<Pairof<Y, X>>(new Pairof<Y, X>(firstLs1, this.first),
                restLs1.zip(this.rest));
    }

    public <Y> Y foldr(BiFunction<X, Y, Y> f, Y b) {
        //
        return f.apply(this.first, this.rest.foldr(f, b));
    }

    public Boolean exists(Predicate<X> p) {
        //
        return p.test(this.first) || this.rest.exists(p);
    }

    // Is this non empty list sorted in ascending order according to c?
    public Boolean isSorted(Comparator<X> c) {
        return this.rest.isSortedAcc(c, this.first);
    }

    // Is Cons(prior, this) sorted in ascending order according to c?
    // ACCUM: element occurring immediately before this list
    public Boolean isSortedAcc(Comparator<X> c, X prior) {
        return c.compare(prior, this.first) <= 0 &&
                this.rest.isSortedAcc(c, this.first);
    }
}

class LengthVisitor<X> implements ListVisitor<Integer, X> {
    public Integer visitEmpty(Empty<X> e) {
        return 0;
    }

    public Integer visitCons(Cons<X> c) {
        return 1 + c.rest.accept(this);
    }
}

class AppendStringVisitor implements ListVisitor<String, String> {
    public String visitEmpty(Empty<String> e) {
        return "";
    }

    public String visitCons(Cons<String> c) {
        return c.first.concat(c.rest.accept(this));
    }
}

