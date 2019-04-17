package edu.umd.cmsc132A;

import tester.Tester;

// import java.lang.Comparable;
import java.util.*;


// import java.util.Optional;


import java.util.function.Function;
import java.util.function.BiFunction;
import java.util.function.Consumer;
// import java.util.function.Predicate;

public class Lab9 {

    public static void main(String[] args) {

        Name d = new Name("D", "VH");
        Name p = new Name("D", "P");
        // System.out.println(d.greeting());

        d.toString();

        Integer i = 100;

        System.out.println(d.toString());
        System.out.println(p.toString());

        // while (condition) { ... }
        /*
        while (i >= 0) {
            System.out.println(i + " bottles of pop on the wall"));
            i = i - 1;
        }
        */

        // for (stmt1; condition; stmt2) { ... }
        // stmt1;
        // while (condition) { ...; stmt2 }

        for (Integer j = 100; j >= 0; j--)
            System.out.println(j + " bottles of pop on the wall");


        ArrayList<String> ar1 = new ArrayList<>();
        ArrayList<Integer> ar2 = new ArrayList<>();


        ar1.add("A");
        ar1.add("B");
        ar1.add("C");

        ar2.add(1);

        for (String s : ar1) System.out.println(s);

        Lo<String> ls1 = new Cons<>("A", new Cons<>("B", new Cons<>("C", new Empty<>())));

        // for (String s : ls1) System.out.println(s);
        // for (String s : ls1) System.out.println(s);


        Iterator<String> si = ls1.iterator();

        while (si.hasNext()) {
            String s = si.next();
            System.out.println(s);
        }

        System.out.println(si.hasNext());
        // System.out.println(si.next());

        si = ls1.iterator();
        System.out.println(si.hasNext());

        while (si.hasNext()) {
            String s = si.next();
            System.out.println(s);
        }


        for (Integer j : new Range(0, 100)) {
            System.out.println(j + " bottles of pop on the wall");
        }

    }
}

class Range implements Iterable<Integer> {
    Integer lo;
    Integer hi;
    Range(Integer lo, Integer hi) {
        this.lo = lo;
        this.hi = hi;
    }
    public Iterator<Integer> iterator() { return new RangeIterator(lo, hi); }
}

class RangeIterator implements Iterator<Integer> {
    Integer curr;
    Integer max;
    RangeIterator(Integer curr, Integer max) {
        this.curr = curr;
        this.max = max;
    }

    public boolean hasNext() {
        return this.curr < this.max;
    }

    public Integer next() {
        return this.curr + 1;

        /*
        Integer m = this.curr;
        this.curr = this.curr + 1;
        return m;
        */
    }
}


class Naturals implements Iterable<Integer> {
    public Iterator<Integer> iterator() { return new NatIterator(); }
}

class NatIterator implements Iterator<Integer> {
    Integer n = 0;

    public Integer next() {
        Integer m = this.n;
        this.n = this.n + 1;
        return m;
    }

    public boolean hasNext() { return true; }

}



interface Y extends Comparable<Y> {
    Boolean isB();
}

class A implements Y, Comparable<Y> {
    public int compareTo(Y y) {
        if (y.isB()) {
            return -1;
        } else {
            return 0;
        }
    }

    public Boolean isB() {
        return false;
    }
}
class B implements Y, Comparable<Y> {
    public int compareTo(Y y) {
        if (y.isB()) {
            return 0;
        } else {
            return 1;
        }
    }

    public Boolean isB() {
        return true;
    }
}



class LongerName implements Comparator<Name> {
    public int compare(Name n1, Name n2) {
        return (n1.first.length() + n1.last.length()) - (n2.first.length() + n2.last.length());
    }
}

class AlphabeticLastName implements Comparator<Name> {

    public int compare(Name n1, Name n2) {
        return n1.last.compareTo(n2.last);
    }
}


class LiftOrdering<X extends Comparable<X>> implements Comparator<X> {
    public int compare(X x1, X x2) {
        return x1.compareTo(x2);
    }
}


class Name extends Object  implements Comparable<Name> {
    String first;
    String last;
    Name(String first, String last) {
        this.first = first;
        this.last = last;
    }

    public String toString() {
        return this.first + " " + this.last + ", aka " + super.toString();
    }


    public static String opening() {
        return "Dear ";
    }

    // Name.opening()


    public int compareTo(Name that) {
        return (this.first.length() + this.last.length()) - (that.first.length() + that.last.length());
    }


    public String greeting() {
        return Name.opening() + this.first + " " + this.last;
    }

    public String fullname() {
        return this.first + " " + this.last;
    }

    public String fullname(String prefix) {
        return prefix + " " + this.fullname();
    }

    public String fullname(Integer i) {
        return i.toString() + " " + this.fullname();
    }

    public String fullname(Object o) {
        return "object";
    }







    public boolean equals(Object n) {
        if (n instanceof Name) {
            return this.equals((Name) n);
        } else {
            return false;
        }
    }

    // public boolean equals(Object n) { ... }

    public boolean equals(Name n) {

        Integer x = 1/0;

        return this.first.equals(n.first) && this.last.equals(n.last);
    }



    // if o1.hashCode() != o2.hashCode(), then o1.equals(o2) is false.
    // A LIE: if o1.hashCode() == o2.hashCode(), then o1.equals(o2) is true.
    // if o1.equals(o2) is true, then o1.hashCode() == o2.hashCode().

    // new Name("Tom", "VH").hashCode()
    // new Name("Sam", "T").hashCode()

    /*
    public int hashCode() {

        return this.first.length() + this.last.length();
    }
    */


}


//-----------------------------------------------------------------------------
// Tests

class Tests {

    Lo<Integer> l1 = new Cons<>(1, new Cons<>(2, new Cons<>(3, new Empty<>())));
    Lo<String> l2 = new Cons<>("1", new Cons<>("2", new Cons<>("3", new Empty<>())));

    Boolean testArrayLists(Tester t) {

        List<String> is = new ArrayList<>();
        is.add("a");
        is.add("b");
        is.add("c");

        return true;
    }


    Boolean testName(Tester t) {
        Name n1 = new Name("D", "VH");
        Name n2 = new Name("D", "P");

        Lo<Name> lon = new Cons<>(n1, new Empty<>());

        Lo<String> los = new Cons<>("Dr.", new Empty<>());

        t.checkExpect(n1.equals(n2), false);
        t.checkExpect(n1.equals(n1), true);
        t.checkExpect(new Name("D", "VH").equals(new Name("D", "VH")), true);

        t.checkExpect(lon.contains(new Name("D", "VH")), true);

        t.checkExpect(n1.fullname(), "D VH");
        t.checkExpect(n1.fullname("Dr."), "Dr. D VH");
        t.checkExpect(n1.fullname(5), "5 D VH");
        t.checkExpect(n1.fullname(new Empty<Integer>()), "object");

        t.checkExpect(los.map(s -> n1.fullname(s)), new Cons<>("Dr. D VH", new Empty<>()));

        return true;
    }



    Boolean testSort(Tester t) {
        Lo<Integer> is = new Cons<>(4, new Cons<>(2, new Cons<>(1, new Empty<>())));

        Comparator<Integer> lt = (i1, i2) -> i1 - i2;
        Comparator<Integer> gt = (i1, i2) -> i2 - i1;

        t.checkExpect(is.insert(lt, 3), new Cons<>(3, is));
        t.checkExpect(is.sort(lt), new Cons<>(1, new Cons<>(2, new Cons<>(4, new Empty<>()))));
        t.checkExpect(is.sort(lt).insert(lt, 3), new Cons<>(1, new Cons<>(2, new Cons<>(3, new Cons<>(4, new Empty<>())))));
        t.checkExpect(is.sort(gt), is);
        t.checkExpect(is.sort(gt).insert(gt,3), new Cons<>(4, new Cons<>(3, new Cons<>(2, new Cons<>(1, new Empty<>())))));
        // t.checkExpect(false, true);

        return true;
    }


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


    Boolean testLengthVisitor(Tester t) {
        ListVisitor<String, Integer> len = new LengthVisitor<String>();

        Lo<String> l1 = new Cons<>("a", new Cons<>("b", new Cons<>("c", new Empty<>())));

        t.checkExpect(l1.accept(len), 3);
        // visitCons("a", new Cons("b", ...))
        // 1 + new Cons("b", ...).accept(new LengthVisitor())

        Lo<Integer> l2 = new Cons<>(8, new Cons<>(2, new Cons<>(4, new Empty<>())));

        t.checkExpect(l2.accept(new AddNListVisitor(3)),
                new Cons<>(11, new Cons<>(5, new Cons<>(7, new Empty<>()))));

        t.checkExpect(l2.accept(new AddNListVisitor(1)),
                new Cons<>(9, new Cons<>(3, new Cons<>(5, new Empty<>()))));


        t.checkExpect(l2.accept(new MapVisitor<>(i -> i*i)),
                new Cons<>(64, new Cons<>(4, new Cons<>(16, new Empty<>()))));


        return true;
    }

}




interface Comparable<X> {
    int compareTo(X x);
}

interface Predicate<T> {
    Boolean test(T t);
}

class TestToday {

    Boolean testCompable(Tester t) {

        Name n1 = new Name("D", "VH");
        Name n2 = new Name("D", "P");

        t.checkExpect(n1.compareTo(n2), 1);
        t.checkExpect(n1.compareTo(n1), 0);
        t.checkExpect(n2.compareTo(n1), -1);
        return true;
    }


    Boolean testFoo(Tester t) {

        Lo<Integer> is = new Cons<>(1, new Empty<>());

        // t.checkExpect(is.first(), Optional.of(1));

        t.checkExpect(is.first().isEmpty(), false);

        t.checkExpect(is.first().orElse(5), 1);

        t.checkExpect(Optional.empty().orElse(5), 5);

        return true;
    }
}


interface Comp<T> {
    // Is this object bigger than that?
    Boolean isBigger(T that);
}

class Wasisable {
    public Integer wasis() {
        return 100;
    }
}

class W extends Wasisable {
    public Integer wasis() {
        return -100;
    }
}



class CPair<X extends Wasisable,Y extends Comp<Y>> implements Comp<CPair<X,Y>> {
    X left;
    Y right;
    CPair(X left, Y right) {
        this.left = left;
        this.right = right;
    }

    public Boolean isBigger(CPair<X,Y> that) {
        return this.left.wasis() > 0;
        // return this.left.isBigger(that.left) || this.right.isBigger(that.right);
    }
}








class Author {
    String name;
    Lo<Book> books;
    Author(String name) {
        this.name = name;
        this.books = new Empty<>();
    }

    Boolean isConnectedAcc(Author a, Lo<Author> seen) {
        return !(seen.ormap(o -> this.equals(o))) &&
                 (this.equals(a) || this.coauthors().ormap(o -> o.isConnectedAcc(a, new Cons<>(this, seen))));
    }

    // Is this author connected to the given author via co-authorship?
    Boolean isConnected(Author a) {
        return this.isConnectedAcc(a, new Empty<>());
    }

    // Produce the list of immediate co-authors of this author
    // May include duplicates
    Lo<Author> coauthors() {
        return this.books.foldr((Book b, Lo<Author> as) -> b.auths.app(as), new Empty<>());
    }
}

class Book {
    String title;
    Lo<Author> auths;

    Book(String title, Lo<Author> auths) {
        this.title = title;
        this.auths = auths;
        // auth.book = this;
        // for each a in auths:    a.books = new Cons<>(this, a.books);
        auths.foreach(a -> a.books = new Cons<>(this, a.books));
    }
}


class Bank {
    Integer amt;
    Bank(Integer amt) {
        this.amt = amt;
    }

    // Deposit given amount in to this account
    // EFFECT: update this account with given deposit
    void deposit(Integer dep) {
        this.amt = this.amt + dep;
    }
}

class Person {
    String name;
    Bank acct;
    Person(String name, Bank acct) {
        this.name = name;
        this.acct = acct;
    }

    // Deposit a payday check in this person's bank account
    // EFFECT: deposit's money into this person's account
    void payday() {
        // this.name = "Deena";
        this.acct.deposit(10000);
    }
}

class TestBank {

    Boolean testComp(Tester t) {

/*
        Optional<Integer> oi = Optional.of(4);

        t.checkExpect(oi.isEmpty(), false);
        t.checkExpect(oi.get(), 4);

        //CPair<Name, Name> cn1 = new CPair<Name,Name>(new Name("A", "B"), new Name("Long", "Name"));
        //CPair<Name, Name> cn2 = new CPair<Name,Name>(new Name("Alfred", "B"), new Name("Shortest", "Name"));

        //t.checkExpect(cn1.isBigger(cn2), false);
        //t.checkExpect(cn2.isBigger(cn1), true);

        //CPair<Integer, String> cbogus = new CPair<Integer, String>(4, "three");

        CPair<Wasisable, Name> c1 = new  CPair<Wasisable,Name>(new Wasisable(), new Name("Long", "Name"));
        // CPair<Wasisable, Name> c1 = new  CPair<Wasisable,Name>(new W(), new Name("Long", "Name"));

*/

        return true;
    }


    Boolean testBook(Tester t) {

        // Book ror = new Book("Realm of Racket", new Author("DVH", new Book("Realm of Racket", new Author("DVH", new Book("Realm of Racket", new Author("DVH", ....)))))
        Author a = new Author("DVH");
        Author m = new Author("MF");
        Author sk = new Author("Stephen King");

        Book b = new Book("RoR", new Cons<>(a, new Cons<>(m, new Empty<>())));

        Author r = new Author("RF");

        Book htdp = new Book("HtDP", new Cons<>(m, new Cons<>(r, new Empty<>())));

        t.checkExpect(a.books, new Cons<>(b, new Empty<>()));
        t.checkExpect(b.auths, new Cons<>(a, new Cons<>(m, new Empty<>())));
        t.checkExpect(a.books, new Cons<>(b, new Empty<>()));
        t.checkExpect(m.books, new Cons<>(htdp, new Cons<>(b, new Empty<>())));

        Cons<Integer> end = new Cons<>(3, new Empty<>());
        Lo<Integer> is = new Cons<>(1, new Cons<>(2, end));

        end.rest = is;


        t.checkExpect(a.coauthors(), new Cons<>(a, new Cons<>(m, new Empty<>())));
        t.checkExpect(m.coauthors(), new Cons<>(m, new Cons<>(r, new Cons<>(a, new Cons<>(m, new Empty<>())))));

        t.checkExpect(a.isConnected(a), true);
        t.checkExpect(a.isConnected(m), true);
        t.checkExpect(a.isConnected(r), true);
        t.checkExpect(a.isConnected(sk), false);

        // Stack overflow
        // t.checkExpect(is.length(), 3);



        /*
        t.checkExpect(b.auth.book, b);
        t.checkExpect(a.book, b);
        t.checkExpect(a.book.auth.book.auth.book.auth, a);
        */

        return true;
    }


    Boolean testBank(Tester t) {
        Bank b = new Bank(0);
        Person d = new Person("DVH", b);
        Person o = new Person("SO", new Bank(0));
        t.checkExpect(b, new Bank(0));

        Bank c = new Bank(d.acct.amt);
        // Bank c = d.acct;

        d.payday();


        c = d.acct;
/*
        t.checkExpect(c.amt, 0);


        t.checkExpect(b, new Bank(10000));

        t.checkExpect(d.name, "DVH");
        t.checkExpect(o.name, "SO");
        t.checkExpect(d.acct.amt, 10000);
        t.checkExpect(b.amt, 10000);
        t.checkExpect(o.acct.amt, 0);
        // t.checkExpect(o.acct.amt, 0);
*/
        return true;
    }

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

interface ListVisitor<X,R> {
    R visitEmpty();
    R visitCons(X first, Lo<X> rest);
}


class LengthVisitor<X> implements ListVisitor<X, Integer> {
    public Integer visitEmpty() {
        return 0;
    }
    public Integer visitCons(X first, Lo<X> rest) {
        return 1 + rest.accept(this);
    }
}

class SumVisitor implements ListVisitor<Integer, Integer> {
    public Integer visitEmpty() {
        return 0;
    }

    public Integer visitCons(Integer first, Lo<Integer> rest) {
        return first + rest.accept(this);
    }
}

/*
;; add-one-to-all : [Listof Integer] -> [Listof Integer]
(define (add-one-to-all loi)
  (cond [(empty? loi) '()]
        [(cons? loi)
         (cons (add1 (first loi)) (add-one-to-all (rest loi)))]))
*/
class AddOneListVisitor implements ListVisitor<Integer, Lo<Integer>> {
    public Lo<Integer> visitEmpty() {
        return new Empty<>();
    }

    public Lo<Integer> visitCons(Integer first, Lo<Integer> rest) {
        return new Cons<>(first + 1, rest.accept(this));
    }
}

/*
;; add-n-to-all : [Listof Integer] Integer -> [Listof Integer]
(define (add-n-to-all loi n)
  (cond [(empty? loi) '()]
        [(cons? loi)
         (cons (+ (first loi) n) (add-n-to-all (rest loi) n))]))
*/
class AddNListVisitor implements ListVisitor<Integer, Lo<Integer>> {
    Integer n;

    AddNListVisitor(Integer n) {
        this.n = n;
    }

    public Lo<Integer> visitEmpty() {
        return new Empty<>();
    }

    public Lo<Integer> visitCons(Integer first, Lo<Integer> rest) {
        return new Cons<>(first + this.n, rest.accept(this));
    }
}

class MapVisitor<X,Y> implements ListVisitor<X, Lo<Y>> {

    Function<X,Y> f;

    MapVisitor(Function<X,Y> f) {
        this.f = f;
    }

    public Lo<Y> visitEmpty() { return new Empty<>(); }

    public Lo<Y> visitCons(X first, Lo<X> rest) {
        return new Cons<>(f.apply(first), rest.accept(this));
    }
}




/*
// Node

    // Apply f to every element of this BST and collect results as a BST
    // NOTE: cannot assume f is monotonic
    public <R extends Comparable<R>> BST<R> map(Function<X, R> f) {
        return new Node<>(f.apply(this.val), this.left.map(f), this.right.map(f));
    }


    // a function f is monotonic if x > y, then f(x) > f(y).

    // a function f is monotonic if x.compareTo(y) > 0, then f.apply(x).compareTo(f.apply(y)) > 0.

*/


class LoIterator<X> implements Iterator<X> {
    Lo<X> elems;
    LoIterator(Lo<X> elems) {
        this.elems = elems;
    }

    public boolean hasNext() {
        return this.elems.foldr((b, y) -> true, false);
    }

    public X next() {
        X f = this.elems.first().get();
        this.elems = this.elems.rest().get();
        return f;
    }
}

class ListofIterator<X> implements Iterator<X> {

    Lo<X> elems;
    ListofIterator(Lo<X> elems) {
        this.elems = elems;
    }


    public boolean hasNext() {
        // return this.elems.first().isPresent();
        return this.elems.foldr((b, y) -> true, false);
    }


    public X next() {
        X f = this.elems.first().get();
        this.elems = this.elems.rest().get();
        return f;
    }

}



interface Lo<X> extends Iterable<X> {

    // Produce an iterator for the elements of this list
    Iterator<X> iterator();

    // Does this list contain that element?
    Boolean contains(X that);


    // Sort the elements of this list in ascending order according to c.
    Lo<X> sort(Comparator<X> c);

    // Insert given element into this (sorted) list to produce a sorted list
    // ASSUME: this list is sorted according to c
    Lo<X> insert(Comparator<X> c, X x);

    // Get the first element if there is one
    Optional<X> first();

    // Get the rest of the list if there is one
    Optional<Lo<X>> rest();

    // Accept a given visitor and compute with this list
    <R> R accept(ListVisitor<X,R> v);

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
    <Y> Lo<Y> map(java.util.function.Function<X,Y> f);

    // Run given command on each element in this list
    void foreach(Consumer<X> f);

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

    <Y> Y foldr(BiFunction<X,Y,Y> f, Y y);

}

abstract class ALo<X> implements Lo<X> {
    public Lo<X> cons(X x) { return new Cons<X>(x, this); }

    public Boolean contains(X that) {
        return this.ormap(y -> y.equals(that));
    }


    public ListofIterator<X> iterator() { return new ListofIterator<>(this); }

}

class Empty<X> extends ALo<X> {


    public Lo<X> sort(Comparator<X> c) {
        return this;
    }

    public Lo<X> insert(Comparator<X> c, X x) {
        return new Cons<>(x, this);
    }

    public Optional<X> first() { return Optional.empty(); }

    public Optional<Lo<X>> rest() { return Optional.empty(); }

    public <R> R accept(ListVisitor<X,R> v) {
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

class Cons<X> extends ALo<X> {
    X first;
    Lo<X> rest;

    Cons(X first, Lo<X> rest) {
        this.first = first;
        this.rest = rest;
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

    public <R> R accept(ListVisitor<X,R> v) {
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

