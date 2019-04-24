package edu.umd.cmsc132A;

import tester.Tester;

// import java.lang.Comparable;
import java.util.*;


// import java.util.Optional;


import java.util.function.Function;
// import java.util.function.Predicate;

public class Lecture {

    public static void main(String[] args) {

        Name d = new Name("D", "VH");
        Name p = new Name("D", "P");
        // System.out.println(d.greeting());

        d.toString();

        Integer i = 100;

        for (String s : new StrIter("abcd")) System.out.println(s);


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

// mutate an index
class StrIter implements Iterable<String>, Iterator<String> {
    String str;
    Integer i;
    StrIter(String str) {
        this.str = str;
        this.i = 0;
    }

    public Iterator<String> iterator() { return this; }
    public boolean hasNext() { return this.str.length() > i; }
    public String next() {
        String s = this.str.substring(i,i+1);
        this.i = this.i + 1;
        return s;
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
        Integer m = this.curr;
        this.curr = this.curr + 1;
        return m;
    }
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
        LoVisitor<String, Integer> len = new LengthVisitor<String>();

        Lo<String> l1 = new Cons<>("a", new Cons<>("b", new Cons<>("c", new Empty<>())));

        t.checkExpect(l1.accept(len), 3);
        // visitCons("a", new Cons("b", ...))
        // 1 + new Cons("b", ...).accept(new LengthVisitor())

        Lo<Integer> l2 = new Cons<>(8, new Cons<>(2, new Cons<>(4, new Empty<>())));

        t.checkExpect(l2.accept(new AddNLoVisitor(3)),
                new Cons<>(11, new Cons<>(5, new Cons<>(7, new Empty<>()))));

        t.checkExpect(l2.accept(new AddNLoVisitor(1)),
                new Cons<>(9, new Cons<>(3, new Cons<>(5, new Empty<>()))));


        t.checkExpect(l2.accept(new MapVisitor<>(i -> i*i)),
                new Cons<>(64, new Cons<>(4, new Cons<>(16, new Empty<>()))));


        return true;
    }

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

class LengthVisitor<X> implements LoVisitor<X, Integer> {
    public Integer visitEmpty() {
        return 0;
    }
    public Integer visitCons(X first, Lo<X> rest) {
        return 1 + rest.accept(this);
    }
}

/*
;; add-one-to-all : [Listof Integer] -> [Listof Integer]
(define (add-one-to-all loi)
  (cond [(empty? loi) '()]
        [(cons? loi)
         (cons (add1 (first loi)) (add-one-to-all (rest loi)))]))
*/
class AddOneLoVisitor implements LoVisitor<Integer, Lo<Integer>> {
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
class AddNLoVisitor implements LoVisitor<Integer, Lo<Integer>> {
    Integer n;

    AddNLoVisitor(Integer n) {
        this.n = n;
    }

    public Lo<Integer> visitEmpty() {
        return new Empty<>();
    }

    public Lo<Integer> visitCons(Integer first, Lo<Integer> rest) {
        return new Cons<>(first + this.n, rest.accept(this));
    }
}

class MapVisitor<X,Y> implements LoVisitor<X, Lo<Y>> {

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
        // return this.elems.first().isPresent();
        return this.elems.foldr((b, y) -> true, false);
    }


    public X next() {
        X f = this.elems.first().get();
        this.elems = this.elems.rest().get();
        return f;
    }

}


