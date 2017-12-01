import tester.*;

// foldr : [List X] [X Y -> Y] Y -> Y
// (foldr (list x1 x2 ... xn) f b)
// = (f x1 (f x2 ... (f xn b)))
/*
(define (foldr xs f b)
  (cond [(empty? xs) b]
        [(cons? xs)
         (f (first xs)
            (foldr (rest xs) f b))]))
*/

// Represents a list of X
    interface List<X> {
    // Sort this list of integers in ascending order.
    List<X> sort(Comparison<X> c);

    // Insert the given integer into this sorted list of integers,
    // to produce a sorted list in ascending order.
    List<X> insert(X i, Comparison<X> c);

    // Reverse the elements of this list.
    List<X> reverse();

    // Put the given element at the end of this list.
    List<X> putAtEnd(X i);

    // Fold given combiner function over the elements of this list
    <Y> Y foldr(Combiner<X,Y> combine, Y base);
}

// Represents a function that combines a given X and Y into a Y.
interface Combiner<X,Y> {
    Y apply(X x, Y y);
}

// Represents an empty list of integers
class Empty<X> implements List<X> {
    Empty() {}

    // Sort this empty list of integers in ascending order.
    public List<X> sort(Comparison<X> c) {
	return this;
    }

    public List<X> insert(X i, Comparison<X> c) {
	return new Cons<X>(i, this);
    }

    // Reverse the elements of this empty list.
    public List<X> reverse() {
	return this;
    }

    // Put the given element at the end of this empty list.
    public List<X> putAtEnd(X i) {
	return new Cons<X>(i, this);
    }

    public <Y> Y foldr(Combiner<X,Y> combine, Y base) {
	return base;
    }
}

// Represents an ordering on elements of type X.
interface Comparison<X> {
    // Is a greater than b?
    Boolean greaterThan(X a, X b);
}

class IntegerGreaterThan implements Comparison<Integer> {
    IntegerGreaterThan() {}
    public Boolean greaterThan(Integer a, Integer b) {
	return a > b;
    }
}

class Runner {
    String name;
    Integer bibnumber;
    Integer runtime; // in seconds
    Runner(String name, Integer bibnumber, Integer runtime) {
	this.name = name;
	this.bibnumber = bibnumber;
	this.runtime = runtime;
    }
}

class ShortName implements Comparison<Runner> {
    ShortName() {}
    public Boolean greaterThan(Runner a, Runner b) {
	return a.name.length() > b.name.length();
    }
}


class RunTime implements Comparison<Runner> {
    RunTime() {}
    public Boolean greaterThan(Runner a, Runner b) {
	return a.runtime > b.runtime;
    }
}

// Represents an non-empty list of integers
class Cons<X> implements List<X> {
    X first;
    List<X> rest;
    Cons(X first, List<X> rest) {
	this.first = first;
	this.rest = rest;
    }

    // Sort this non-empty list of integers in ascending order.
    public List<X> sort(Comparison<X> c) {
	return this.rest.sort(c).insert(this.first, c);
    }

    public List<X> insert(X i, Comparison<X> c) {
	if (c.greaterThan(i, this.first)) {
	    return new Cons<X>(this.first,
			       this.rest.insert(i, c));
	} else {
	    return new Cons<X>(i, this);
	}
    }

    // Reverse the elements of this non-empty list.
    public List<X> reverse() {
	return this.rest.reverse().putAtEnd(this.first);
    }

    // Put the given element at the end of this non-empty list.
    public List<X> putAtEnd(X i) {
	return new Cons<X>(this.first,
			   this.rest.putAtEnd(i));
    }

    public <Y> Y foldr(Combiner<X,Y> combine, Y base) {
	return combine.apply(this.first,
			     this.rest.foldr(combine, base));
    }
}

// Represents the "+" function.
class Plus implements Combiner<Integer,Integer> {
    Plus() {}
    public Integer apply(Integer a, Integer b) {
	return a + b;
    }
}

class Examples {
    Examples() {}

    List<Integer> mt = new Empty<Integer>();
    List<Integer> one = new Cons<Integer>(1, mt);
    List<Integer> l1 = new Cons<Integer>(4, new Cons<Integer>(3, new Cons<Integer>(1, mt)));
    List<Integer> l2 = new Cons<Integer>(2, new Cons<Integer>(3, new Cons<Integer>(1, mt)));
    List<Integer> sl1 = new Cons<Integer>(1, new Cons<Integer>(3, new Cons<Integer>(4, mt)));
    List<Integer> sl2 = new Cons<Integer>(1, new Cons<Integer>(4, mt));

    Comparison<Integer> gt = new IntegerGreaterThan();

    void testReverse(Tester t) {
	t.checkExpect(mt.reverse(), mt);
	t.checkExpect(one.reverse(), one);
	t.checkExpect(sl2.reverse(), new Cons<Integer>(4, new Cons<Integer>(1, mt)));
    }

    void testInsert(Tester t) {
	t.checkExpect(mt.insert(1, gt), one);
	t.checkExpect(sl2.insert(3, gt), sl1);
    }

    void testSort(Tester t) {
	t.checkExpect(mt.sort(gt), new Empty<Integer>());
	t.checkExpect(one.sort(gt), one);
	t.checkExpect(sl1.sort(gt), sl1);
	t.checkExpect(l1.sort(gt),
		      new Cons<Integer>(1, new Cons<Integer>(3, new Cons<Integer>(4, mt))));
    }

    void testShortName(Tester t) {
	Runner dvh = new Runner("David", 0, 540000000);
	Runner os  = new Runner("Olin", 1, 34000);
	List<Runner> rs = new Cons<Runner>(dvh,
					   new Cons<Runner>(os,
							    new Empty<Runner>()));
	Comparison<Runner> comp = new ShortName();
	t.checkExpect(comp.greaterThan(dvh, os), true);
	t.checkExpect(rs.sort(comp),
		      new Cons<Runner>(os,
				       new Cons<Runner>(dvh,
							new Empty<Runner>())));
    }

    void testSum(Tester t) {
	t.checkExpect(sl1.foldr(new Plus(), 0), 8);
    }
}
