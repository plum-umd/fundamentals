import java.util.*;
import tester.*;

class Alg {
    Double avg(Iterable<Double> nums) {
	Double sum = 0.0;
	Double cnt = 0.0;
	for (Double n : nums) {
	    sum = sum + n;
	    cnt = cnt + 1;
	}
	if (cnt.equals(0.0)) {
	    throw new RuntimeException("No nums");
	} else {
	    return sum / cnt;
	}
    }
}

class SumSquares implements Iterable<Integer>, Iterator<Integer> {
    Iterator<Integer> xi;
    Integer sum;
    SumSquares(Iterable<Integer> x) {
	this.xi = x.iterator();
	this.sum = 0;
    }
    public Iterator<Integer> iterator() {
	return this;
    }
    public boolean hasNext() {
	return this.xi.hasNext();
    }
    public Integer next() {
	Integer n = this.xi.next();
	this.sum = this.sum + (n * n);
	return this.sum;
    }
    public void remove() {}
}

class Pair<X,Y> {
    X left;
    Y right;
    Pair(X left, Y right) {
	this.left = left;
	this.right = right;
    }
}

class Zip<X,Y> implements Iterable<Pair<X,Y>>, Iterator<Pair<X,Y>> {
    Iterator<X> xi;
    Iterator<Y> yi;
    Zip(Iterable<X> x, Iterable<Y> y) {
	this.xi = x.iterator();
	this.yi = y.iterator();
    }
    public Iterator<Pair<X,Y>> iterator() {
	return this;
    }
    public boolean hasNext() {
	return this.xi.hasNext()
	    && this.yi.hasNext();
    }
    public Pair<X,Y> next() {
	return new Pair<X,Y>(this.xi.next(), this.yi.next());
    }
    public void remove() {}
}

// A magic utility
class Iter<X> implements Iterable<X> {
    X[] vals;
    Iter(X... vals) {
	this.vals = vals;
    }
    public Iterator<X> iterator() {
	return Arrays.asList(this.vals).iterator();
    }
}

class Nat implements Iterable<Integer>, Iterator<Integer> {
    Integer n;
    Nat() { this.n = -1; }
    public Integer next() {
	this.n = this.n + 1;
	return n;
    }
    public Iterator<Integer> iterator() { return this; }
    public boolean hasNext() { return true; }
    public void remove() {}
}

class Examples {
    Alg a = new Alg();
    void testAvg(Tester t) {
	t.checkExpect(a.avg(new Iter<Double>(1.0)), 1.0);
	t.checkExpect(a.avg(new Iter<Double>(1.0, 2.0)), 1.5);
	t.checkExpect(a.avg(new Iter<Double>(1.0, 2.0, 3.0)), 2.0);
    }
    void testSumSquares(Tester t) {
	Iterator<Integer> i = new SumSquares(new Nat());
	t.checkExpect(i.hasNext(), true);
	t.checkExpect(i.next(), 0);
	t.checkExpect(i.next(), 1);
	t.checkExpect(i.next(), 5);
	t.checkExpect(i.next(), 14);
	t.checkExpect(i.next(), 30);
    }
    void testZip(Tester t) {
 	Iterator<Pair<Integer,Integer>> h =
	    new Zip<Integer,Integer>(new Iter<Integer>(),
				     new Iter<Integer>(4, 5, 6));
 	Iterator<Pair<Integer,Integer>> i =
	    new Zip<Integer,Integer>(new Iter<Integer>(1, 2, 3),
				     new Iter<Integer>(4, 5, 6));
 	Iterator<Pair<Integer,Integer>> j =
	    new Zip<Integer,Integer>(new Iter<Integer>(1, 2),
				     new Iter<Integer>(4, 5, 6));

	t.checkExpect(h.hasNext(), false);
	t.checkExpect(i.hasNext(), true);
	t.checkExpect(i.next(), new Pair<Integer,Integer>(1,4));
	t.checkExpect(i.next(), new Pair<Integer,Integer>(2,5));
	t.checkExpect(i.next(), new Pair<Integer,Integer>(3,6));
	t.checkExpect(i.hasNext(), false);
	t.checkExpect(j.hasNext(), true);
	t.checkExpect(j.next(), new Pair<Integer,Integer>(1,4));
	t.checkExpect(j.next(), new Pair<Integer,Integer>(2,5));
	t.checkExpect(j.hasNext(), false);
    }
}