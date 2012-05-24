/*********************************************************************/
/* Given */

import tester.*;

// Represents a function from A to B.
interface Fun<A,B> {
    // Apply this function to the given argument.
    B apply(A a);
}

// Represents a point on the Cartesian plane.
class Posn {
    Integer x;
    Integer y;
    Posn(Integer x, Integer y) {
	this.x = x;
	this.y = y;
    }
}

// Represents a computation over a list of X producing an R.
interface ListVisitor<X,R> {
    R visitEmpty();
    R visitCons(X first, List<X> rest);
}

// Represents a list of X.
interface List<X> {
    <R> R accept(ListVisitor<X,R> v);
}

// Represents an empty list of X.
class Empty<X> implements List<X> {
    Empty() {}
    public <R> R accept(ListVisitor<X,R> v) {
	return v.visitEmpty();
    }
}

// Represents a non-empty list of X.
class Cons<X> implements List<X> {
    X first;
    List<X> rest;
    Cons(X first, List<X> rest) {
	this.first = first;
	this.rest = rest;
    }
    public <R> R accept(ListVisitor<X,R> v) {
	return v.visitCons(this.first, this.rest);
    }
}

/*********************************************************************/
/* Problem 1 */

class Dist implements Fun<Posn,Double> {
    Dist() {}
    public Double apply(Posn p) {
	return Math.sqrt((p.x * p.x) + (p.y * p.y));
    }
}

/*********************************************************************/
/* Problem 2 */

// Map given function over a list and produce a list of results.
class Map<X,Y> implements ListVisitor<X,List<Y>> {
    Fun<X,Y> f;
    Map(Fun<X,Y> f) {
	this.f = f;
    }
    public List<Y> visitEmpty() {
	return new Empty<Y>();
    }
    public List<Y> visitCons(X first, List<X> rest) {
	return new Cons<Y>(this.f.apply(first), rest.accept(this));
    }
}

/*********************************************************************/
/* Problem 3 */

// Find first element of non-empty list that minimizes function.
class ArgMin<X> implements ListVisitor<X,X> {
    Fun<X,Double> f;
    ArgMin(Fun<X,Double> f) {
	this.f = f;
    }
    public X visitEmpty() {
	throw new RuntimeException("ArgMin of empty list.");
    }
    public X visitCons(X first, List<X> rest) {
	return rest.accept(new ArgMinAcc<X>(this.f, first));
    }
}

// Find first element of list or a that minimizes function.
// ACCUM: element that minimizes the function so far.
class ArgMinAcc<X> implements ListVisitor<X,X> {
    Fun<X,Double> f;
    X a;
    ArgMinAcc(Fun<X,Double> f, X a) {
	this.f = f;
	this.a = a;
    }
    public X visitEmpty() {
	return this.a;
    }
    public X visitCons(X first, List<X> rest) {
	Double r = this.f.apply(first);
	if (r < this.f.apply(a)) {
	    return rest.accept(new ArgMinAcc<X>(this.f, first));
	} else {
	    return rest.accept(this);
	}
    }
}


/*********************************************************************/
/* Tests */

class Examples {

    void testDist(Tester t) {
	t.checkExpect(new Dist().apply(new Posn(3, 4)), 5.0);
    }

    void testMap(Tester t) {
	Map<Posn,Double> mapDist = new Map<Posn,Double>(new Dist());
	List<Posn> l1 =
	    new Cons<Posn>(new Posn(3,4),
			   new Cons<Posn>(new Posn(0,0),
					  new Empty<Posn>()));
	List<Double> l2 =
	    new Cons<Double>(5.0,
			     new Cons<Double>(0.0,
					      new Empty<Double>()));

	t.checkExpect(new Empty<Posn>().accept(mapDist),
		      new Empty<Double>());
	t.checkExpect(l1.accept(mapDist), l2);
    }

    void testArgMin(Tester t) {
	Fun<Posn,Double> dist = new Dist();
	List<Posn> l1 =
	    new Cons<Posn>(new Posn(3,4),
			   new Cons<Posn>(new Posn(0,0),
					  new Empty<Posn>()));
	List<Posn> l2 =
	    new Cons<Posn>(new Posn(3,4),
			   new Cons<Posn>(new Posn(4,3),
					  new Empty<Posn>()));

	t.checkExpect(l1.accept(new ArgMin<Posn>(dist)), new Posn(0,0));
	t.checkExpect(l2.accept(new ArgMin<Posn>(dist)), new Posn(3,4));
    }
}
