import java.util.Iterator;

interface List<X> extends Iterable<X> {
   // Cons given element on to this list.
   List<X> cons(X x);

   // Get the first element of this list (only defined on non-empty lists).
   X first();

   // Get the rest of this list (only defined on non-empty lists).
   List<X> rest();

   // Get the ith element of this list
   // (only defined for lists of i+1 or more elements).
   X get(Integer i);

   // Compute the number of elements in this list.
   Integer size();

   // Is this list empty?
   Boolean isEmpty();

   // Accept given visitor
   <R> R accept(ListVisitor<X,R> v);
}

interface ListVisitor<X,R> {
    R visitEmpty();
    R visitCons(X first, List<X> rest);
}

class Cons<X> implements List<X> {
    X first;
    List<X> rest;
    Cons(X first, List<X> rest) {
	this.first = first;
	this.rest = rest;
    }

    public List<X> cons(X x) {
	return new Cons<X>(x, this);
    }
    public List<X> rest() {
	return this.rest;
    }
    public X first() {
	return this.first;
    }
    public Integer size() {
	return 1 + this.rest.size();
    }
    public X get(Integer i) {
	return i.equals(0) ? this.first : this.rest.get(i-1);
    }
    public Iterator<X> iterator() {
	return new ListIterator<X>(this);
    }
    public Boolean isEmpty() { return false; }
    public <R> R accept(ListVisitor<X,R> v) {
	return v.visitCons(this.first, this.rest);
    }
}

class ListIterator<X> implements Iterator<X> {
    List<X> ls;
    ListIterator(List<X> ls) {
	this.ls = ls;
    }
    public boolean hasNext() {
	return ls.isEmpty();
    }
    public X next() {
	X r = this.ls.first();
	this.ls = this.ls.rest();
	return r;
    }
    public void remove() {}
}

interface QList<X> extends List<X> {
    // Cons given element on to this list.
    QList<X> cons(X x);

    // Get the rest of this list (only defined on non-empty lists).
    QList<X> rest();
}

class QEmpty<X> implements QList<X> {
    QEmpty() {}

    public QList<X> cons(X x) {
	return new Q<X>(new Cons<FNode<X>>(new FNode<X>(x, new FLeaf<X>(), new FLeaf<X>()),
					   new QEmpty<FNode<X>>()));
    }
    public X first() {
	throw new RuntimeException("first of empty");
    }
    public QList<X> rest() {
	throw new RuntimeException("rest of empty");
    }
    public X get(Integer i) { throw new RuntimeException("Invalid index."); }
    public Integer size() { return 0; }
    public Boolean isEmpty() { return true; }
    public Iterator<X> iterator() {
	// FIXME
	return null;
    }
    public <R> R accept(ListVisitor<X,R> v) {
	return v.visitEmpty();
    }
}

class Q<X> implements QList<X> {
    Cons<FNode<X>> forest;
    Q(Cons<FNode<X>> forest) {
	this.forest = forest;
    }
    public QList<X> cons(X x) {
	if (!this.forest.rest.isEmpty() &&
	    this.forest.first.size().equals(this.forest.rest.first().size())) {
	    return new Q<X>(new Cons<FNode<X>>(new FNode<X>(x,
							    this.forest.first,
							    this.forest.rest.first()),
					       this.forest.rest.rest()));
	} else {
	    return new Q<X>(new Cons<FNode<X>>(new FNode<X>(x, new FLeaf<X>(), new FLeaf<X>()),
					       this.forest));
	}
    }
    public X first() {
	return this.forest.first.val;
    }
    public QList<X> rest() {
	if (this.forest.first.left.isLeaf()) {
	    return this.forest.rest.accept(new ListVisitor<FNode<X>,QList<X>> () {
		    public QList<X> visitEmpty() {
			return new QEmpty<X>();
		    }
		    public QList<X> visitCons(FNode<X> first, List<FNode<X>> rest) {
			return new Q<X>(new Cons<FNode<X>>(first, rest));
		    }
		});
	} else {
	    return new Q<X>(new Cons<FNode<X>>((FNode)this.forest.first.left,
					       new Cons<FNode<X>>((FNode)this.forest.first.right,
								  this.forest.rest)));
	}
    }
    public X get(Integer i) {
	Integer j = i;
	for (FNode<X> t : forest) {
	    Integer s = t.size();
	    if (j < s) {
		return t.get(j);
	    } else {
		j = j - s;
	    }
	}
	throw new RuntimeException("Invalid index.");
    }

    public Integer size() {
	return this.forest.size();
    }

    public Boolean isEmpty() {
	return false;
    }

    public Iterator<X> iterator() {
	// FIXME
	return null;
    }
    public <R> R accept(ListVisitor<X,R> v) {
	// FIXME
	return null;
    }
}

interface FBT<X> {
    Integer size();
    Integer height();

    X get(Integer i);

    Integer expt(Integer n, Integer m);
    Boolean isLeaf();
}

abstract class AFBT<X> implements FBT<X> {
    public Integer expt(Integer n, Integer m) {
	return m.equals(0) ? 1 : n * this.expt(n, m-1);
    }
    public Integer size() {
	return this.expt(2, this.height()) - 1;
    }
}

class FNode<X> extends AFBT<X> {
    X val;
    FBT<X> left;
    FBT<X> right;
    FNode(X val, FBT<X> left, FBT<X> right) {
	this.val = val;
	this.left = left;
	this.right = right;
    }

    public Integer height() {
	return 1 + this.left.height();
    }
    public X get(Integer i) {
	Integer s = this.size();
	return i.equals(0)
	    ? this.val
	    : i < (s + 1) / 2
	    ? this.left.get(i)
	    : this.right.get(i - ((s + 1) / 2));
    }
    public Boolean isLeaf() { return false; }
}

class FLeaf<X> extends AFBT<X> {
    FLeaf() {}
    public Integer height() { return 0; }
    public X get(Integer i) {
	throw new RuntimeException("Illegal index");
    }
    public Boolean isLeaf() { return true; }
}
