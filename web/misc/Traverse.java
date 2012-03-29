import tester.*;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Comparator;


// interface Iterator<T> {
//     // is it OK to call next()?
//     boolean hasNext();
//     // Get the first element in the iteration.
//     // EFFECT: update this iterator to the remaining elements.
//     T next();
// }

// interface Traversal<T> {
//     boolean isEmpty();
//     T getFirst();
//     Traversal<T> getRest();
// }

class Posn extends Object {
    Integer x;
    Integer y;
    Posn(Integer x, Integer y) {
	this.x = x;
	this.y = y;
    }

    public boolean equals(Object o) {
	Posn p = (Posn)o;
	return this.x.equals(p.x)
	    && this.y.equals(p.y);
    }

    // x.equals(y) ==> x.hashCode() == y.hashCode()
    public int hashCode() {
	return this.x + this.y;
    }
}




interface List<T> extends Traversal<T>, Iterable<T> {
}

// Represent an iterator on a list of elements.
class ListIterator<T> implements Iterator<T> {
    Traversal<T> ls;
    ListIterator(List<T> ls) {
	this.ls = ls;
    }

    public boolean hasNext() {
	return !this.ls.isEmpty();
    }

    public T next() {
	T elem = this.ls.getFirst();
	this.ls = this.ls.getRest();
	return elem;
    }

    public void remove() {
	throw new RuntimeException("Don't be removing.");
    }
}

interface Predicate<T> {
    // Does this predicate hold on the given value?
    Boolean apply(T t);
}

class Even implements Predicate<Integer> {
    public Boolean apply(Integer i) {
	return i % 2 == 0;
    }
}

class Odd implements Predicate<Integer> {
    Predicate<Integer> evenHuh = new Even();
    public Boolean apply(Integer i) {
	return !evenHuh.apply(i);
    }
}

class WrapAL<T> implements Traversal<T> {
    ArrayList<T> list;
    Integer position;
    WrapAL(ArrayList<T> list) {
	this.list = list;
	this.position = 0;
    }
    private WrapAL(ArrayList<T> list, Integer position) {
	this.list = list;
	this.position = position;
    }
    public boolean isEmpty() {
	return this.position.equals(this.list.size());
    }

    public T getFirst() {
	return this.list.get(this.position);
    }

    public Traversal<T> getRest() {
	this.position = this.position + 1;
	return this;
    }
}

class TraversalMethods {
    // Sum all the integers in the given iterator.

    // 	t.checkExpect(ms.sum(is), 6);
    Integer sum(Iterable<Integer> i) {
	Integer acc = 0;
	for(Integer j : i) {
	    acc = acc + j;
	}
	return acc;

	// Iterator<Integer> is = i.iterator();
	// while (is.hasNext()) {
	//     acc = acc + is.next();
	// }
	// return acc;
    }


    <T> Integer countSuchLoop(Traversal<T> ts, Predicate<T> p) {
	Integer count = 0;
	while (!ts.isEmpty()) {
	    if (p.apply(ts.getFirst())) {
		count = count + 1;
	    } else {
		count = count;
	    }
	    ts = ts.getRest();
	}
	return count;
    }

    // count the number of elements that satisfy the pred.
    <T> Integer countSuch(Traversal<T> ts, Predicate<T> p) {
	return this.countSuchAcc(ts, p, 0);
    }

    // Count the number of elements that satisfy the pred,
    // given that we've seen count satisfying elements so far.
    private <T> Integer countSuchAcc(Traversal<T> ts,
				     Predicate<T> p,
				     Integer count) {
	if (ts.isEmpty()) {
	    return count;
	} else {
	    if (p.apply(ts.getFirst())) {
		return this.countSuchAcc(ts.getRest(), p, count+1);
	    } else {
		return this.countSuchAcc(ts.getRest(), p, count);
	    }
	}
    }

    <T> List<T> reverseLoop(Traversal<T> ts) {
	List<T> rev = new MT<T>();
	while (!ts.isEmpty()) {
	    rev = new Cons<T>(ts.getFirst(), rev);
	    ts = ts.getRest();
	}
	return rev;
    }

    // Produce a list of elements in reverse order.
    <T> List<T> reverse(Traversal<T> ts) {
	return this.reverseAcc(ts, new MT<T>());
    }

    // Produce a list of elements in reverse order, given
    // the elements we've seen so far in reverse order.
    <T> List<T> reverseAcc(Traversal<T> ts, List<T> rev) {
	if (ts.isEmpty()) {
	    return rev;
	} else {
	    return this.reverseAcc(ts.getRest(),
				   new Cons<T>(ts.getFirst(), rev));
	}
    }

    <T> ArrayList<T> sort(Traversal<T> ts, Comparator<T> comp) {
	return sortAcc(ts, comp, new ArrayList<T>());
    }

    <T> ArrayList<T> sortAcc(Traversal<T> ts, Comparator<T> comp, ArrayList<T> ls) {
	if (ts.isEmpty()) {
	    return ls;
	} else {
	    return this.sortAcc(ts.getRest(), comp, this.insert(ts.getFirst(), comp, ls));
	}
    }

    <T> ArrayList<T> insert(T t, Comparator<T> comp, ArrayList<T> ls) {
	return this.insertAcc(t, comp, ls, 0);
    }

    <T> ArrayList<T> insertAcc(T t, Comparator<T> comp, ArrayList<T> ls, Integer i) {
	if (i.equals(ls.size())) {
	    ls.add(t);
	    return ls;
	} else {
	    if (comp.compare(t, ls.get(i)) < 0) {
		ls.add(i, t);
		return ls;
	    } else {
		return this.insertAcc(t, comp, ls, i+1);
	    }
	}
    }

}


class Cons<T> implements List<T> {
    T first;
    List<T> rest;
    Cons(T first, List<T> rest) {
	this.first = first;
	this.rest = rest;
    }
    public boolean isEmpty() {
	return false;
    }

    public T getFirst() {
	return this.first;
    }

    public Traversal<T> getRest() {
	return this.rest;
    }

    public Iterator<T> iterator() {
	return new ListIterator<T>(this);
    }
}

class MT<T> implements List<T> {
    MT() {}

    public boolean isEmpty() {
	return true;
    }
    public T getFirst() {
	throw new RuntimeException("Ain't no first.");
    }
    public Traversal<T> getRest() {
	throw new RuntimeException("null");
    }

    public Iterator<T> iterator() {
	return new ListIterator<T>(this);
    }
}

class Examples {
    List<Integer> imt = new MT<Integer>();
    List<Integer> is =
	new Cons<Integer>(1,
	    new Cons<Integer>(2,
			      new Cons<Integer>(3, imt)));

    List<Integer> ris =
	new Cons<Integer>(3,
	    new Cons<Integer>(2,
			      new Cons<Integer>(1, imt)));


    ArrayList<Integer> as = new ArrayList<Integer>();

    TraversalMethods ms = new TraversalMethods();

    void testEquals(Tester t) {
	t.checkExpect(new Posn(3, 4).equals(new Posn(3, 4)), true);
	t.checkExpect(new Posn(3, 4).hashCode(),
		      new Posn(3, 4).hashCode());
    }

    void testListIterator(Tester t) {
	Iterator<Integer> iter = is.iterator();
	t.checkExpect(iter.hasNext(), true);
	t.checkExpect(iter.next(), 1);
	t.checkExpect(iter.next(), 2);
	t.checkExpect(iter.next(), 3);
	t.checkExpect(iter.hasNext(), false);
	t.checkExpect(is, new Cons<Integer>(1,
			      new Cons<Integer>(2,
				  new Cons<Integer>(3, imt))));

	t.checkExpect(ms.sum(is), 6);
    }


    void testCountSuch(Tester t) {
	Traversal<Integer> ws1 = new WrapAL<Integer>(as);
	Traversal<Integer> ws2 = new WrapAL<Integer>(as);
	as.add(1);
	as.add(2);
	as.add(3);

	t.checkExpect(ms.countSuch(imt, new Even()), 0);
	t.checkExpect(ms.countSuch(imt, new Odd()), 0);
	t.checkExpect(ms.countSuch(is, new Even()), 1);
	t.checkExpect(ms.countSuch(is, new Odd()), 2);

	t.checkExpect(ms.countSuch(ws1, new Even()), 1);
	t.checkExpect(ms.countSuch(ws2, new Odd()), 2);

	t.checkExpect(ms.countSuchLoop(imt, new Even()), 0);
	t.checkExpect(ms.countSuchLoop(imt, new Odd()), 0);
	t.checkExpect(ms.countSuchLoop(is, new Even()), 1);
	t.checkExpect(ms.countSuchLoop(is, new Odd()), 2);

	t.checkExpect(ms.reverse(imt), imt);
	t.checkExpect(ms.reverse(is), ris);

	t.checkExpect(ms.reverseLoop(imt), imt);
	t.checkExpect(ms.reverseLoop(is), ris);
    }
}