import tester.*;
import java.util.*;

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
	return new WrapAL<T>(this.list, this.position + 1);
    }
}


interface List<T> extends Traversal<T>, Iterable<T> {
}

class ListIterator<T> implements Iterator<T> {
    Traversal<T> elems;
    ListIterator(Traversal<T> elems) {
	this.elems = elems;
    }

    public boolean hasNext() {
	return !this.elems.isEmpty();
    }

    public T next() {
	T first = this.elems.getFirst();
	this.elems = this.elems.getRest();
	return first;
    }

    public void remove() {
	throw new RuntimeException("We didn't implement remove.");
    }
}

class MT<T> implements List<T> {
    MT() {}

    public Iterator<T> iterator() {
	return new ListIterator<T>(this);
    }

    public boolean isEmpty() {
	return true;
    }

    public T getFirst() {
	throw new RuntimeException("getFirst of the empty list");
    }

    public Traversal<T> getRest() {
	throw new RuntimeException("getRest of the empty list");
    }
}

class Cons<T> implements List<T> {
    T first;
    List<T> rest;
    Cons(T first, List<T> rest) {
	this.first = first;
	this.rest = rest;
    }

    public Iterator<T> iterator() {
	return new ListIterator<T>(this);
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

}




interface Predicate<T> {
    // Does this predicate hold on the given T?
    Boolean apply(T t);
}

class Even implements Predicate<Integer> {
    public Boolean apply(Integer i) {
	return (i % 2) == 0;
    }
}

class Odd implements Predicate<Integer> {
    public Boolean apply(Integer i) {
	return !(new Even().apply(i));
    }
}

class TraversalMethods {

    public<T> Integer sum(Iterable<Integer> i) {
	Integer acc = 0;
	for(Integer n : i) {
	    acc = acc + n;
	}
	return acc;
    }


    public <T> Integer countSuchLoop(Traversal<T> ts, Predicate<T> p) {
	Integer count = 0;
	while (!ts.isEmpty()) {
	    if (p.apply(ts.getFirst())) {
		count = count + 1;
	    }
	    ts = ts.getRest();
	}
	return count;
    }

    // Count the number of elements of ts that satisfy p.
    public <T> Integer countSuch(Traversal<T> ts, Predicate<T> p) {
	return this.countSuchAcc(ts, p, 0);
    }

    // Count the number of elements of ts that satisfy p, given
    // that count elements have satisfied p so far.
    public <T> Integer countSuchAcc(Traversal<T> ts, Predicate<T> p, Integer count) {
	if (ts.isEmpty()) {
	    return count;
	} else {
	    if (p.apply(ts.getFirst())) {
		return this.countSuchAcc(ts.getRest(), p, count + 1);
	    } else {
		return this.countSuchAcc(ts.getRest(), p, count);
	    }
	}
    }

    public <T> Boolean anySuchLoop(Traversal<T> ts, Predicate<T> p) {
	Boolean seen = false;
	while (!ts.isEmpty()) {
	    seen = p.apply(ts.getFirst()) || seen;
	    ts = ts.getRest();
	}
	return seen;
    }

    // Is there any element that satisfies the predicate?
    public <T> Boolean anySuch(Traversal<T> ts, Predicate<T> p) {
	return this.anySuchAcc(ts, p, false);
    }

    // Is there any element that satisfies the predicate, or have we
    // already seen one?
    public <T> Boolean anySuchAcc(Traversal<T> ts, Predicate<T> p, Boolean seen) {
	if (ts.isEmpty()) {
	    return seen;
	} else {
	    return this.anySuchAcc(ts.getRest(), p, (p.apply(ts.getFirst()) || seen));
	}
    }

    public <T> List<T> reverseLoop(Traversal<T> ts) {
	List<T> list = new MT<T>();
	while (!ts.isEmpty()) {
	    list = new Cons<T>(ts.getFirst(), list);
	    ts = ts.getRest();
	}
	return list;
    }

    // Produces a list of elements from traversal in reverse order
    public <T> List<T> reverse(Traversal<T> ts) {
	return this.reverseAcc(ts, new MT<T>());
    }

    public <T> List<T> reverseAcc(Traversal<T> ts, List<T> ls) {
	if (ts.isEmpty()) {
	    return ls;
	} else {
	    return this.reverseAcc(ts.getRest(), new Cons<T>(ts.getFirst(), ls));
	}
    }

    // Sort the given elements according to the given ordering.
    public <T> ArrayList<T> sort(Traversal<T> ts, Comparator<T> comp) {
	ArrayList<T> acc = new ArrayList<T>();
	while (!ts.isEmpty()) {
	    acc = this.insert(ts.getFirst(), acc, comp);
	    ts = ts.getRest();
	}
	return acc;
    }

    // Insert the given element into the sorted array list.
    public <T> ArrayList<T> insert(T t, ArrayList<T> list, Comparator<T> comp) {
	list.add(this.findIndex(t, list, comp, 0), t);
	return list;
    }

    // Find the index where t should be inserted in the given sorted array list.
    public <T> Integer findIndex(T t, ArrayList<T> list, Comparator<T> comp, Integer acc) {
	if (acc.equals(list.size())) {
	    return acc;
	} else {
	    if (comp.compare(t, list.get(acc)) < 0) {
		return acc;
	    } else {
		return this.findIndex(t, list, comp, acc+1);
	    }
	}
    }

}

class Examples {
    List<Integer> imt = new MT<Integer>();
    List<Integer> is1 =
	new Cons<Integer>(1,
			  new Cons<Integer>(2,
					    new Cons<Integer>(3,
							      imt)));
    TraversalMethods ms = new TraversalMethods();

    ArrayList<Integer> ais = new ArrayList<Integer>();
    WrapAL<Integer> wal = new WrapAL<Integer>(ais);

    Examples() {
	ais.add(1);
	ais.add(2);
	ais.add(3);
    }

    void reset() {
	ais.clear();
	ais.add(1);
	ais.add(2);
	ais.add(3);
    }

    void testSum(Tester t) {
	this.reset();
	t.checkExpect(ms.sum(ais), 6);
	t.checkExpect(ms.sum(is1), 6);
    }

    void testIterator(Tester t) {
	ais.clear();
	ais.add(4);
	ais.add(5);
	ais.add(6);
	Iterator<Integer> i = ais.iterator();
	t.checkExpect(i.hasNext(), true);
	t.checkExpect(i.next(), 4);
	t.checkExpect(i.hasNext(), true);
	t.checkExpect(i.next(), 5);
	t.checkExpect(i.next(), 6);
	t.checkExpect(i.hasNext(), false);
    }


    void testCountSuch(Tester t) {
	this.reset();
	t.checkExpect(ms.countSuch(is1, new Even()), 1);
	t.checkExpect(ms.countSuch(is1, new Odd()), 2);
	t.checkExpect(ms.countSuch(wal, new Even()), 1);
	t.checkExpect(ms.countSuch(wal, new Odd()), 2);
	t.checkExpect(ms.countSuchLoop(is1, new Even()), 1);
	t.checkExpect(ms.countSuchLoop(is1, new Odd()), 2);
	t.checkExpect(ms.countSuchLoop(wal, new Even()), 1);
	t.checkExpect(ms.countSuchLoop(wal, new Odd()), 2);
	t.checkExpect(ms.anySuch(wal, new Odd()), true);
	t.checkExpect(ms.anySuch(wal, new Even()), true);
	t.checkExpect(ms.anySuch(imt, new Odd()), false);
	t.checkExpect(ms.anySuchLoop(wal, new Odd()), true);
	t.checkExpect(ms.anySuchLoop(wal, new Even()), true);
	t.checkExpect(ms.anySuchLoop(imt, new Odd()), false);
	t.checkExpect(ms.reverseLoop(imt), imt);
	t.checkExpect(ms.reverseLoop(is1),
		      new Cons<Integer>(3,
					new Cons<Integer>(2,
							  new Cons<Integer>(1,
									    imt))));

    }
}
