import java.util.*;
import tester.*;

interface ListVisitor<X,R> {
    R visitEmpty();
    R visitCons(X first, List<X> rest);
}

interface List<X> extends Iterable<X> {
    // add more list stuff...
    Boolean isEmpty();
    X getFirst();
    List<X> getRest();
    <R> R accept(ListVisitor<X,R> v);
}

abstract class AList<X> implements List<X> {
    public Iterator<X> iterator() {
	return new ListIterator<X>(this);
    }
}

class Empty<X> extends AList<X> {
    Empty() {}
    public Boolean isEmpty() { return true; }
    public X getFirst() { throw new RuntimeException("Empty list"); }
    public List<X> getRest() { throw new RuntimeException("Empty list"); }
    public <R> R accept(ListVisitor<X,R> v) { return v.visitEmpty(); }
}

class Cons<X> extends AList<X> {
    X first;
    List<X> rest;
    Cons(X first, List<X> rest) {
	this.first = first;
	this.rest = rest;
    }
    public Boolean isEmpty() { return false; }
    public X getFirst() { return this.first; }
    public List<X> getRest() { return this.rest; }

    public <R> R accept(ListVisitor<X,R> v) {
	return v.visitCons(this.first, this.rest);
    }
}

// Represents an iteration through a list.
class ListIterator<X> implements Iterator<X> {
    List<X> list;
    ListIterator(List<X> list) {
	this.list = list;
    }

    public boolean hasNext() {
	return !(this.list.isEmpty());
    }

    public X next() {
	X res = this.list.getFirst();
	this.list = this.list.getRest();
	return res;
    }

    public void remove() {}
}

class Pair<A,B> {
    A left;
    B right;
    Pair(A left, B right) {
	this.left = left;
	this.right = right;
    }
}

class HashTable<K,V> {
    ArrayList<List<Pair<K,V>>> table;
    Integer SIZE = 30;
    HashTable() {
	// for (INITIAL; CONTINUATION; UPDATE) { BODY }
	table = new ArrayList<List<Pair<K,V>>>();
	for (Integer i = 0; i < this.SIZE; i = i+1) {
	    table.add(new Empty<Pair<K,V>>());
	}
    }

    // Lookup the value associated with given key.
    // Assume key already has a value in this table
    V lookup(K key) {
	Integer i = Math.abs(key.hashCode() % this.SIZE);
	for (Pair<K,V> p : this.table.get(i)) {
	    if (p.left.equals(key)) {
		return p.right;
	    }
	}
	throw new RuntimeException("Key not in list.");
	// Using a visitor:
	// return this.table.get(i).accept(new Lookup<K,V>(key));
    }

    // Assume key already has a value in this table
    V set(K key, V val) {
	V old = this.lookup(key);
	this.add(key, val);
	return old;
    }

    // Add given key,val pair to this hash table.
    void add(K key, V val) {
	Integer i = Math.abs(key.hashCode() % this.SIZE);
	List<Pair<K,V>> ls = this.table.get(i);
	this.table.set(i, new Cons<Pair<K,V>>(new Pair<K,V>(key, val), ls));
    }
}

/*
// Lookup value associated with given key in list of pairs.
class Lookup<K,V> implements ListVisitor<Pair<K,V>,V> {
    K key;
    Lookup(K key) {
	this.key = key;
    }
    public V visitEmpty() { throw new RuntimeException("Key not in list."); }
    public V visitCons(Pair<K,V> first, List<Pair<K,V>> rest) {
	return first.left.equals(key)
	    ? first.right
	    : rest.accept(this);
    }
}
*/





class Alg {
    Integer sum(Iterable<Integer> is) {
	Integer acc = 0;
	for (Integer i : is) {
	    acc = acc + i;
	}
	return acc;
    }
}


class Examples {
    Alg a = new Alg();
    List<Integer> is = new Cons<Integer>(3,
					 new Cons<Integer>(4,
							   new Empty<Integer>()));
    void testIterator(Tester t) {
	t.checkExpect(a.sum(is), 7);
    }

    void testLookup(Tester t) {
	HashTable<String, String> tbl = new HashTable<String, String>();
	tbl.add("Smiley", "Bart");
	tbl.add("Squarey", "Fred");
	t.checkExpect(tbl.lookup("Smiley"), "Bart");
	t.checkExpect(tbl.lookup("Squarey"), "Fred");
    }

}
