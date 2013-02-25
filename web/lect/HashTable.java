import tester.*;
import java.util.*;

interface Maybe<V> {}
class Nothing<V> implements Maybe<V> {
}
class Something<V> implements Maybe<V> {
    V val;
    Something(V val) {
	this.val = val;
    }
}

interface HT<K,V> extends Iterable<K> {

    // Produce an iterator on the keys.
    Iterator<K> iterator();

    // Does this hash table have given key?
    Boolean hasKey(K key);

    // Lookup given key in this hash table.
    // Throws exception if key is not in table.
    V lookup(K key);

    // Lookup given key in this hash table.
    // Produce "nothing" if key is not in table.
    Maybe<V> lookupMaybe(K key);

    // Add given key, value to this hash table.
    // Overwrites any existing map.
    void add(K key, V val);

    // Clear all data from this hash table.
    void clear();
}

interface KeyVals<K,V> {
    // Does this list have the given key?
    Boolean hasKey(K key);

    // Lookup the value associated with the given key.
    V lookup(K key);

    Maybe<V> lookupMaybe(K key);

    // EFFECT: add this key to the list of keys.
    void addTo(ArrayList<K> keys);
}

class MT<K,V> implements KeyVals<K,V> {
    public Boolean hasKey(K key) {
	return false;
    }

    public V lookup(K key) {
	throw new RuntimeException("no such key");
    }

    public Maybe<V> lookupMaybe(K key) {
	return new Nothing<V>();
    }

    public void addTo(ArrayList<K> keys) {
	// This space is not intentionally blank
    }
}

class Cons<K,V> implements KeyVals<K,V> {
    K firstKey;
    V firstVal;
    KeyVals<K,V> rest;
    Cons(K firstKey, V firstVal, KeyVals<K,V> rest) {
	this.firstKey = firstKey;
	this.firstVal = firstVal;
	this.rest = rest;
    }

    public Boolean hasKey(K key) {
	return this.firstKey.equals(key)
	    || this.rest.hasKey(key);
    }

    public V lookup(K key) {
	if (this.firstKey.equals(key)) {
	    return this.firstVal;
	} else {
	    return this.rest.lookup(key);
	}
    }

    public Maybe<V> lookupMaybe(K key) {
	if (this.firstKey.equals(key)) {
	    return new Something<V>(this.firstVal);
	} else {
	    return this.rest.lookupMaybe(key);
	}
    }

    public void addTo(ArrayList<K> keys) {
	keys.add(this.firstKey);
	this.rest.addTo(keys);
    }
}



class HashTable<K,V> implements HT<K,V> {
    ArrayList<KeyVals<K,V>> list;
    Integer capacity = 500;
    HashTable() {
	this.list = new ArrayList<KeyVals<K,V>>(this.capacity);
	this.clear();
    }

    public Iterator<K> iterator() {
	ArrayList<K> keys = new ArrayList<K>();
	for(KeyVals<K,V> kv : this.list) {
	    kv.addTo(keys);
	}
	return keys.iterator();
    }

    // Does this hash table have given key?
    public Boolean hasKey(K key) {
	Integer i = key.hashCode() % this.capacity;
	return this.list.get(i).hasKey(key);
    }

    // Lookup given key in this hash table.
    // Throws exception if key is not in table.
    public V lookup(K key) {
	Integer i = key.hashCode() % this.capacity;
	return this.list.get(i).lookup(key);
    }

    // Lookup given key in this hash table.
    // Produce nothing if key is not in this table.
    public Maybe<V> lookupMaybe(K key) {
	Integer i = key.hashCode() % this.capacity;
	return this.list.get(i).lookupMaybe(key);
    }


    // Add given key, value to this hash table.
    // Overwrites any existing map.
    public void add(K key, V val) {
	Integer i = key.hashCode() % this.capacity;
	this.list.set(i,
		      new Cons<K,V>(key, val, this.list.get(i)));
    }

    // Clear all data from this hash table.
    public void clear() {
	this.list.clear();
	Integer i = 0;
	while (!i.equals(this.capacity)) {
	    this.list.add(new MT<K,V>());
	    i = i+1;
	}
    }
}

class Examples {
    HT<Integer,String> ht = new HashTable<Integer,String>();

    void testHash(Tester t) {
	ht.clear();

	ht.add(1, "DVH");
	ht.add(2, "STH");
	t.checkExpect(ht.hasKey(1), true);
	t.checkExpect(ht.hasKey(2), true);
	t.checkExpect(ht.hasKey(3), false);
	t.checkExpect(ht.lookup(1), "DVH");
	t.checkExpect(ht.lookup(2), "STH");

	ht.add(2, "MF");
	t.checkExpect(ht.hasKey(2), true);
	t.checkExpect(ht.lookup(2), "MF");

	t.checkExpect(ht.lookupMaybe(2),
		      new Something<String>("MF"));
	t.checkExpect(ht.lookupMaybe(3),
		      new Nothing<String>());

	ht.clear();
	ht.add(1, "DVH");
	ht.add(1, "STH");
	Iterator<Integer> i = ht.iterator();
	t.checkExpect(i.hasNext(), true);
	t.checkExpect(i.next(), 1);
	t.checkExpect(i.hasNext(), false);
    }
}
