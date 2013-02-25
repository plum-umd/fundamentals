import tester.*;
import java.util.*;

interface HT<K,V> {
    // Does this hash table have given key?
    Boolean hasKey(K key);

    // Lookup given key in this hash table.
    // Throws exception if key is not in table.
    V lookup(K key);

    // Lookup given key in this hash table.
    // Produce "nothing" if key is not in table.
    V or Nothing lookupMaybe(K key);

    // Add given key, value to this hash table.
    // Overwrites any existing map.
    void add(K key, V val);

    // Clear all data from this hash table.
    void clear();
}

interface KeyVals<K,V> {
    // Does this list contain the given key?
    Boolean contains(K key);

    // Get the value associated with the given key.
    V get(K key);
}

class MT<K,V> implements KeyVals<K,V> {
    public Boolean contains(K key) {
	return false;
    }

    public V get(K key) {
	throw new RuntimeException("No such key");
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

    public Boolean contains(K key) {
	if (this.firstKey.equals(key)) {
	    return true;
	} else {
	    return this.rest.contains(key);
	}
    }

    public V get(K key) {
	if (this.firstKey.equals(key)) {
	    return this.firstVal;
	} else {
	    return this.rest.get(key);
	}
    }
}

class Hash<K,V> implements HT<K,V> {
    ArrayList<KeyVals<K,V>> list;
    Integer capacity = 500;

    Hash() {
	this.list = new ArrayList<KeyVals<K,V>>(this.capacity);
	this.clear();
    }

    // Does this hash table have given key?
    public Boolean hasKey(K key) {
	return this.list.get(key.hashCode() % this.capacity).contains(key);
    }

    // Lookup given key in this hash table.
    // Throws exception if key is not in table.
    public V lookup(K key) {
	return this.list.get(key.hashCode() % this.capacity).get(key);
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
    HT<Integer,String> ht = new Hash<Integer,String>();

    void testHash(Tester t) {
	ht.clear();

	ht.add(1, "DVH");
	ht.add(2, "VKP");
	t.checkExpect(ht.hasKey(1), true);
	t.checkExpect(ht.hasKey(2), true);
	t.checkExpect(ht.hasKey(3), false);
	t.checkExpect(ht.lookup(1), "DVH");
	t.checkExpect(ht.lookup(2), "VKP");

	ht.add(2, "MF");
	t.checkExpect(ht.hasKey(2), true);
	t.checkExpect(ht.lookup(2), "MF");
    }
}
